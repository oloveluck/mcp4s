package mcp4s.server.auth

import cats.effect.IO
import cats.effect.Ref
import io.circe.Json
import mcp4s.protocol.{ProtectedResourceMetadata, TokenInfo}
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.headers.{Authorization, `WWW-Authenticate`}
import org.http4s.implicits.*
import org.typelevel.vault.Key
import AuthMiddleware.tokenInfo

class AuthMiddlewareSpec extends CatsEffectSuite:

  // === Test Fixtures ===

  val testMetadata: ProtectedResourceMetadata = ProtectedResourceMetadata(
    resource = "http://localhost:3000",
    authorizationServers = List("https://auth.example.com"),
    scopesSupported = Some(List("mcp:read", "mcp:write"))
  )

  def testConfig(validator: TokenValidator[IO], requiredScopes: Set[String] = Set.empty): AuthConfig[IO] =
    AuthConfig(testMetadata, validator, requiredScopes)

  def testRoutes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "test" => Ok("success")
    case POST -> Root / "mcp" => Ok("mcp response")
  }

  // === Metadata Endpoint Tests ===

  test("metadata endpoint returns protected resource metadata") {
    val config = testConfig(TokenValidator.allowAll[IO])
    val routes = AuthMiddleware.metadataRoutes[IO](config)

    val request = Request[IO](Method.GET, uri"/.well-known/oauth-protected-resource")
    routes.orNotFound.run(request).flatMap { response =>
      assertEquals(response.status, Status.Ok)
      response.as[Json].map { json =>
        assertEquals(json.hcursor.get[String]("resource"), Right("http://localhost:3000"))
        assertEquals(json.hcursor.get[List[String]]("authorization_servers"), Right(List("https://auth.example.com")))
        assertEquals(json.hcursor.get[List[String]]("scopes_supported"), Right(List("mcp:read", "mcp:write")))
      }
    }
  }

  // === No Auth Tests ===

  test("request without Authorization header returns 401") {
    for
      key <- Key.newKey[IO, TokenInfo]
      given Key[TokenInfo] = key
      config = testConfig(TokenValidator.allowAll[IO])
      protectedRoutes = AuthMiddleware[IO](config, testRoutes)
      request = Request[IO](Method.GET, uri"/test")
      response <- protectedRoutes.orNotFound.run(request)
    yield
      assertEquals(response.status, Status.Unauthorized)
      val wwwAuth = response.headers.get[`WWW-Authenticate`]
      assert(wwwAuth.isDefined, "Expected WWW-Authenticate header")
  }

  // === allowAll Validator Tests ===

  test("allowAll validator accepts any non-empty token") {
    for
      key <- Key.newKey[IO, TokenInfo]
      given Key[TokenInfo] = key
      config = testConfig(TokenValidator.allowAll[IO])
      protectedRoutes = AuthMiddleware[IO](config, testRoutes)
      request = Request[IO](Method.GET, uri"/test")
        .withHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, "any-token")))
      response <- protectedRoutes.orNotFound.run(request)
    yield
      assertEquals(response.status, Status.Ok)
  }

  test("allowAll validator rejects empty token") {
    for
      key <- Key.newKey[IO, TokenInfo]
      given Key[TokenInfo] = key
      config = testConfig(TokenValidator.allowAll[IO])
      protectedRoutes = AuthMiddleware[IO](config, testRoutes)
      request = Request[IO](Method.GET, uri"/test")
        .withHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, "")))
      response <- protectedRoutes.orNotFound.run(request)
    yield
      assertEquals(response.status, Status.Unauthorized)
  }

  // === denyAll Validator Tests ===

  test("denyAll validator rejects all tokens") {
    for
      key <- Key.newKey[IO, TokenInfo]
      given Key[TokenInfo] = key
      config = testConfig(TokenValidator.denyAll[IO]("http://localhost:3000/.well-known/oauth-protected-resource"))
      protectedRoutes = AuthMiddleware[IO](config, testRoutes)
      request = Request[IO](Method.GET, uri"/test")
        .withHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, "valid-token")))
      response <- protectedRoutes.orNotFound.run(request)
    yield
      assertEquals(response.status, Status.Unauthorized)
  }

  // === apiKey Validator Tests ===

  test("apiKey validator accepts valid key") {
    for
      key <- Key.newKey[IO, TokenInfo]
      given Key[TokenInfo] = key
      config = testConfig(TokenValidator.apiKey[IO](Set("secret-key-123")))
      protectedRoutes = AuthMiddleware[IO](config, testRoutes)
      request = Request[IO](Method.GET, uri"/test")
        .withHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, "secret-key-123")))
      response <- protectedRoutes.orNotFound.run(request)
    yield
      assertEquals(response.status, Status.Ok)
  }

  test("apiKey validator rejects invalid key") {
    for
      key <- Key.newKey[IO, TokenInfo]
      given Key[TokenInfo] = key
      config = testConfig(TokenValidator.apiKey[IO](Set("secret-key-123")))
      protectedRoutes = AuthMiddleware[IO](config, testRoutes)
      request = Request[IO](Method.GET, uri"/test")
        .withHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, "wrong-key")))
      response <- protectedRoutes.orNotFound.run(request)
    yield
      assertEquals(response.status, Status.Unauthorized)
  }

  // === Scope Tests ===

  test("request with sufficient scopes succeeds") {
    for
      key <- Key.newKey[IO, TokenInfo]
      given Key[TokenInfo] = key
      config = testConfig(
        TokenValidator.apiKeyWithScopes[IO](Map("key" -> Set("mcp:read"))),
        requiredScopes = Set("mcp:read")
      )
      protectedRoutes = AuthMiddleware[IO](config, testRoutes)
      request = Request[IO](Method.GET, uri"/test")
        .withHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, "key")))
      response <- protectedRoutes.orNotFound.run(request)
    yield
      assertEquals(response.status, Status.Ok)
  }

  test("request with insufficient scopes returns 403") {
    for
      key <- Key.newKey[IO, TokenInfo]
      given Key[TokenInfo] = key
      config = testConfig(
        TokenValidator.apiKeyWithScopes[IO](Map("key" -> Set("mcp:read"))),
        requiredScopes = Set("mcp:write")
      )
      protectedRoutes = AuthMiddleware[IO](config, testRoutes)
      request = Request[IO](Method.GET, uri"/test")
        .withHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, "key")))
      response <- protectedRoutes.orNotFound.run(request)
    yield
      assertEquals(response.status, Status.Forbidden)
      val wwwAuth = response.headers.get[`WWW-Authenticate`]
      assert(wwwAuth.isDefined, "Expected WWW-Authenticate header with insufficient_scope")
  }

  // === TokenInfo Attachment Tests ===

  test("authenticated request has TokenInfo attached") {
    for
      key <- Key.newKey[IO, TokenInfo]
      given Key[TokenInfo] = key
      foundTokenInfoRef <- Ref.of[IO, Option[TokenInfo]](None)
      config = testConfig(TokenValidator.allowAll[IO])
      routesWithCheck = HttpRoutes.of[IO] {
        case req @ GET -> Root / "test" =>
          foundTokenInfoRef.set(req.tokenInfo) *> Ok("success")
      }
      protectedRoutes = AuthMiddleware[IO](config, routesWithCheck)
      request = Request[IO](Method.GET, uri"/test")
        .withHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, "test-user")))
      response <- protectedRoutes.orNotFound.run(request)
      foundTokenInfo <- foundTokenInfoRef.get
    yield
      assertEquals(response.status, Status.Ok)
      assert(foundTokenInfo.isDefined, "TokenInfo should be attached to request")
      assertEquals(foundTokenInfo.get.subject, "test-user")
  }

  // === Non-Bearer Auth Tests ===

  test("non-Bearer auth scheme returns 401") {
    for
      key <- Key.newKey[IO, TokenInfo]
      given Key[TokenInfo] = key
      config = testConfig(TokenValidator.allowAll[IO])
      protectedRoutes = AuthMiddleware[IO](config, testRoutes)
      request = Request[IO](Method.GET, uri"/test")
        .withHeaders(Headers(Header.Raw(org.typelevel.ci.CIString("Authorization"), "Basic dXNlcjpwYXNz")))
      response <- protectedRoutes.orNotFound.run(request)
    yield
      assertEquals(response.status, Status.Unauthorized)
  }
