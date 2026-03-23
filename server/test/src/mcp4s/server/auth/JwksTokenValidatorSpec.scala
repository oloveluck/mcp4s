package mcp4s.server.auth

import cats.effect.IO
import io.circe.Json
import io.circe.syntax.*
import mcp4s.protocol.{AuthError, TokenInfo}
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.dsl.io.*

import java.security.{KeyPairGenerator, Signature}
import java.security.interfaces.RSAPublicKey

class JwksTokenValidatorSpec extends CatsEffectSuite:

  // === RSA Key Generation for Tests ===

  private val keyPair =
    val gen = KeyPairGenerator.getInstance("RSA")
    gen.initialize(2048)
    gen.generateKeyPair()

  private val publicKey = keyPair.getPublic.asInstanceOf[RSAPublicKey]

  /** Base64url-encode bytes without padding */
  private def b64url(bytes: Array[Byte]): String =
    java.util.Base64.getUrlEncoder.withoutPadding().encodeToString(bytes)

  /** Base64url-encode a string without padding */
  private def b64url(s: String): String = b64url(s.getBytes("UTF-8"))

  /** Sign data with RSA SHA-256 */
  private def rsaSign(data: Array[Byte]): Array[Byte] =
    val sig = Signature.getInstance("SHA256withRSA")
    sig.initSign(keyPair.getPrivate)
    sig.update(data)
    sig.sign()

  /** Create a properly signed RS256 JWT */
  private def makeSignedJwt(payload: String, kid: String = "test-key-1"): String =
    val header = s"""{"alg":"RS256","typ":"JWT","kid":"$kid"}"""
    val headerB64 = b64url(header)
    val payloadB64 = b64url(payload)
    val signingInput = s"$headerB64.$payloadB64"
    val signature = rsaSign(signingInput.getBytes("UTF-8"))
    s"$headerB64.$payloadB64.${b64url(signature)}"

  /** Build JWKS JSON from the test RSA public key */
  private def jwksJson: Json =
    val n = b64url(publicKey.getModulus.toByteArray)
    val e = b64url(publicKey.getPublicExponent.toByteArray)
    Json.obj(
      "keys" -> Json.arr(
        Json.obj(
          "kty" -> Json.fromString("RSA"),
          "kid" -> Json.fromString("test-key-1"),
          "use" -> Json.fromString("sig"),
          "alg" -> Json.fromString("RS256"),
          "n" -> Json.fromString(n),
          "e" -> Json.fromString(e)
        )
      )
    )

  /** Create a mock HTTP client that returns the test JWKS */
  private def mockJwksClient: Client[IO] =
    Client.fromHttpApp[IO](HttpRoutes.of[IO] {
      case GET -> Root / ".well-known" / "jwks.json" =>
        Ok(jwksJson)
    }.orNotFound)

  // === Tests ===

  test("verifies valid RS256 JWT signature") {
    val payload = """{"sub":"user123","iss":"https://auth.example.com","exp":4102444800}"""
    val token = makeSignedJwt(payload)

    for
      validator <- JwksTokenValidator.create[IO](
        jwksUri = "http://localhost/.well-known/jwks.json",
        httpClient = mockJwksClient
      )
      result <- validator.validate(token)
    yield
      assert(result.isRight, s"Expected Right, got $result")
      assertEquals(result.toOption.get.subject, "user123")
  }

  test("rejects JWT with tampered payload") {
    val payload = """{"sub":"user123","iss":"https://auth.example.com","exp":4102444800}"""
    val token = makeSignedJwt(payload)
    // Tamper with the payload portion
    val parts = token.split('.')
    val tamperedPayload = b64url("""{"sub":"hacker","iss":"https://auth.example.com","exp":4102444800}""")
    val tamperedToken = s"${parts(0)}.$tamperedPayload.${parts(2)}"

    for
      validator <- JwksTokenValidator.create[IO](
        jwksUri = "http://localhost/.well-known/jwks.json",
        httpClient = mockJwksClient
      )
      result <- validator.validate(tamperedToken)
    yield
      assert(result.isLeft, "Should reject tampered JWT")
  }

  test("validates issuer claim") {
    val payload = """{"sub":"user123","iss":"https://wrong-issuer.com","exp":4102444800}"""
    val token = makeSignedJwt(payload)

    for
      validator <- JwksTokenValidator.create[IO](
        jwksUri = "http://localhost/.well-known/jwks.json",
        httpClient = mockJwksClient,
        issuer = Some("https://auth.example.com")
      )
      result <- validator.validate(token)
    yield
      assert(result.isLeft, "Should reject wrong issuer")
      result match
        case Left(AuthError.InvalidToken(msg)) => assert(msg.contains("issuer"))
        case other => fail(s"Expected InvalidToken, got $other")
  }

  test("validates audience claim") {
    val payload = """{"sub":"user123","aud":"wrong-audience","exp":4102444800}"""
    val token = makeSignedJwt(payload)

    for
      validator <- JwksTokenValidator.create[IO](
        jwksUri = "http://localhost/.well-known/jwks.json",
        httpClient = mockJwksClient,
        audience = Some("https://api.example.com")
      )
      result <- validator.validate(token)
    yield
      assert(result.isLeft, "Should reject wrong audience")
      result match
        case Left(_: AuthError.InvalidAudience) => () // expected
        case other => fail(s"Expected InvalidAudience, got $other")
  }

  test("accepts matching issuer and audience") {
    val payload = """{"sub":"user123","iss":"https://auth.example.com","aud":"https://api.example.com","exp":4102444800}"""
    val token = makeSignedJwt(payload)

    for
      validator <- JwksTokenValidator.create[IO](
        jwksUri = "http://localhost/.well-known/jwks.json",
        httpClient = mockJwksClient,
        issuer = Some("https://auth.example.com"),
        audience = Some("https://api.example.com")
      )
      result <- validator.validate(token)
    yield
      assert(result.isRight, s"Expected Right, got $result")
      val info = result.toOption.get
      assertEquals(info.issuer, Some("https://auth.example.com"))
      assertEquals(info.audience, Some("https://api.example.com"))
  }

  test("extracts scopes from signed JWT") {
    val payload = """{"sub":"user123","scope":"mcp:read mcp:write","exp":4102444800}"""
    val token = makeSignedJwt(payload)

    for
      validator <- JwksTokenValidator.create[IO](
        jwksUri = "http://localhost/.well-known/jwks.json",
        httpClient = mockJwksClient
      )
      result <- validator.validate(token)
    yield
      assert(result.isRight)
      assertEquals(result.toOption.get.scopes, Set("mcp:read", "mcp:write"))
  }

  test("rejects JWT with unknown key ID") {
    val payload = """{"sub":"user123","exp":4102444800}"""
    val token = makeSignedJwt(payload, kid = "unknown-key")

    for
      validator <- JwksTokenValidator.create[IO](
        jwksUri = "http://localhost/.well-known/jwks.json",
        httpClient = mockJwksClient
      )
      result <- validator.validate(token)
    yield
      assert(result.isLeft, "Should reject unknown key ID")
      result match
        case Left(AuthError.InvalidToken(msg)) => assert(msg.contains("not found"))
        case other => fail(s"Expected InvalidToken, got $other")
  }

  test("rejects malformed JWT") {
    for
      validator <- JwksTokenValidator.create[IO](
        jwksUri = "http://localhost/.well-known/jwks.json",
        httpClient = mockJwksClient
      )
      result <- validator.validate("not-a-jwt")
    yield
      assert(result.isLeft)
  }

  test("caches JWKS keys between calls") {
    val payload = """{"sub":"user1","exp":4102444800}"""
    val token1 = makeSignedJwt(payload)
    val token2 = makeSignedJwt("""{"sub":"user2","exp":4102444800}""")

    var fetchCount = 0
    val countingClient = Client.fromHttpApp[IO](HttpRoutes.of[IO] {
      case GET -> Root / ".well-known" / "jwks.json" =>
        fetchCount += 1
        Ok(jwksJson)
    }.orNotFound)

    for
      validator <- JwksTokenValidator.create[IO](
        jwksUri = "http://localhost/.well-known/jwks.json",
        httpClient = countingClient
      )
      r1 <- validator.validate(token1)
      r2 <- validator.validate(token2)
    yield
      assert(r1.isRight)
      assert(r2.isRight)
      assertEquals(fetchCount, 1, "JWKS should only be fetched once due to caching")
  }
