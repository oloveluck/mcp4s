package mcp4s.server.auth

import cats.Applicative
import cats.syntax.all.*
import mcp4s.protocol.{AuthError, TokenInfo}

/** Abstract token validation trait.
  *
  * Implementations validate bearer tokens and return token info
  * or an appropriate auth error.
  */
trait TokenValidator[F[_]]:
  /** Validate a bearer token and extract token info.
    *
    * @param token The bearer token from Authorization header
    * @return Either an auth error or the validated token info
    */
  def validate(token: String): F[Either[AuthError, TokenInfo]]

object TokenValidator:

  /** Development validator that accepts any non-empty token.
    *
    * For development and testing only - accepts any token
    * and creates a TokenInfo with the token as the subject.
    */
  def allowAll[F[_]: Applicative]: TokenValidator[F] = new TokenValidator[F]:
    def validate(token: String): F[Either[AuthError, TokenInfo]] =
      if token.isEmpty then
        AuthError.InvalidToken("Empty token").asLeft[TokenInfo].pure[F]
      else
        TokenInfo(subject = token).asRight[AuthError].pure[F]

  /** Test validator that rejects all tokens.
    *
    * For testing auth error handling - rejects all tokens
    * with a MissingCredentials error pointing to the metadata URI.
    *
    * @param resourceMetadataUri URI to the protected resource metadata endpoint
    */
  def denyAll[F[_]: Applicative](resourceMetadataUri: String): TokenValidator[F] = new TokenValidator[F]:
    def validate(token: String): F[Either[AuthError, TokenInfo]] =
      AuthError.InvalidToken("Token validation disabled").asLeft[TokenInfo].pure[F]

  /** Simple API key validation.
    *
    * Validates tokens against a set of valid API keys.
    * The token itself becomes the subject in TokenInfo.
    *
    * @param validKeys Set of valid API keys
    */
  def apiKey[F[_]: Applicative](validKeys: Set[String]): TokenValidator[F] = new TokenValidator[F]:
    def validate(token: String): F[Either[AuthError, TokenInfo]] =
      if validKeys.contains(token) then
        TokenInfo(subject = token).asRight[AuthError].pure[F]
      else
        AuthError.InvalidToken("Invalid API key").asLeft[TokenInfo].pure[F]

  /** API key validation with custom scopes.
    *
    * Validates tokens against a map of API keys to their granted scopes.
    *
    * @param keyScopes Map of API key to granted scopes
    */
  def apiKeyWithScopes[F[_]: Applicative](keyScopes: Map[String, Set[String]]): TokenValidator[F] = new TokenValidator[F]:
    def validate(token: String): F[Either[AuthError, TokenInfo]] =
      keyScopes.get(token) match
        case Some(scopes) =>
          TokenInfo(subject = token, scopes = scopes).asRight[AuthError].pure[F]
        case None =>
          AuthError.InvalidToken("Invalid API key").asLeft[TokenInfo].pure[F]

  /** JWT validation that accepts any well-formed JWT (dev mode).
    *
    * Parses the JWT payload and extracts standard claims into TokenInfo.
    * Does NOT verify the signature - use only for development/testing.
    *
    * Standard claims extracted:
    * - sub → subject
    * - aud → audience
    * - iss → issuer
    * - exp → expiration
    * - scope (space-separated) or scopes (array) → scopes
    */
  def jwt[F[_]: Applicative]: TokenValidator[F] = new TokenValidator[F]:
    def validate(token: String): F[Either[AuthError, TokenInfo]] =
      parseJwt(token).pure[F]

  private def parseJwt(token: String): Either[AuthError, TokenInfo] =
    token.split('.').toList match
      case _ :: payload :: _ =>
        for
          decoded <- decodeBase64(payload)
          json <- io.circe.parser.parse(decoded).leftMap(_ => AuthError.InvalidToken("Invalid JSON in JWT payload"))
          tokenInfo <- extractClaims(json)
        yield tokenInfo
      case _ =>
        Left(AuthError.InvalidToken("Invalid JWT format"))

  private def decodeBase64(s: String): Either[AuthError, String] =
    try
      // Handle URL-safe base64
      val padded = s + "=" * ((4 - s.length % 4) % 4)
      val decoded = java.util.Base64.getUrlDecoder.decode(padded)
      Right(new String(decoded, "UTF-8"))
    catch
      case _: Exception => Left(AuthError.InvalidToken("Invalid base64 encoding"))

  private def extractClaims(json: io.circe.Json): Either[AuthError, TokenInfo] =
    val cursor = json.hcursor
    val subject = cursor.get[String]("sub").getOrElse("unknown")
    val audience = cursor.get[String]("aud").toOption
    val issuer = cursor.get[String]("iss").toOption
    val expiration = cursor.get[Long]("exp").toOption

    // Scopes can be space-separated string or array
    val scopes = cursor.get[String]("scope").map(_.split(' ').toSet).toOption
      .orElse(cursor.get[List[String]]("scopes").map(_.toSet).toOption)
      .getOrElse(Set.empty)

    Right(TokenInfo(
      subject = subject,
      audience = audience,
      scopes = scopes,
      issuer = issuer,
      expiration = expiration,
      claims = json.asObject.map(_.toMap).getOrElse(Map.empty)
    ))
