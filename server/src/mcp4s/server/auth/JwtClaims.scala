package mcp4s.server.auth

import io.circe.*
import mcp4s.protocol.{AuthError, TokenInfo}

/** Shared JWT parsing utilities used by both the dev JWT validator and JwksTokenValidator. */
private[auth] object JwtClaims:

  /** Split a JWT into its three base64url-encoded parts. */
  def splitJwt(token: String): Either[AuthError, (String, String, String)] =
    token.split('.').toList match
      case header :: payload :: signature :: Nil => Right((header, payload, signature))
      case _ => Left(AuthError.InvalidToken("Invalid JWT format: expected 3 parts"))

  /** Decode a base64url-encoded string to JSON. */
  def decodeJson(base64: String): Either[AuthError, Json] =
    try
      val decoded = decodeBase64Bytes(base64)
      val str = new String(decoded, "UTF-8")
      io.circe.parser.parse(str).leftMap(_ => AuthError.InvalidToken("Invalid JSON in JWT"))
    catch
      case _: Exception => Left(AuthError.InvalidToken("Invalid base64 encoding in JWT"))

  /** Decode a base64url string to raw bytes. */
  def decodeBase64Bytes(s: String): Array[Byte] =
    val padded = s + "=" * ((4 - s.length % 4) % 4)
    java.util.Base64.getUrlDecoder.decode(padded)

  /** Extract standard JWT claims into TokenInfo, optionally validating issuer and audience. */
  def extractAndValidate(
      payload: Json,
      expectedIssuer: Option[String] = None,
      expectedAudience: Option[String] = None
  ): Either[AuthError, TokenInfo] =
    val cursor = payload.hcursor
    val subject = cursor.get[String]("sub").getOrElse("unknown")
    val audience = cursor.get[String]("aud").toOption
      .orElse(cursor.get[List[String]]("aud").toOption.flatMap(_.headOption))
    val issuer = cursor.get[String]("iss").toOption
    val expiration = cursor.get[Long]("exp").toOption

    // Scopes: space-separated string or array
    val scopes = cursor.get[String]("scope").map(_.split(' ').toSet).toOption
      .orElse(cursor.get[List[String]]("scopes").map(_.toSet).toOption)
      .getOrElse(Set.empty)

    // Validate issuer
    val issuerValid = expectedIssuer match
      case Some(expected) => issuer.contains(expected)
      case None => true

    // Validate audience
    val audienceValid = expectedAudience match
      case Some(expected) => audience.contains(expected)
      case None => true

    if !issuerValid then
      Left(AuthError.InvalidToken(s"Invalid issuer: expected ${expectedIssuer.getOrElse("")}, got ${issuer.getOrElse("none")}"))
    else if !audienceValid then
      Left(AuthError.InvalidAudience(expectedAudience.getOrElse(""), audience))
    else
      Right(TokenInfo(
        subject = subject,
        audience = audience,
        scopes = scopes,
        issuer = issuer,
        expiration = expiration,
        claims = payload.asObject.map(_.toMap).getOrElse(Map.empty)
      ))
