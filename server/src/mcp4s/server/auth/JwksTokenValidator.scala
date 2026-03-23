package mcp4s.server.auth

import cats.effect.{Async, Ref, Clock}
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.{AuthError, TokenInfo}
import org.http4s.Uri
import org.http4s.client.Client

import java.math.BigInteger
import java.security.{KeyFactory, Signature}
import java.security.spec.{RSAPublicKeySpec, ECPoint, ECPublicKeySpec}
import java.security.interfaces.{RSAPublicKey, ECPublicKey}

import scala.concurrent.duration.*

/** JWT token validator that verifies signatures against a JWKS (JSON Web Key Set) endpoint.
  *
  * Fetches public keys from the auth server's JWKS URI, caches them, and uses them
  * to verify JWT signatures. Supports RS256 and ES256 algorithms.
  *
  * Example usage:
  * {{{
  * JwksTokenValidator.create[IO](
  *   jwksUri = "https://auth.example.com/.well-known/jwks.json",
  *   httpClient = client,
  *   issuer = Some("https://auth.example.com"),
  *   audience = Some("https://api.example.com")
  * )
  * }}}
  */
object JwksTokenValidator:

  /** Configuration for JWKS token validation */
  final case class Config(
      /** JWKS endpoint URI */
      jwksUri: String,
      /** Expected issuer (`iss` claim). If set, tokens with a different issuer are rejected. */
      issuer: Option[String] = None,
      /** Expected audience (`aud` claim). If set, tokens without this audience are rejected. */
      audience: Option[String] = None,
      /** How long to cache JWKS keys before refetching */
      cacheTtl: FiniteDuration = 5.minutes,
      /** Clock skew tolerance for expiration checks */
      clockSkew: FiniteDuration = 30.seconds
  )

  /** Create a JWKS-verifying token validator.
    *
    * @param jwksUri JWKS endpoint URI (e.g. "https://auth.example.com/.well-known/jwks.json")
    * @param httpClient http4s Client for fetching JWKS
    * @param issuer Expected issuer claim (optional)
    * @param audience Expected audience claim (optional)
    * @param cacheTtl How long to cache JWKS keys (default 5 minutes)
    */
  def create[F[_]: Async](
      jwksUri: String,
      httpClient: Client[F],
      issuer: Option[String] = None,
      audience: Option[String] = None,
      cacheTtl: FiniteDuration = 5.minutes
  ): F[TokenValidator[F]] =
    create[F](Config(jwksUri, issuer, audience, cacheTtl), httpClient)

  /** Create a JWKS-verifying token validator with full config. */
  def create[F[_]: Async](
      config: Config,
      httpClient: Client[F]
  ): F[TokenValidator[F]] =
    for
      cacheRef <- Ref.of[F, Option[CachedKeys]](None)
    yield new JwksValidator[F](config, httpClient, cacheRef)

  // === Internal Types ===

  private case class CachedKeys(keys: Map[String, JwkKey], fetchedAt: Long)

  private case class JwkKey(
      kty: String,          // Key type: RSA or EC
      kid: Option[String],  // Key ID
      alg: Option[String],  // Algorithm
      use: Option[String],  // Key use: sig or enc
      n: Option[String],    // RSA modulus
      e: Option[String],    // RSA exponent
      crv: Option[String],  // EC curve
      x: Option[String],    // EC x coordinate
      y: Option[String]     // EC y coordinate
  )

  private object JwkKey:
    given Decoder[JwkKey] = Decoder.instance { c =>
      for
        kty <- c.get[String]("kty")
        kid <- c.get[Option[String]]("kid")
        alg <- c.get[Option[String]]("alg")
        use <- c.get[Option[String]]("use")
        n <- c.get[Option[String]]("n")
        e <- c.get[Option[String]]("e")
        crv <- c.get[Option[String]]("crv")
        x <- c.get[Option[String]]("x")
        y <- c.get[Option[String]]("y")
      yield JwkKey(kty, kid, alg, use, n, e, crv, x, y)
    }

  // === Validator Implementation ===

  private class JwksValidator[F[_]: Async](
      config: Config,
      httpClient: Client[F],
      cacheRef: Ref[F, Option[CachedKeys]]
  ) extends TokenValidator[F]:

    def validate(token: String): F[Either[AuthError, TokenInfo]] =
      (for
        parts <- Async[F].fromEither(splitJwt(token))
        (headerB64, payloadB64, signatureB64) = parts
        header <- Async[F].fromEither(decodeJson(headerB64))
        payload <- Async[F].fromEither(decodeJson(payloadB64))
        alg <- Async[F].fromEither(
          header.hcursor.get[String]("alg").leftMap(_ => AuthError.InvalidToken("Missing alg in JWT header"))
        )
        kid = header.hcursor.get[String]("kid").toOption
        keys <- getKeys
        key <- Async[F].fromEither(findKey(keys, kid, alg))
        _ <- Async[F].fromEither(verifySignature(headerB64, payloadB64, signatureB64, key, alg))
        tokenInfo <- Async[F].fromEither(extractAndValidateClaims(payload))
      yield tokenInfo).attempt.map {
        case Right(info) => Right(info)
        case Left(e: AuthError) => Left(e)
        case Left(e) => Left(AuthError.InvalidToken(e.getMessage))
      }

    private def getKeys: F[Map[String, JwkKey]] =
      for
        now <- Clock[F].realTime.map(_.toSeconds)
        cached <- cacheRef.get
        keys <- cached match
          case Some(c) if now - c.fetchedAt < config.cacheTtl.toSeconds =>
            Async[F].pure(c.keys)
          case _ =>
            fetchAndCacheKeys(now)
      yield keys

    private def fetchAndCacheKeys(now: Long): F[Map[String, JwkKey]] =
      val uri = Uri.unsafeFromString(config.jwksUri)
      httpClient.expect[Json](uri).flatMap { json =>
        json.hcursor.downField("keys").as[List[JwkKey]] match
          case Right(jwkKeys) =>
            val keyMap = jwkKeys
              .filter(k => k.use.forall(_ == "sig")) // Only signing keys
              .map(k => k.kid.getOrElse("default") -> k)
              .toMap
            cacheRef.set(Some(CachedKeys(keyMap, now))).as(keyMap)
          case Left(_) =>
            Async[F].raiseError(AuthError.InvalidToken("Failed to parse JWKS response"))
      }

    private def findKey(
        keys: Map[String, JwkKey],
        kid: Option[String],
        alg: String
    ): Either[AuthError, JwkKey] =
      kid match
        case Some(id) =>
          keys.get(id).toRight(AuthError.InvalidToken(s"Key ID '$id' not found in JWKS"))
        case None =>
          // No kid in JWT header - try to find a key matching the algorithm
          keys.values
            .find(k => k.alg.forall(_ == alg))
            .toRight(AuthError.InvalidToken("No matching key found in JWKS"))

    private def verifySignature(
        headerB64: String,
        payloadB64: String,
        signatureB64: String,
        key: JwkKey,
        alg: String
    ): Either[AuthError, Unit] =
      try
        val signingInput = s"$headerB64.$payloadB64".getBytes("UTF-8")
        val signatureBytes = decodeBase64Bytes(signatureB64)

        alg match
          case "RS256" => verifyRsa(signingInput, signatureBytes, key, "SHA256withRSA")
          case "RS384" => verifyRsa(signingInput, signatureBytes, key, "SHA384withRSA")
          case "RS512" => verifyRsa(signingInput, signatureBytes, key, "SHA512withRSA")
          case "ES256" => verifyEc(signingInput, signatureBytes, key, "SHA256withECDSA", "secp256r1")
          case "ES384" => verifyEc(signingInput, signatureBytes, key, "SHA384withECDSA", "secp384r1")
          case other   => Left(AuthError.InvalidToken(s"Unsupported algorithm: $other"))
      catch
        case e: Exception => Left(AuthError.InvalidToken(s"Signature verification failed: ${e.getMessage}"))

    private def verifyRsa(
        signingInput: Array[Byte],
        signatureBytes: Array[Byte],
        key: JwkKey,
        jcaAlg: String
    ): Either[AuthError, Unit] =
      for
        n <- key.n.toRight(AuthError.InvalidToken("RSA key missing modulus (n)"))
        e <- key.e.toRight(AuthError.InvalidToken("RSA key missing exponent (e)"))
        modulus = new BigInteger(1, decodeBase64Bytes(n))
        exponent = new BigInteger(1, decodeBase64Bytes(e))
        spec = RSAPublicKeySpec(modulus, exponent)
        publicKey = KeyFactory.getInstance("RSA").generatePublic(spec)
        sig = Signature.getInstance(jcaAlg)
        _ = sig.initVerify(publicKey)
        _ = sig.update(signingInput)
        result <- if sig.verify(signatureBytes) then Right(()) else Left(AuthError.InvalidToken("Invalid JWT signature"))
      yield result

    private def verifyEc(
        signingInput: Array[Byte],
        signatureBytes: Array[Byte],
        key: JwkKey,
        jcaAlg: String,
        curveName: String
    ): Either[AuthError, Unit] =
      for
        xStr <- key.x.toRight(AuthError.InvalidToken("EC key missing x coordinate"))
        yStr <- key.y.toRight(AuthError.InvalidToken("EC key missing y coordinate"))
        xBytes = decodeBase64Bytes(xStr)
        yBytes = decodeBase64Bytes(yStr)
        point = ECPoint(new BigInteger(1, xBytes), new BigInteger(1, yBytes))
        paramSpec = java.security.AlgorithmParameters.getInstance("EC")
        _ = paramSpec.init(java.security.spec.ECGenParameterSpec(curveName))
        ecSpec = paramSpec.getParameterSpec(classOf[java.security.spec.ECParameterSpec])
        spec = ECPublicKeySpec(point, ecSpec)
        publicKey = KeyFactory.getInstance("EC").generatePublic(spec)
        sig = Signature.getInstance(jcaAlg)
        _ = sig.initVerify(publicKey)
        _ = sig.update(signingInput)
        // EC signatures from JWTs are in raw R||S format, convert to DER for JCA
        derSig = ecRawToDer(signatureBytes, curveName)
        result <- if sig.verify(derSig) then Right(()) else Left(AuthError.InvalidToken("Invalid JWT signature"))
      yield result

    /** Convert raw R||S EC signature to DER format expected by JCA */
    private def ecRawToDer(raw: Array[Byte], curveName: String): Array[Byte] =
      val componentLen = raw.length / 2
      val r = new BigInteger(1, raw.take(componentLen))
      val s = new BigInteger(1, raw.drop(componentLen))
      val rBytes = r.toByteArray
      val sBytes = s.toByteArray
      val totalLen = rBytes.length + sBytes.length + 4
      val der = Array.newBuilder[Byte]
      der += 0x30.toByte
      der += totalLen.toByte
      der += 0x02.toByte
      der += rBytes.length.toByte
      der ++= rBytes
      der += 0x02.toByte
      der += sBytes.length.toByte
      der ++= sBytes
      der.result()

    private def extractAndValidateClaims(payload: Json): Either[AuthError, TokenInfo] =
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
      val issuerValid = config.issuer match
        case Some(expected) => issuer.contains(expected)
        case None => true

      // Validate audience
      val audienceValid = config.audience match
        case Some(expected) => audience.contains(expected)
        case None => true

      if !issuerValid then
        Left(AuthError.InvalidToken(s"Invalid issuer: expected ${config.issuer.getOrElse("")}, got ${issuer.getOrElse("none")}"))
      else if !audienceValid then
        Left(AuthError.InvalidAudience(config.audience.getOrElse(""), audience.getOrElse("none")))
      else
        Right(TokenInfo(
          subject = subject,
          audience = audience,
          scopes = scopes,
          issuer = issuer,
          expiration = expiration,
          claims = payload.asObject.map(_.toMap).getOrElse(Map.empty)
        ))

  // === Shared Helpers ===

  private def splitJwt(token: String): Either[AuthError, (String, String, String)] =
    token.split('.').toList match
      case header :: payload :: signature :: Nil => Right((header, payload, signature))
      case _ => Left(AuthError.InvalidToken("Invalid JWT format: expected 3 parts"))

  private def decodeJson(base64: String): Either[AuthError, Json] =
    try
      val padded = base64 + "=" * ((4 - base64.length % 4) % 4)
      val decoded = java.util.Base64.getUrlDecoder.decode(padded)
      val str = new String(decoded, "UTF-8")
      io.circe.parser.parse(str).leftMap(_ => AuthError.InvalidToken("Invalid JSON in JWT"))
    catch
      case _: Exception => Left(AuthError.InvalidToken("Invalid base64 encoding in JWT"))

  private def decodeBase64Bytes(s: String): Array[Byte] =
    val padded = s + "=" * ((4 - s.length % 4) % 4)
    java.util.Base64.getUrlDecoder.decode(padded)
