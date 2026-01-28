package mcp4s.protocol

/** Authentication and authorization errors for OAuth resource server.
  *
  * These errors are used by the auth middleware to communicate
  * token validation failures to clients per RFC 6750.
  */
sealed abstract class AuthError(val message: String) extends Exception(message)

object AuthError:

  /** No credentials provided - return 401 with WWW-Authenticate header.
    *
    * @param resourceMetadataUri URI to the protected resource metadata endpoint
    */
  final case class MissingCredentials(resourceMetadataUri: String)
      extends AuthError("No authorization credentials provided")

  /** Token is invalid (malformed, bad signature, etc).
    *
    * @param reason Human-readable description of why the token is invalid
    */
  final case class InvalidToken(reason: String)
      extends AuthError(s"Invalid token: $reason")

  /** Token has expired.
    *
    * @param expiredAt Unix timestamp when the token expired
    */
  final case class TokenExpired(expiredAt: Long)
      extends AuthError(s"Token expired at $expiredAt")

  /** Token audience does not match this resource server.
    *
    * @param expected The expected audience (this server's resource URI)
    * @param actual The actual audience from the token
    */
  final case class InvalidAudience(expected: String, actual: Option[String])
      extends AuthError(s"Invalid audience: expected $expected, got ${actual.getOrElse("none")}")

  /** Token does not have required scopes.
    *
    * @param required Scopes required by the operation
    * @param actual Scopes present in the token
    */
  final case class InsufficientScope(required: Set[String], actual: Set[String])
      extends AuthError(s"Insufficient scope: required ${required.mkString(", ")}, got ${actual.mkString(", ")}")
