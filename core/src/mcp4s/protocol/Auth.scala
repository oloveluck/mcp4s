package mcp4s.protocol

import io.circe.Json

/** Protected Resource Metadata per RFC 9728.
  *
  * This metadata is exposed at `/.well-known/oauth-protected-resource`
  * and allows OAuth clients to discover the authorization servers
  * that can issue tokens for this protected resource.
  *
  * @param resource Canonical URI of this protected resource
  * @param authorizationServers URLs of authorization servers that can issue tokens
  * @param scopesSupported OAuth scopes that this resource understands
  * @param bearerMethodsSupported Methods for sending bearer tokens (typically "header")
  */
final case class ProtectedResourceMetadata(
    resource: String,
    authorizationServers: List[String],
    scopesSupported: Option[List[String]] = None,
    bearerMethodsSupported: Option[List[String]] = Some(List("header"))
)

/** Information extracted from a validated access token.
  *
  * @param subject The authenticated subject (user/client ID)
  * @param audience The intended audience for this token
  * @param scopes OAuth scopes granted by this token
  * @param issuer The issuer of this token (authorization server)
  * @param expiration Unix timestamp when this token expires
  * @param claims Additional claims from the token
  */
final case class TokenInfo(
    subject: String,
    audience: Option[String] = None,
    scopes: Set[String] = Set.empty,
    issuer: Option[String] = None,
    expiration: Option[Long] = None,
    claims: Map[String, Json] = Map.empty
)
