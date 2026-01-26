package mcp4s.server.auth

import mcp4s.protocol.ProtectedResourceMetadata

/** Configuration for OAuth resource server authentication.
  *
  * @param metadata Protected resource metadata exposed at well-known endpoint
  * @param validator Token validator for verifying bearer tokens
  * @param requiredScopes Global scopes required for all MCP operations
  */
final case class AuthConfig[F[_]](
    metadata: ProtectedResourceMetadata,
    validator: TokenValidator[F],
    requiredScopes: Set[String] = Set.empty
):
  /** URI for the protected resource metadata endpoint. */
  def metadataUri: String = s"${metadata.resource}/.well-known/oauth-protected-resource"
