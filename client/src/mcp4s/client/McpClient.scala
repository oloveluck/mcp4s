package mcp4s.client

import cats.effect.Concurrent
import mcp4s.protocol.*

/** MCP client for connecting to MCP servers.
  *
  * Provides handlers for server-initiated requests:
  *   - Roots: Expose filesystem boundaries to servers
  *   - Sampling: Handle server-initiated LLM requests
  *   - Elicitation: Handle server-initiated user input requests
  *
  * Use [[McpClient.builder]] to create instances with the fluent DSL.
  *
  * @example
  * {{{
  * val client = McpClient.builder[IO]
  *   .withInfo(ClientInfo("my-client", "1.0.0"))
  *   .withRoots(List(Root("file:///workspace", Some("Workspace"))))
  *   .build
  * }}}
  */
trait McpClient[F[_]]:

  /** Client information sent during initialization */
  def info: ClientInfo

  /** Client capabilities */
  def capabilities: ClientCapabilities

  // === Server-Initiated Request Handlers (Client Features) ===

  /** Handle roots/list request from server
    * Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/client/roots
    */
  def listRoots: F[ListRootsResult]

  /** Handle sampling/createMessage request from server
    * Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/client/sampling
    */
  def createMessage(params: CreateMessageParams): F[CreateMessageResult]

  /** Handle elicitation/create request from server
    * Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/client/elicitation
    */
  def elicit(params: ElicitParams): F[ElicitResult]

object McpClient:

  /** Create a new builder for constructing an MCP client */
  def builder[F[_]: Concurrent]: McpClientBuilder[F] =
    McpClientBuilder.empty[F]
