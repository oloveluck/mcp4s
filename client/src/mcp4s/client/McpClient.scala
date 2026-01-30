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
  * Use [[McpClient.builder]] to create instances with the fluent DSL,
  * or [[McpClient.from]] for a compositional approach.
  *
  * @example
  * {{{
  * // Builder approach
  * val client = McpClient.builder[IO]
  *   .withInfo(ClientInfo("my-client", "1.0.0"))
  *   .withRoots(List(Root("file:///workspace", Some("Workspace"))))
  *   .build
  *
  * // Compositional approach
  * import mcp4s.client.mcp.*
  *
  * val sampling = Sampling[IO](params => message("Hello", "model").pure[IO])
  * val roots = Roots[IO]("file:///workspace", "Workspace")
  *
  * val client = McpClient.from[IO](
  *   info = ClientInfo("my-client", "1.0.0"),
  *   roots = Some(roots),
  *   sampling = Some(sampling)
  * )
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

  /** Handle notifications/elicitation/complete from server
    * Called when URL mode elicitation completes (e.g., OAuth flow finished)
    * Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/client/elicitation
    */
  def onElicitationComplete(params: ElicitationCompleteParams): F[Unit]

object McpClient:

  /** Create a new builder for constructing an MCP client */
  def builder[F[_]: Concurrent]: McpClientBuilder[F] =
    McpClientBuilder.empty[F]

  /** Create a client from composed handlers.
    *
    * This is the compositional alternative to the builder pattern, allowing
    * handlers to be defined separately and combined.
    *
    * @example
    * {{{
    * import mcp4s.client.mcp.*
    * import cats.syntax.semigroup.*
    *
    * val sampling = Sampling[IO](params => message("Hello", "model").pure[IO])
    * val roots = Roots[IO]("file:///workspace", "Workspace") |+|
    *             Roots[IO]("file:///home", "Home")
    *
    * val client = McpClient.from[IO](
    *   info = ClientInfo("my-client", "1.0.0"),
    *   roots = Some(roots),
    *   sampling = Some(sampling)
    * )
    * }}}
    */
  def from[F[_]: Concurrent](
      info: ClientInfo,
      roots: Option[McpRoots[F]] = None,
      sampling: Option[McpSamplings[F]] = None,
      elicitation: Option[McpElicitations[F]] = None
  ): McpClient[F] =
    val caps = ClientCapabilities(
      roots = roots.map(_ => RootsCapability(Some(true))),
      sampling = sampling.map(_ => SamplingCapability()),
      elicitation = elicitation.map(_ => ElicitationCapability())
    )

    new ComposedMcpClient[F](
      info,
      caps,
      Nil,
      roots,
      sampling,
      elicitation
    )
