package mcp4s.client

import cats.Applicative
import cats.effect.Concurrent
import mcp4s.protocol.*

/** Builder for constructing MCP clients with a fluent DSL.
  *
  * @example
  * {{{
  * McpClient.builder[IO]
  *   .withInfo(ClientInfo("my-client", "1.0.0"))
  *   .withRoots(List(Root("file:///workspace", Some("Workspace"))))
  *   .withSamplingHandler(params => IO.pure(CreateMessageResult(...)))
  *   .build
  * }}}
  */
final class McpClientBuilder[F[_]: Concurrent] private (
    private val clientInfo: ClientInfo,
    private val roots: List[Root],
    private val samplingHandler: Option[CreateMessageParams => F[CreateMessageResult]],
    private val elicitationHandler: Option[ElicitParams => F[ElicitResult]],
    private val rootsListChanged: Boolean
):

  /** Set the client info */
  def withInfo(info: ClientInfo): McpClientBuilder[F] =
    new McpClientBuilder(info, roots, samplingHandler, elicitationHandler, rootsListChanged)

  /** Set roots that can be exposed to servers */
  def withRoots(newRoots: List[Root]): McpClientBuilder[F] =
    new McpClientBuilder(clientInfo, newRoots, samplingHandler, elicitationHandler, true)

  /** Add a single root */
  def withRoot(root: Root): McpClientBuilder[F] =
    withRoots(roots :+ root)

  /** Add a root by URI and optional name */
  def withRoot(uri: String, name: Option[String] = None): McpClientBuilder[F] =
    withRoot(Root(uri, name))

  /** Register sampling handler for server-initiated LLM requests */
  def withSamplingHandler(handler: CreateMessageParams => F[CreateMessageResult]): McpClientBuilder[F] =
    new McpClientBuilder(clientInfo, roots, Some(handler), elicitationHandler, rootsListChanged)

  /** Register elicitation handler for server-initiated user input */
  def withElicitationHandler(handler: ElicitParams => F[ElicitResult]): McpClientBuilder[F] =
    new McpClientBuilder(clientInfo, roots, samplingHandler, Some(handler), rootsListChanged)

  /** Build the client with computed capabilities */
  def build: McpClient[F] =
    val caps = ClientCapabilities(
      roots = if roots.nonEmpty then Some(RootsCapability(Some(rootsListChanged))) else None,
      sampling = samplingHandler.map(_ => SamplingCapability()),
      elicitation = elicitationHandler.map(_ => ElicitationCapability())
    )
    new BuiltMcpClient[F](clientInfo, caps, roots, samplingHandler, elicitationHandler)

private[client] final class BuiltMcpClient[F[_]: Concurrent](
    val info: ClientInfo,
    val capabilities: ClientCapabilities,
    private val roots: List[Root],
    private val samplingHandler: Option[CreateMessageParams => F[CreateMessageResult]],
    private val elicitationHandler: Option[ElicitParams => F[ElicitResult]]
) extends McpClient[F]:

  def listRoots: F[ListRootsResult] =
    Applicative[F].pure(ListRootsResult(roots))

  def createMessage(params: CreateMessageParams): F[CreateMessageResult] =
    samplingHandler match
      case Some(handler) => handler(params)
      case None =>
        Concurrent[F].raiseError(McpError.MethodNotSupported("sampling/createMessage"))

  def elicit(params: ElicitParams): F[ElicitResult] =
    elicitationHandler match
      case Some(handler) => handler(params)
      case None =>
        Concurrent[F].raiseError(McpError.MethodNotSupported("elicitation/create"))

object McpClientBuilder:
  def empty[F[_]: Concurrent]: McpClientBuilder[F] =
    new McpClientBuilder[F](
      ClientInfo("mcp4s-client", "0.1.0"),
      Nil,
      None,
      None,
      false
    )
