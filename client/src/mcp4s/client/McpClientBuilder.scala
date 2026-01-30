package mcp4s.client

import cats.Applicative
import cats.effect.Concurrent
import cats.syntax.all.*
import mcp4s.protocol.*

/** Builder for constructing MCP clients with a fluent DSL.
  *
  * Supports both raw function handlers and composed handler types.
  *
  * @example
  * {{{
  * // Using raw handlers
  * McpClient.builder[IO]
  *   .withInfo(ClientInfo("my-client", "1.0.0"))
  *   .withRoots(List(Root("file:///workspace", Some("Workspace"))))
  *   .withSamplingHandler(params => IO.pure(CreateMessageResult(...)))
  *   .build
  *
  * // Using composed handlers
  * import mcp4s.client.mcp.*
  *
  * val sampling = Sampling[IO](params => message("Hello", "model").pure[IO])
  * val roots = Roots[IO]("file:///workspace", "Workspace")
  *
  * McpClient.builder[IO]
  *   .withInfo(ClientInfo("my-client", "1.0.0"))
  *   .withRoots(roots)
  *   .withSampling(sampling)
  *   .build
  * }}}
  */
final class McpClientBuilder[F[_]: Concurrent] private (
    private val clientInfo: ClientInfo,
    private val roots: List[Root],
    private val samplingHandler: Option[CreateMessageParams => F[CreateMessageResult]],
    private val elicitationHandler: Option[ElicitParams => F[ElicitResult]],
    private val elicitationCompleteHandler: Option[ElicitationCompleteParams => F[Unit]],
    private val rootsListChanged: Boolean,
    private val mcpSampling: Option[McpSamplings[F]],
    private val mcpElicitation: Option[McpElicitations[F]],
    private val mcpRoots: Option[McpRoots[F]]
):

  /** Set the client info */
  def withInfo(info: ClientInfo): McpClientBuilder[F] =
    new McpClientBuilder(info, roots, samplingHandler, elicitationHandler, elicitationCompleteHandler, rootsListChanged, mcpSampling, mcpElicitation, mcpRoots)

  /** Set roots that can be exposed to servers */
  def withRoots(newRoots: List[Root]): McpClientBuilder[F] =
    new McpClientBuilder(clientInfo, newRoots, samplingHandler, elicitationHandler, elicitationCompleteHandler, true, mcpSampling, mcpElicitation, mcpRoots)

  /** Set composed roots provider */
  def withRoots(roots: McpRoots[F]): McpClientBuilder[F] =
    new McpClientBuilder(clientInfo, this.roots, samplingHandler, elicitationHandler, elicitationCompleteHandler, true, mcpSampling, mcpElicitation, Some(roots))

  /** Add a single root */
  def withRoot(root: Root): McpClientBuilder[F] =
    withRoots(roots :+ root)

  /** Add a root by URI and optional name */
  def withRoot(uri: String, name: Option[String] = None): McpClientBuilder[F] =
    withRoot(Root(uri, name))

  /** Register sampling handler for server-initiated LLM requests */
  def withSamplingHandler(handler: CreateMessageParams => F[CreateMessageResult]): McpClientBuilder[F] =
    new McpClientBuilder(clientInfo, roots, Some(handler), elicitationHandler, elicitationCompleteHandler, rootsListChanged, mcpSampling, mcpElicitation, mcpRoots)

  /** Register composed sampling handler */
  def withSampling(sampling: McpSamplings[F]): McpClientBuilder[F] =
    new McpClientBuilder(clientInfo, roots, samplingHandler, elicitationHandler, elicitationCompleteHandler, rootsListChanged, Some(sampling), mcpElicitation, mcpRoots)

  /** Register elicitation handler for server-initiated user input */
  def withElicitationHandler(handler: ElicitParams => F[ElicitResult]): McpClientBuilder[F] =
    new McpClientBuilder(clientInfo, roots, samplingHandler, Some(handler), elicitationCompleteHandler, rootsListChanged, mcpSampling, mcpElicitation, mcpRoots)

  /** Register composed elicitation handler */
  def withElicitation(elicitation: McpElicitations[F]): McpClientBuilder[F] =
    new McpClientBuilder(clientInfo, roots, samplingHandler, elicitationHandler, elicitationCompleteHandler, rootsListChanged, mcpSampling, Some(elicitation), mcpRoots)

  /** Register handler for URL mode elicitation completion notifications */
  def withElicitationCompleteHandler(handler: ElicitationCompleteParams => F[Unit]): McpClientBuilder[F] =
    new McpClientBuilder(clientInfo, roots, samplingHandler, elicitationHandler, Some(handler), rootsListChanged, mcpSampling, mcpElicitation, mcpRoots)

  /** Build the client with computed capabilities */
  def build: McpClient[F] =
    // Merge raw handlers with composed handlers (composed handlers take precedence)
    val effectiveSampling = mcpSampling.orElse(samplingHandler.map(McpSamplings.apply[F]))
    val effectiveElicitation = mcpElicitation.orElse(
      elicitationHandler.map { handler =>
        elicitationCompleteHandler match
          case Some(complete) => McpElicitations.withComplete[F](handler, complete)
          case None           => McpElicitations[F](handler)
      }
    )

    val hasRoots = roots.nonEmpty || mcpRoots.isDefined
    val caps = ClientCapabilities(
      roots = if hasRoots then Some(RootsCapability(Some(rootsListChanged))) else None,
      sampling = effectiveSampling.map(_ => SamplingCapability()),
      elicitation = effectiveElicitation.map(_ => ElicitationCapability())
    )
    new ComposedMcpClient[F](clientInfo, caps, roots, mcpRoots, effectiveSampling, effectiveElicitation)

/** Client implementation using composed handlers */
private[client] final class ComposedMcpClient[F[_]: Concurrent](
    val info: ClientInfo,
    val capabilities: ClientCapabilities,
    private val staticRoots: List[Root],
    private val mcpRoots: Option[McpRoots[F]],
    private val samplingHandler: Option[McpSamplings[F]],
    private val elicitationHandler: Option[McpElicitations[F]]
) extends McpClient[F]:

  def listRoots: F[ListRootsResult] =
    mcpRoots match
      case Some(roots) =>
        roots.list.map { dynamic =>
          ListRootsResult(staticRoots ++ dynamic)
        }
      case None =>
        Applicative[F].pure(ListRootsResult(staticRoots))

  def createMessage(params: CreateMessageParams): F[CreateMessageResult] =
    samplingHandler match
      case Some(handler) =>
        handler.handle(params).value.flatMap {
          case Some(result) => Concurrent[F].pure(result)
          case None =>
            Concurrent[F].raiseError(McpError.MethodNotSupported("sampling/createMessage"))
        }
      case None =>
        Concurrent[F].raiseError(McpError.MethodNotSupported("sampling/createMessage"))

  def elicit(params: ElicitParams): F[ElicitResult] =
    elicitationHandler match
      case Some(handler) =>
        handler.handle(params).value.flatMap {
          case Some(result) => Concurrent[F].pure(result)
          case None =>
            Concurrent[F].raiseError(McpError.MethodNotSupported("elicitation/create"))
        }
      case None =>
        Concurrent[F].raiseError(McpError.MethodNotSupported("elicitation/create"))

  def onElicitationComplete(params: ElicitationCompleteParams): F[Unit] =
    elicitationHandler match
      case Some(handler) => handler.onComplete(params)
      case None          => Concurrent[F].unit

object McpClientBuilder:
  def empty[F[_]: Concurrent]: McpClientBuilder[F] =
    new McpClientBuilder[F](
      ClientInfo("mcp4s-client", "0.1.0"),
      Nil,
      None,
      None,
      None,
      false,
      None,
      None,
      None
    )
