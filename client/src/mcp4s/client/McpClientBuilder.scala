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
    private val state: McpClientBuilder.State[F]
):

  /** Set the client info */
  def withInfo(info: ClientInfo): McpClientBuilder[F] =
    new McpClientBuilder(state.copy(clientInfo = info))

  /** Set roots that can be exposed to servers */
  def withRoots(newRoots: List[Root]): McpClientBuilder[F] =
    new McpClientBuilder(state.copy(roots = newRoots, rootsListChanged = true))

  /** Set composed roots provider */
  def withRoots(roots: Roots[F]): McpClientBuilder[F] =
    new McpClientBuilder(state.copy(rootsListChanged = true, mcpRoots = Some(roots)))

  /** Add a single root */
  def withRoot(root: Root): McpClientBuilder[F] =
    withRoots(state.roots :+ root)

  /** Add a root by URI and optional name */
  def withRoot(uri: String, name: Option[String] = None): McpClientBuilder[F] =
    withRoot(Root(uri, name))

  /** Register sampling handler for server-initiated LLM requests */
  def withSamplingHandler(handler: CreateMessageParams => F[CreateMessageResult]): McpClientBuilder[F] =
    new McpClientBuilder(state.copy(samplingHandler = Some(handler)))

  /** Register composed sampling handler */
  def withSampling(sampling: Samplings[F]): McpClientBuilder[F] =
    new McpClientBuilder(state.copy(sampling = Some(sampling)))

  /** Register elicitation handler for server-initiated user input */
  def withElicitationHandler(handler: ElicitParams => F[ElicitResult]): McpClientBuilder[F] =
    new McpClientBuilder(state.copy(elicitationHandler = Some(handler)))

  /** Register composed elicitation handler */
  def withElicitation(elicitation: Elicitations[F]): McpClientBuilder[F] =
    new McpClientBuilder(state.copy(elicitation = Some(elicitation)))

  /** Register handler for URL mode elicitation completion notifications */
  def withElicitationCompleteHandler(handler: ElicitationCompleteParams => F[Unit]): McpClientBuilder[F] =
    new McpClientBuilder(state.copy(elicitationCompleteHandler = Some(handler)))

  /** Build the client with computed capabilities */
  def build: McpClient[F] =
    // Merge raw handlers with composed handlers (composed handlers take precedence)
    val effectiveSampling = state.sampling.orElse(state.samplingHandler.map(Samplings.apply[F]))
    val effectiveElicitation = state.elicitation.orElse(
      state.elicitationHandler.map { handler =>
        state.elicitationCompleteHandler match
          case Some(complete) => Elicitations.withComplete[F](handler, complete)
          case None           => Elicitations[F](handler)
      }
    )

    val hasRoots = state.roots.nonEmpty || state.mcpRoots.isDefined
    val caps = ClientCapabilities(
      roots = if hasRoots then Some(RootsCapability(Some(state.rootsListChanged))) else None,
      sampling = effectiveSampling.map(_ => SamplingCapability()),
      elicitation = effectiveElicitation.map(_ => ElicitationCapability())
    )
    new ComposedMcpClient[F](state.clientInfo, caps, state.roots, state.mcpRoots, effectiveSampling, effectiveElicitation)

/** Client implementation using composed handlers */
private[client] final class ComposedMcpClient[F[_]: Concurrent](
    val info: ClientInfo,
    val capabilities: ClientCapabilities,
    private val staticRoots: List[Root],
    private val mcpRoots: Option[Roots[F]],
    private val samplingHandler: Option[Samplings[F]],
    private val elicitationHandler: Option[Elicitations[F]]
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
  private case class State[F[_]](
      clientInfo: ClientInfo = ClientInfo("mcp4s-client", "0.1.0"),
      roots: List[Root] = Nil,
      samplingHandler: Option[CreateMessageParams => F[CreateMessageResult]] = None,
      elicitationHandler: Option[ElicitParams => F[ElicitResult]] = None,
      elicitationCompleteHandler: Option[ElicitationCompleteParams => F[Unit]] = None,
      rootsListChanged: Boolean = false,
      sampling: Option[Samplings[F]] = None,
      elicitation: Option[Elicitations[F]] = None,
      mcpRoots: Option[Roots[F]] = None
  )

  def empty[F[_]: Concurrent]: McpClientBuilder[F] =
    new McpClientBuilder[F](State[F]())
