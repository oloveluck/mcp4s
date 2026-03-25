package mcp4s.server

import cats.Semigroup
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*

/** Core MCP server abstraction.
  *
  * Implementors provide handlers for tools, resources, and prompts. The server handles protocol
  * lifecycle, capability negotiation, and request routing.
  */
trait Server[F[_]]:

  /** Server information returned during initialization */
  def info: ServerInfo

  /** Server capabilities */
  def capabilities: ServerCapabilities

  // === Tool Methods ===

  /** List available tools */
  def listTools: F[List[Tool]]

  /** Call a tool with the given arguments */
  def callTool(name: String, arguments: Json): F[ToolResult]

  /** Call a tool with context for server-to-client operations.
    * Falls back to regular callTool if tool doesn't require context.
    */
  def callToolWithContext(name: String, arguments: Json, context: ToolContext[F]): F[ToolResult] =
    callTool(name, arguments) // Default: ignore context

  // === Resource Methods ===

  /** List available resources */
  def listResources: F[List[Resource]]

  /** List available resource templates */
  def listResourceTemplates: F[List[ResourceTemplate]]

  /** Read a resource by URI */
  def readResource(uri: String): F[ResourceContent]

  // === Prompt Methods ===

  /** List available prompts */
  def listPrompts: F[List[Prompt]]

  /** Get a prompt with the given arguments */
  def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult]

object Server:

  /** Create a new builder for constructing an MCP server */
  def builder[F[_]: Concurrent]: ServerBuilder[F] =
    ServerBuilder.empty[F]

  /** Create a server declaratively from composed parts.
    *
    * {{{
    * val add = McpTool[IO]("add", "Add")(number("a") *: number("b")) { case (a, b) =>
    *   IO.pure(ToolResult.text(s"${a + b}"))
    * }
    * val readme = McpResource[IO]("file:///readme", "README")("Hello")
    * val greet = McpPrompt.noArgs[IO]("greet", "Greet")(IO.pure(GetPromptResult(...)))
    *
    * val server = Server.from[IO](
    *   info      = ServerInfo("calc", "1.0.0"),
    *   tools     = add,
    *   resources = readme,
    *   prompts   = greet
    * )
    * }}}
    */
  def from[F[_]: Concurrent](
      info: ServerInfo,
      tools: Tools[F],
      resources: Resources[F],
      prompts: Prompts[F]
  ): Server[F] =
    DeclarativeServer(info, tools, resources, prompts)

  /** Create a server with only tool routes */
  def fromTools[F[_]: Concurrent](
      info: ServerInfo,
      tools: Tools[F]
  ): Server[F] =
    DeclarativeServer(info, tools, Resources.empty[F], Prompts.empty[F])

  /** Semigroup instance for composing MCP servers.
    *
    * When combining servers, the left server takes precedence for conflicts (same tool name, resource
    * URI, or prompt name). Capabilities are merged with OR logic.
    */
  given [F[_]: Concurrent]: Semigroup[Server[F]] with
    def combine(x: Server[F], y: Server[F]): Server[F] =
      ComposedServer(x, y)

  extension [F[_]: Concurrent](server: Server[F])

    /** Combine with another server. This server's handlers take precedence on conflicts. */
    def combine(other: Server[F]): Server[F] =
      ComposedServer(server, other)

    /** Alias for combine using http4s-style operator. */
    def <+>(other: Server[F]): Server[F] =
      combine(other)

    /** Create a new server with different info. */
    def withInfo(newInfo: ServerInfo): Server[F] =
      new Server[F]:
        val info: ServerInfo = newInfo
        val capabilities: ServerCapabilities = server.capabilities
        def listTools: F[List[Tool]] = server.listTools
        def callTool(name: String, arguments: Json): F[ToolResult] = server.callTool(name, arguments)
        def listResources: F[List[Resource]] = server.listResources
        def listResourceTemplates: F[List[ResourceTemplate]] = server.listResourceTemplates
        def readResource(uri: String): F[ResourceContent] = server.readResource(uri)
        def listPrompts: F[List[Prompt]] = server.listPrompts
        def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
          server.getPrompt(name, arguments)

/** Composed MCP server that delegates to two underlying servers.
  *
  * The left server takes precedence for conflicts. Capabilities are merged.
  */
private final class ComposedServer[F[_]: Concurrent](
    left: Server[F],
    right: Server[F]
) extends Server[F]:

  val info: ServerInfo = left.info

  val capabilities: ServerCapabilities = mergeCapabilities(left.capabilities, right.capabilities)

  def listTools: F[List[Tool]] =
    for
      leftTools <- left.listTools
      rightTools <- right.listTools
      leftNames = leftTools.map(_.name).toSet
    yield leftTools ++ rightTools.filterNot(t => leftNames.contains(t.name))

  def callTool(name: String, arguments: Json): F[ToolResult] =
    left.callTool(name, arguments).recoverWith { case McpError.ToolNotFound(_) =>
      right.callTool(name, arguments)
    }

  def listResources: F[List[Resource]] =
    for
      leftRes <- left.listResources
      rightRes <- right.listResources
      leftUris = leftRes.map(_.uri).toSet
    yield leftRes ++ rightRes.filterNot(r => leftUris.contains(r.uri))

  def listResourceTemplates: F[List[ResourceTemplate]] =
    for
      leftTemplates <- left.listResourceTemplates
      rightTemplates <- right.listResourceTemplates
      leftUris = leftTemplates.map(_.uriTemplate).toSet
    yield leftTemplates ++ rightTemplates.filterNot(t => leftUris.contains(t.uriTemplate))

  def readResource(uri: String): F[ResourceContent] =
    left.readResource(uri).recoverWith { case McpError.ResourceNotFound(_) =>
      right.readResource(uri)
    }

  def listPrompts: F[List[Prompt]] =
    for
      leftPrompts <- left.listPrompts
      rightPrompts <- right.listPrompts
      leftNames = leftPrompts.map(_.name).toSet
    yield leftPrompts ++ rightPrompts.filterNot(p => leftNames.contains(p.name))

  def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
    left.getPrompt(name, arguments).recoverWith { case McpError.PromptNotFound(_) =>
      right.getPrompt(name, arguments)
    }

  private def mergeCapabilities(l: ServerCapabilities, r: ServerCapabilities): ServerCapabilities =
    ServerCapabilities(
      tools = l.tools.orElse(r.tools),
      resources = l.resources.orElse(r.resources),
      prompts = l.prompts.orElse(r.prompts),
      logging = l.logging.orElse(r.logging),
      completions = l.completions.orElse(r.completions),
      tasks = l.tasks.orElse(r.tasks),
      experimental = mergeExperimental(l.experimental, r.experimental)
    )

  private def mergeExperimental(l: Option[Json], r: Option[Json]): Option[Json] =
    (l, r) match
      case (Some(lj), Some(rj)) => Some(lj.deepMerge(rj))
      case (Some(lj), None)     => Some(lj)
      case (None, Some(rj))     => Some(rj)
      case (None, None)         => None

/** MCP server assembled declaratively from composed Tools, Resources, Prompts. */
private final class DeclarativeServer[F[_]: Concurrent](
    val info: ServerInfo,
    private val tools: Tools[F],
    private val resources: Resources[F],
    private val prompts: Prompts[F]
) extends Server[F]:

  val capabilities: ServerCapabilities =
    // Capabilities are determined lazily based on what's registered.
    // Since we can't peek inside at construction time without running F,
    // we optimistically declare all capabilities that have registered handlers.
    ServerCapabilities(
      tools = Some(ToolsCapability()),
      resources = Some(ResourcesCapability(subscribe = Some(true))),
      prompts = Some(PromptsCapability()),
      logging = Some(LoggingCapability()),
      completions = Some(CompletionsCapability())
    )

  def listTools: F[List[Tool]] = tools.list

  def callTool(name: String, arguments: Json): F[ToolResult] =
    tools.call(name, arguments).getOrElseF(
      Concurrent[F].raiseError(McpError.ToolNotFound(name))
    )

  override def callToolWithContext(name: String, arguments: Json, context: ToolContext[F]): F[ToolResult] =
    tools.callWithContext(name, arguments, context).getOrElseF(
      Concurrent[F].raiseError(McpError.ToolNotFound(name))
    )

  def listResources: F[List[Resource]] = resources.list

  def listResourceTemplates: F[List[ResourceTemplate]] = resources.listTemplates

  def readResource(uri: String): F[ResourceContent] =
    resources.read(uri).getOrElseF(
      Concurrent[F].raiseError(McpError.ResourceNotFound(uri))
    )

  def listPrompts: F[List[Prompt]] = prompts.list

  def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
    prompts.get(name, arguments).getOrElseF(
      Concurrent[F].raiseError(McpError.PromptNotFound(name))
    )
