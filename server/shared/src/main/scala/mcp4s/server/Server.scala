/*
 * Copyright 2025 MCP4S Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

  /** Call a tool with context for server-to-client operations. Falls back to regular callTool if
    * tool doesn't require context.
    */
  def callToolWithContext(
      name: String,
      arguments: Json,
      @scala.annotation.unused context: ToolContext[F]
  ): F[ToolResult] =
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

  /** Create a server declaratively from composed parts.
    *
    * {{{
    * import mcp4s.server.dsl.*
    *
    * case class CalcArgs(a: Double, b: Double) derives Schema
    * val add = Tool.from[CalcArgs].withDescription("Add").handle[IO] { args =>
    *   IO.pure(ToolResult.text(s"${args.a + args.b}"))
    * }
    * val readme = Resource.text[IO]("file:///readme", "README")("Hello")
    * val greet = Prompt("greet").withDescription("Greet").messages[IO](user("Hello!"))
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
    * When combining servers, the left server takes precedence for conflicts (same tool name,
    * resource URI, or prompt name). Capabilities are merged with OR logic.
    */
  given [F[_]: Concurrent]: Semigroup[Server[F]] with
    def combine(x: Server[F], y: Server[F]): Server[F] =
      ComposedServer(x, y)

  extension [F[_]: Concurrent](server: Server[F])
    /** Combine with another server. This server's handlers take precedence on conflicts. Servers
      * also compose with `|+|` via their [[cats.Semigroup]] instance.
      */
    def combine(other: Server[F]): Server[F] =
      ComposedServer(server, other)

  extension [F[_]](server: Server[F])
    /** Bind this server to the stdio transport: `server.stdio.run`. */
    def stdio: McpServer.StdioBinding[F] = McpServer.StdioBinding(server)

    /** Bind this server to the Streamable HTTP transport: `server.http().resource` or
      * `server.http(config).routes` for embedding.
      */
    def http(
        config: mcp4s.server.transport.HttpConfig[F] = mcp4s.server.transport.HttpConfig[F]()
    ): McpServer.HttpBinding[F] = McpServer.HttpBinding(server, config)

    /** Bind this server to the WebSocket transport: `server.webSocket().resource`. */
    def webSocket(
        config: mcp4s.server.transport.WebSocketConfig = mcp4s.server.transport.WebSocketConfig()
    ): McpServer.WebSocketBinding[F] = McpServer.WebSocketBinding(server, config)

  extension [F[_]](server: Server[F])
    /** Create a new server with different info. */
    def withInfo(newInfo: ServerInfo): Server[F] =
      new Server[F]:
        val info: ServerInfo                                       = newInfo
        val capabilities: ServerCapabilities                       = server.capabilities
        def listTools: F[List[Tool]]                               = server.listTools
        def callTool(name: String, arguments: Json): F[ToolResult] =
          server.callTool(name, arguments)
        def listResources: F[List[Resource]]                 = server.listResources
        def listResourceTemplates: F[List[ResourceTemplate]] = server.listResourceTemplates
        def readResource(uri: String): F[ResourceContent]    = server.readResource(uri)
        def listPrompts: F[List[Prompt]]                     = server.listPrompts
        def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
          server.getPrompt(name, arguments)

/** Composed MCP server that delegates to two underlying servers.
  *
  * The left server takes precedence for conflicts. Capabilities are merged.
  */
final private class ComposedServer[F[_]: Concurrent](
    left: Server[F],
    right: Server[F]
) extends Server[F]:

  val info: ServerInfo = left.info

  val capabilities: ServerCapabilities = mergeCapabilities(left.capabilities, right.capabilities)

  def listTools: F[List[Tool]] =
    for
      leftTools  <- left.listTools
      rightTools <- right.listTools
      leftNames = leftTools.map(_.name).toSet
    yield leftTools ++ rightTools.filterNot(t => leftNames.contains(t.name))

  def callTool(name: String, arguments: Json): F[ToolResult] =
    left.callTool(name, arguments).recoverWith { case McpError.ToolNotFound(_) =>
      right.callTool(name, arguments)
    }

  def listResources: F[List[Resource]] =
    for
      leftRes  <- left.listResources
      rightRes <- right.listResources
      leftUris = leftRes.map(_.uri).toSet
    yield leftRes ++ rightRes.filterNot(r => leftUris.contains(r.uri))

  def listResourceTemplates: F[List[ResourceTemplate]] =
    for
      leftTemplates  <- left.listResourceTemplates
      rightTemplates <- right.listResourceTemplates
      leftUris = leftTemplates.map(_.uriTemplate).toSet
    yield leftTemplates ++ rightTemplates.filterNot(t => leftUris.contains(t.uriTemplate))

  def readResource(uri: String): F[ResourceContent] =
    left.readResource(uri).recoverWith { case McpError.ResourceNotFound(_) =>
      right.readResource(uri)
    }

  def listPrompts: F[List[Prompt]] =
    for
      leftPrompts  <- left.listPrompts
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
final private class DeclarativeServer[F[_]: Concurrent](
    val info: ServerInfo,
    private val tools: Tools[F],
    private val resources: Resources[F],
    private val prompts: Prompts[F]
) extends Server[F]:

  val capabilities: ServerCapabilities =
    // Derived from what is actually registered: empty routes advertise no capability,
    // and resources.subscribe reflects whether any resource is subscribable.
    ServerCapabilities(
      tools = if tools.isEmpty then None else Some(ToolsCapability()),
      resources =
        if resources.isEmpty then None
        else Some(ResourcesCapability(subscribe = Some(resources.supportsSubscribe))),
      prompts = if prompts.isEmpty then None else Some(PromptsCapability()),
      logging = Some(LoggingCapability()),
      completions = Some(CompletionsCapability())
    )

  def listTools: F[List[Tool]] = tools.list

  def callTool(name: String, arguments: Json): F[ToolResult] =
    val ctx = ToolContext.minimal[F](SamplingRequester.unsupported[F], RequestId.NullId)
    tools
      .call(name, arguments, ctx)
      .getOrElseF(
        Concurrent[F].raiseError(McpError.ToolNotFound(name))
      )

  override def callToolWithContext(
      name: String,
      arguments: Json,
      context: ToolContext[F]
  ): F[ToolResult] =
    tools
      .call(name, arguments, context)
      .getOrElseF(
        Concurrent[F].raiseError(McpError.ToolNotFound(name))
      )

  def listResources: F[List[Resource]] = resources.list

  def listResourceTemplates: F[List[ResourceTemplate]] = resources.listTemplates

  def readResource(uri: String): F[ResourceContent] =
    resources
      .read(uri)
      .getOrElseF(
        Concurrent[F].raiseError(McpError.ResourceNotFound(uri))
      )

  def listPrompts: F[List[Prompt]] = prompts.list

  def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
    prompts
      .get(name, arguments)
      .getOrElseF(
        Concurrent[F].raiseError(McpError.PromptNotFound(name))
      )
