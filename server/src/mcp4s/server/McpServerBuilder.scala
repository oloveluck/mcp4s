package mcp4s.server

import cats.Applicative
import cats.effect.Concurrent
import io.circe.Json
import mcp4s.protocol.*

/** Builder for constructing MCP servers with a fluent DSL.
  *
  * Example usage:
  * {{{
  * McpServer.builder[IO]
  *   .withInfo(ServerInfo("my-server", "1.0.0"))
  *   .withTool(
  *     Tool("greet", Some("Greet someone"), schema),
  *     args => IO.pure(ToolResult.text(s"Hello!"))
  *   )
  *   .build
  * }}}
  */
final class McpServerBuilder[F[_]: Concurrent] private (
    private val serverInfo: ServerInfo,
    private val tools: Map[String, (Tool, Json => F[ToolResult])],
    private val resources: Map[String, (Resource, String => F[ResourceContent])],
    private val resourceTemplates: List[ResourceTemplate],
    private val prompts: Map[String, (Prompt, Map[String, String] => F[GetPromptResult])]
):

  /** Set the server info */
  def withInfo(info: ServerInfo): McpServerBuilder[F] =
    new McpServerBuilder(info, tools, resources, resourceTemplates, prompts)

  /** Register a tool with its handler */
  def withTool(tool: Tool, handler: Json => F[ToolResult]): McpServerBuilder[F] =
    new McpServerBuilder(serverInfo, tools + (tool.name -> (tool, handler)), resources, resourceTemplates, prompts)

  /** Register a resource with its handler */
  def withResource(resource: Resource, handler: String => F[ResourceContent]): McpServerBuilder[F] =
    new McpServerBuilder(serverInfo, tools, resources + (resource.uri -> (resource, handler)), resourceTemplates, prompts)

  /** Register a resource template */
  def withResourceTemplate(template: ResourceTemplate): McpServerBuilder[F] =
    new McpServerBuilder(serverInfo, tools, resources, resourceTemplates :+ template, prompts)

  /** Register a prompt with its handler */
  def withPrompt(
      prompt: Prompt,
      handler: Map[String, String] => F[GetPromptResult]
  ): McpServerBuilder[F] =
    new McpServerBuilder(serverInfo, tools, resources, resourceTemplates, prompts + (prompt.name -> (prompt, handler)))

  /** Build the server with computed capabilities */
  def build: McpServer[F] =
    val caps = ServerCapabilities(
      tools = if tools.nonEmpty then Some(ToolsCapability()) else None,
      resources = if resources.nonEmpty || resourceTemplates.nonEmpty then Some(ResourcesCapability()) else None,
      prompts = if prompts.nonEmpty then Some(PromptsCapability()) else None
    )
    new BuiltMcpServer[F](serverInfo, caps, tools, resources, resourceTemplates, prompts)

private final class BuiltMcpServer[F[_]: Concurrent](
    val info: ServerInfo,
    val capabilities: ServerCapabilities,
    private val tools: Map[String, (Tool, Json => F[ToolResult])],
    private val resources: Map[String, (Resource, String => F[ResourceContent])],
    private val resourceTemplates: List[ResourceTemplate],
    private val prompts: Map[String, (Prompt, Map[String, String] => F[GetPromptResult])]
) extends McpServer[F]:

  def listTools: F[List[Tool]] =
    Applicative[F].pure(tools.values.map(_._1).toList)

  def callTool(name: String, arguments: Json): F[ToolResult] =
    tools.get(name) match
      case Some((_, handler)) => handler(arguments)
      case None =>
        Concurrent[F].raiseError(McpError.ToolNotFound(name))

  def listResources: F[List[Resource]] =
    Applicative[F].pure(resources.values.map(_._1).toList)

  def listResourceTemplates: F[List[ResourceTemplate]] =
    Applicative[F].pure(resourceTemplates)

  def readResource(uri: String): F[ResourceContent] =
    resources.get(uri) match
      case Some((_, handler)) => handler(uri)
      case None =>
        Concurrent[F].raiseError(McpError.ResourceNotFound(uri))

  def listPrompts: F[List[Prompt]] =
    Applicative[F].pure(prompts.values.map(_._1).toList)

  def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
    prompts.get(name) match
      case Some((_, handler)) => handler(arguments)
      case None =>
        Concurrent[F].raiseError(McpError.PromptNotFound(name))

object McpServerBuilder:
  def empty[F[_]: Concurrent]: McpServerBuilder[F] =
    new McpServerBuilder[F](
      ServerInfo("mcp4s", "0.1.0"),
      Map.empty,
      Map.empty,
      Nil,
      Map.empty
    )
