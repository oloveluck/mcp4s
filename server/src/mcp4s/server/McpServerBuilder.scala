package mcp4s.server

import cats.Applicative
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*
import mcp4s.protocol.ToolInput
import mcp4s.protocol.PromptInput

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
    private val contextTools: Map[String, (Tool, (Json, ToolContext[F]) => F[ToolResult])],
    private val resources: Map[String, (Resource, String => F[ResourceContent])],
    private val resourceTemplates: List[ResourceTemplate],
    private val templateHandlers: Map[String, String => F[ResourceContent]],
    private val prompts: Map[String, (Prompt, Map[String, String] => F[GetPromptResult])],
    private val mcpTools: Option[McpTools[F]],
    private val mcpResources: Option[McpResources[F]],
    private val mcpPrompts: Option[McpPrompts[F]]
):

  /** Set the server info */
  def withInfo(info: ServerInfo): McpServerBuilder[F] =
    new McpServerBuilder(info, tools, contextTools, resources, resourceTemplates, templateHandlers, prompts, mcpTools, mcpResources, mcpPrompts)

  /** Register a tool with its handler */
  def withTool(tool: Tool, handler: Json => F[ToolResult]): McpServerBuilder[F] =
    new McpServerBuilder(serverInfo, tools + (tool.name -> (tool, handler)), contextTools, resources, resourceTemplates, templateHandlers, prompts, mcpTools, mcpResources, mcpPrompts)

  /** Register a context-aware tool with its handler */
  def withToolWithContext(tool: Tool, handler: (Json, ToolContext[F]) => F[ToolResult]): McpServerBuilder[F] =
    new McpServerBuilder(serverInfo, tools, contextTools + (tool.name -> (tool, handler)), resources, resourceTemplates, templateHandlers, prompts, mcpTools, mcpResources, mcpPrompts)

  /** Register a resource with its handler */
  def withResource(resource: Resource, handler: String => F[ResourceContent]): McpServerBuilder[F] =
    new McpServerBuilder(serverInfo, tools, contextTools, resources + (resource.uri -> (resource, handler)), resourceTemplates, templateHandlers, prompts, mcpTools, mcpResources, mcpPrompts)

  /** Register a resource template without a handler (for listing only) */
  def withResourceTemplate(template: ResourceTemplate): McpServerBuilder[F] =
    new McpServerBuilder(serverInfo, tools, contextTools, resources, resourceTemplates :+ template, templateHandlers, prompts, mcpTools, mcpResources, mcpPrompts)

  /** Register a resource template with a handler for reading */
  def withResourceTemplate(template: ResourceTemplate, handler: String => F[ResourceContent]): McpServerBuilder[F] =
    new McpServerBuilder(serverInfo, tools, contextTools, resources, resourceTemplates :+ template, templateHandlers + (template.uriTemplate -> handler), prompts, mcpTools, mcpResources, mcpPrompts)

  /** Register a prompt with its handler */
  def withPrompt(
      prompt: Prompt,
      handler: Map[String, String] => F[GetPromptResult]
  ): McpServerBuilder[F] =
    new McpServerBuilder(serverInfo, tools, contextTools, resources, resourceTemplates, templateHandlers, prompts + (prompt.name -> (prompt, handler)), mcpTools, mcpResources, mcpPrompts)

  /** Register tool routes (composable with <+>) */
  def withTools(newTools: McpTools[F]): McpServerBuilder[F] =
    val combined = mcpTools match
      case Some(existing) => Some(existing <+> newTools)
      case None           => Some(newTools)
    new McpServerBuilder(serverInfo, tools, contextTools, resources, resourceTemplates, templateHandlers, prompts, combined, mcpResources, mcpPrompts)

  /** Register composed McpResources (composable with <+>)
    *
    * Example:
    * {{{
    * import mcp4s.server.mcp.*
    *
    * val resources =
    *   Resource.text[IO]("test://readme", "README") { "Hello" } |+|
    *   Resource.template[IO]("test://users/{id}", "User") { uri => ... }
    *
    * builder.withResources(resources)
    * }}}
    */
  def withResources(newResources: McpResources[F]): McpServerBuilder[F] =
    val combined = mcpResources match
      case Some(existing) => Some(existing <+> newResources)
      case None           => Some(newResources)
    new McpServerBuilder(serverInfo, tools, contextTools, resources, resourceTemplates, templateHandlers, prompts, mcpTools, combined, mcpPrompts)

  /** Register composed McpPrompts (composable with <+>)
    *
    * Example:
    * {{{
    * import mcp4s.server.mcp.*
    *
    * val prompts =
    *   Prompt[IO]("greet", "Greet")(user("Hello!")) |+|
    *   Prompt.withDesc[IO]("help", "Help", "Get help")(user("How can I help?"))
    *
    * builder.withPrompts(prompts)
    * }}}
    */
  def withPrompts(newPrompts: McpPrompts[F]): McpServerBuilder[F] =
    val combined = mcpPrompts match
      case Some(existing) => Some(existing <+> newPrompts)
      case None           => Some(newPrompts)
    new McpServerBuilder(serverInfo, tools, contextTools, resources, resourceTemplates, templateHandlers, prompts, mcpTools, mcpResources, combined)

  // === Simplified DSL Methods ===

  /** Register a tool with name, description, schema, and handler */
  def tool(name: String, description: String, schema: JsonSchema)(
      handler: Json => F[ToolResult]
  ): McpServerBuilder[F] =
    withTool(Tool(name, Some(description), schema), handler)

  /** Register a tool with name, description, and handler (empty schema) */
  def tool(name: String, description: String)(
      handler: Json => F[ToolResult]
  ): McpServerBuilder[F] =
    withTool(Tool(name, Some(description), JsonSchema.empty), handler)

  /** Register a type-safe tool with derived schema and automatic argument decoding.
    *
    * Example:
    * {{{
    * case class AddArgs(a: Double, b: Double) derives ToolInput
    *
    * .tool[AddArgs]("add", "Add two numbers") { args =>
    *   IO.pure(ToolResult.text(s"${args.a + args.b}"))
    * }
    * }}}
    */
  def tool[A](name: String, description: String)(handler: A => F[ToolResult])(using
      ti: ToolInput[A]
  ): McpServerBuilder[F] =
    withTool(
      Tool(name, Some(description), ti.schema),
      json =>
        ti.decode(json) match
          case Right(a)  => handler(a)
          case Left(err) => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err))
    )

  // === Context-Aware Tool Methods (for sampling/progress) ===

  /** Register a context-aware tool with name, description, schema, and handler.
    *
    * Context-aware tools can use server-to-client operations like sampling.
    */
  def toolWithContext(name: String, description: String, schema: JsonSchema)(
      handler: (Json, ToolContext[F]) => F[ToolResult]
  ): McpServerBuilder[F] =
    withToolWithContext(Tool(name, Some(description), schema), handler)

  /** Register a context-aware tool with name, description, and handler (empty schema) */
  def toolWithContext(name: String, description: String)(
      handler: (Json, ToolContext[F]) => F[ToolResult]
  ): McpServerBuilder[F] =
    withToolWithContext(Tool(name, Some(description), JsonSchema.empty), handler)

  /** Register a type-safe context-aware tool with derived schema.
    *
    * Example:
    * {{{
    * case class SmartCalcArgs(query: String) derives ToolInput
    *
    * .toolWithContext[SmartCalcArgs]("smart-calc", "Calculate with LLM help") { (args, ctx) =>
    *   for
    *     result <- ctx.sampling.createMessage(CreateMessageParams(
    *       messages = List(SamplingMessage(Role.User, SamplingTextContent(args.query))),
    *       maxTokens = 100
    *     ))
    *   yield ToolResult.text(result.content match {
    *     case SamplingTextContent(text) => text
    *     case _ => "Unexpected response"
    *   })
    * }
    * }}}
    */
  def toolWithContext[A](name: String, description: String)(handler: (A, ToolContext[F]) => F[ToolResult])(using
      ti: ToolInput[A]
  ): McpServerBuilder[F] =
    withToolWithContext(
      Tool(name, Some(description), ti.schema),
      (json, ctx) =>
        ti.decode(json) match
          case Right(a)  => handler(a, ctx)
          case Left(err) => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err))
    )

  /** Register a simple text resource */
  def resource(uri: String, name: String)(content: => String): McpServerBuilder[F] =
    withResource(
      Resource(uri, name, mimeType = Some("text/plain")),
      _ => Concurrent[F].pure(ResourceContent.text(uri, content))
    )

  /** Register a resource with a handler */
  def resource(uri: String, name: String, mimeType: String)(
      handler: String => F[ResourceContent]
  ): McpServerBuilder[F] =
    withResource(Resource(uri, name, mimeType = Some(mimeType)), handler)

  /** Register a prompt with name, description, and handler */
  def prompt(name: String, description: String)(
      handler: Map[String, String] => F[GetPromptResult]
  ): McpServerBuilder[F] =
    withPrompt(Prompt(name, Some(description), Nil), handler)

  /** Register a prompt with name, description, arguments, and handler */
  def prompt(name: String, description: String, arguments: List[PromptArgument])(
      handler: Map[String, String] => F[GetPromptResult]
  ): McpServerBuilder[F] =
    withPrompt(Prompt(name, Some(description), arguments), handler)

  /** Register a type-safe prompt with derived arguments and automatic decoding.
    *
    * Example:
    * {{{
    * case class CalculateArgs(
    *   @description("Operation") operation: String,
    *   @description("First number") a: String
    * ) derives PromptInput
    *
    * .prompt[CalculateArgs]("calculate", "Perform calculation") { args =>
    *   IO.pure(GetPromptResult(...))
    * }
    * }}}
    */
  def prompt[A](name: String, description: String)(handler: A => F[GetPromptResult])(using
      pi: PromptInput[A]
  ): McpServerBuilder[F] =
    withPrompt(
      Prompt(name, Some(description), pi.arguments),
      args =>
        pi.decode(args) match
          case Right(a)  => handler(a)
          case Left(err) => Concurrent[F].raiseError(McpError.InvalidPromptArguments(name, err))
    )

  /** Build the server with computed capabilities */
  def build: McpServer[F] =
    val hasTools = tools.nonEmpty || contextTools.nonEmpty || mcpTools.isDefined
    val hasResources = resources.nonEmpty || resourceTemplates.nonEmpty || mcpResources.isDefined
    val hasPrompts = prompts.nonEmpty || mcpPrompts.isDefined
    val caps = ServerCapabilities(
      tools = if hasTools then Some(ToolsCapability()) else None,
      resources = if hasResources then Some(ResourcesCapability(subscribe = Some(true))) else None,
      prompts = if hasPrompts then Some(PromptsCapability()) else None,
      logging = Some(LoggingCapability()),
      completions = Some(CompletionsCapability())
    )
    new BuiltMcpServer[F](serverInfo, caps, tools, contextTools, resources, resourceTemplates, templateHandlers, prompts, mcpTools, mcpResources, mcpPrompts)

private final class BuiltMcpServer[F[_]: Concurrent](
    val info: ServerInfo,
    val capabilities: ServerCapabilities,
    private val tools: Map[String, (Tool, Json => F[ToolResult])],
    private val contextTools: Map[String, (Tool, (Json, ToolContext[F]) => F[ToolResult])],
    private val resources: Map[String, (Resource, String => F[ResourceContent])],
    private val resourceTemplates: List[ResourceTemplate],
    private val templateHandlers: Map[String, String => F[ResourceContent]],
    private val prompts: Map[String, (Prompt, Map[String, String] => F[GetPromptResult])],
    private val mcpTools: Option[McpTools[F]],
    private val mcpResources: Option[McpResources[F]],
    private val mcpPrompts: Option[McpPrompts[F]]
) extends McpServer[F]:

  def listTools: F[List[Tool]] =
    val builderTools = tools.values.map(_._1).toList
    val ctxTools = contextTools.values.map(_._1).toList
    val allBuilderTools = builderTools ++ ctxTools
    mcpTools match
      case Some(mt) =>
        mt.list.map { routeTools =>
          val builderNames = allBuilderTools.map(_.name).toSet
          allBuilderTools ++ routeTools.filterNot(t => builderNames.contains(t.name))
        }
      case None => Applicative[F].pure(allBuilderTools)

  def callTool(name: String, arguments: Json): F[ToolResult] =
    tools.get(name) match
      case Some((_, handler)) => handler(arguments)
      case None =>
        // Context tools called without context get an unsupported sampler
        contextTools.get(name) match
          case Some((_, handler)) =>
            val ctx = ToolContext.minimal[F](SamplingRequester.unsupported[F], RequestId.NullId)
            handler(arguments, ctx)
          case None =>
            mcpTools match
              case Some(mt) =>
                mt.call(name, arguments).getOrElseF(
                  Concurrent[F].raiseError(McpError.ToolNotFound(name))
                )
              case None =>
                Concurrent[F].raiseError(McpError.ToolNotFound(name))

  override def callToolWithContext(name: String, arguments: Json, context: ToolContext[F]): F[ToolResult] =
    // First check context tools, then regular tools, then McpTools
    contextTools.get(name) match
      case Some((_, handler)) => handler(arguments, context)
      case None =>
        tools.get(name) match
          case Some((_, handler)) => handler(arguments)  // Regular tools ignore context
          case None =>
            mcpTools match
              case Some(mt) =>
                // Use callWithContext to support both regular and context-aware tools
                mt.callWithContext(name, arguments, context).getOrElseF(
                  Concurrent[F].raiseError(McpError.ToolNotFound(name))
                )
              case None =>
                Concurrent[F].raiseError(McpError.ToolNotFound(name))

  def listResources: F[List[Resource]] =
    val builderResources = resources.values.map(_._1).toList
    mcpResources match
      case Some(mr) =>
        mr.list.map { routeResources =>
          val builderUris = builderResources.map(_.uri).toSet
          builderResources ++ routeResources.filterNot(r => builderUris.contains(r.uri))
        }
      case None => Applicative[F].pure(builderResources)

  def listResourceTemplates: F[List[ResourceTemplate]] =
    mcpResources match
      case Some(mr) =>
        mr.listTemplates.map { routeTemplates =>
          val builderUris = resourceTemplates.map(_.uriTemplate).toSet
          resourceTemplates ++ routeTemplates.filterNot(t => builderUris.contains(t.uriTemplate))
        }
      case None => Applicative[F].pure(resourceTemplates)

  def readResource(uri: String): F[ResourceContent] =
    resources.get(uri) match
      case Some((_, handler)) => handler(uri)
      case None =>
        // Try matching against template handlers
        findMatchingTemplateHandler(uri) match
          case Some(handler) => handler(uri)
          case None =>
            // Try McpResources
            mcpResources match
              case Some(mr) =>
                mr.read(uri).getOrElseF(
                  Concurrent[F].raiseError(McpError.ResourceNotFound(uri))
                )
              case None =>
                Concurrent[F].raiseError(McpError.ResourceNotFound(uri))

  private def findMatchingTemplateHandler(uri: String): Option[String => F[ResourceContent]] =
    templateHandlers.collectFirst {
      case (pattern, handler) if matchesTemplate(pattern, uri) => handler
    }

  private def matchesTemplate(template: String, uri: String): Boolean =
    // Convert URI template pattern like "test://template/{id}/data" to regex
    val regexPattern = template
      .replace(".", "\\.")
      .replace("/", "\\/")
      .replaceAll("\\{[^}]+\\}", "[^/]+")
    uri.matches(regexPattern)

  def listPrompts: F[List[Prompt]] =
    val builderPrompts = prompts.values.map(_._1).toList
    mcpPrompts match
      case Some(mp) =>
        mp.list.map { routePrompts =>
          val builderNames = builderPrompts.map(_.name).toSet
          builderPrompts ++ routePrompts.filterNot(p => builderNames.contains(p.name))
        }
      case None => Applicative[F].pure(builderPrompts)

  def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
    prompts.get(name) match
      case Some((_, handler)) => handler(arguments)
      case None =>
        // Try McpPrompts
        mcpPrompts match
          case Some(mp) =>
            mp.get(name, arguments).getOrElseF(
              Concurrent[F].raiseError(McpError.PromptNotFound(name))
            )
          case None =>
            Concurrent[F].raiseError(McpError.PromptNotFound(name))

object McpServerBuilder:
  def empty[F[_]: Concurrent]: McpServerBuilder[F] =
    new McpServerBuilder[F](
      ServerInfo("mcp4s", "0.1.0"),
      Map.empty,
      Map.empty,
      Map.empty,
      Nil,
      Map.empty,
      Map.empty,
      None,
      None,
      None
    )
