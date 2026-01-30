package mcp4s.server

import cats.Applicative
import cats.effect.Concurrent
import mcp4s.protocol.*

/** Unified DSL for MCP server construction.
  *
  * This object provides a single import (`import mcp4s.server.mcp.*`) that gives
  * access to all DSL constructors for building MCP servers with minimal boilerplate.
  *
  * Example usage:
  * {{{
  * import mcp4s.server.mcp.*
  *
  * val textTools =
  *   Tool.text[IO]("echo", "Echo text") {
  *     "Hello, world!"
  *   } |+|
  *   Tool[IO]("greet", "Greet someone") {
  *     IO.pure(ok("Hello!"))
  *   }
  *
  * val resources =
  *   Resource.text[IO]("test://readme", "README") {
  *     "This is the content."
  *   }
  *
  * val prompts =
  *   Prompt[IO]("greeting", "A greeting")(
  *     user("Hello!"),
  *     assistant("Hi there!")
  *   )
  *
  * val server = McpServer.from[IO](
  *   ServerInfo("my-server", "1.0.0"),
  *   textTools,
  *   resources,
  *   prompts
  * )
  * }}}
  */
object mcp:

  // === Result Builders ===

  /** Create a successful text tool result */
  def ok(text: String): ToolResult = ToolResult.text(text)

  /** Create an error tool result */
  def error(msg: String): ToolResult = ToolResult.error(msg)

  /** Create a tool result from content items */
  def content(items: Content*): ToolResult = ToolResult(items.toList)

  /** Create a text resource content */
  def text(uri: String, s: String): ResourceContent = ResourceContent.text(uri, s)

  /** Create a blob resource content */
  def blob(uri: String, data: String, mime: String): ResourceContent =
    ResourceContent.blob(uri, data, Some(mime))

  /** Create a prompt result from messages (no description) */
  def messages(msgs: PromptMessage*): GetPromptResult = GetPromptResult(None, msgs.toList)

  /** Create a prompt result from messages with description */
  def messages(desc: String)(msgs: PromptMessage*): GetPromptResult =
    GetPromptResult(Some(desc), msgs.toList)

  // === Content Builders ===

  /** Create text content */
  def textContent(s: String): TextContent = TextContent(s)

  /** Create image content from base64 data */
  def imageContent(data: String, mime: String): ImageContent = ImageContent(data, mime)

  /** Create audio content from base64 data */
  def audioContent(data: String, mime: String): AudioContent = AudioContent(data, mime)

  // === Message Builders ===

  /** Create a user message with text content */
  def user(text: String): PromptMessage = PromptMessage(Role.User, TextContent(text))

  /** Create a user message with any content */
  def user(content: Content): PromptMessage = PromptMessage(Role.User, content)

  /** Create an assistant message with text content */
  def assistant(text: String): PromptMessage = PromptMessage(Role.Assistant, TextContent(text))

  /** Create an assistant message with any content */
  def assistant(content: Content): PromptMessage = PromptMessage(Role.Assistant, content)

  // === Tool Constructors ===

  /** Namespaced tool constructors */
  object Tool:

    /** Create a tool with a pure string handler (no arguments).
      *
      * Example:
      * {{{
      * val version = Tool.text[IO]("version", "Get version") {
      *   "1.0.0"
      * }
      * }}}
      */
    def text[F[_]: Concurrent](name: String, desc: String)(f: => String): McpTools[F] =
      McpTool.pureTextNoArgs[F](name, desc)(f)

    /** Create a tool with a pure string handler with typed arguments.
      *
      * Example:
      * {{{
      * case class EchoArgs(message: String) derives ToolInput
      * val echo = Tool.text[IO, EchoArgs]("echo", "Echo message") { args =>
      *   args.message
      * }
      * }}}
      */
    def text[F[_]: Concurrent, A: ToolInput](name: String, desc: String)(f: A => String): McpTools[F] =
      McpTool.pureText[F, A](name, desc)(f)

    /** Create a tool with an effectful handler (no arguments).
      *
      * Example:
      * {{{
      * val time = Tool[IO]("time", "Get current time") {
      *   IO.realTimeInstant.map(t => ok(t.toString))
      * }
      * }}}
      */
    def apply[F[_]: Concurrent](name: String, desc: String)(f: => F[ToolResult]): McpTools[F] =
      McpTool.noArgs[F](name, desc)(f)

    /** Create a tool with an effectful handler with typed arguments.
      *
      * Example:
      * {{{
      * case class AddArgs(a: Double, b: Double) derives ToolInput
      * val add = Tool[IO, AddArgs]("add", "Add numbers") { args =>
      *   IO.pure(ok(s"${args.a + args.b}"))
      * }
      * }}}
      */
    def apply[F[_]: Concurrent, A: ToolInput](name: String, desc: String)(
        f: A => F[ToolResult]
    ): McpTools[F] =
      McpTool[F, A](name, desc)(f)

    /** Create a context-aware tool with no typed arguments.
      *
      * Context-aware tools can access server-to-client operations like sampling,
      * progress notifications, and logging via the ToolContext.
      *
      * Example:
      * {{{
      * val ping = Tool.withContext[IO]("ping", "Ping with logging") { ctx =>
      *   for
      *     _ <- ctx.log(LogLevel.Info, "Ping received")
      *   yield ok("pong")
      * }
      * }}}
      */
    def withContext[F[_]: Concurrent](name: String, desc: String)(
        f: ToolContext[F] => F[ToolResult]
    ): McpTools[F] =
      McpTool.withContextNoArgs[F](name, desc)(f)

    /** Create a context-aware tool with typed arguments.
      *
      * Example:
      * {{{
      * case class QueryArgs(query: String) derives ToolInput
      * val smart = Tool.withContext[IO, QueryArgs]("smart", "Smart query") { (args, ctx) =>
      *   for
      *     _ <- ctx.log(LogLevel.Info, s"Processing: ${args.query}")
      *     result <- ctx.sampling.createMessage(...)
      *   yield ok(result.content.toString)
      * }
      * }}}
      */
    def withContext[F[_]: Concurrent, A: ToolInput](name: String, desc: String)(
        f: (A, ToolContext[F]) => F[ToolResult]
    ): McpTools[F] =
      McpTool.withContext[F, A](name, desc)(f)

  // === Resource Constructors ===

  /** Namespaced resource constructors */
  object Resource:

    /** Create a static text resource.
      *
      * Example:
      * {{{
      * val readme = Resource.text[IO]("file:///readme", "README") {
      *   "Hello world"
      * }
      * }}}
      */
    def text[F[_]: Concurrent](uri: String, name: String)(content: => String): McpResources[F] =
      McpResource[F](uri, name)(content)

    /** Create a resource with an effectful handler.
      *
      * Example:
      * {{{
      * val config = Resource[IO]("file:///config", "Config") {
      *   loadConfig().map(c => mcp.text("file:///config", c.toString))
      * }
      * }}}
      */
    def apply[F[_]: Concurrent](uri: String, name: String)(
        f: => F[ResourceContent]
    ): McpResources[F] =
      McpResource.handler[F](uri, name)(_ => f)

    /** Create a template resource that matches URI patterns.
      *
      * Example:
      * {{{
      * val users = Resource.template[IO]("api://users/{id}", "User", "Get user by ID") { uri =>
      *   val id = extractId(uri)
      *   IO.pure(mcp.text(uri, s"""{"id":"$id"}"""))
      * }
      * }}}
      */
    def template[F[_]: Concurrent](pattern: String, name: String, description: String = "")(
        handler: String => F[ResourceContent]
    ): McpResources[F] =
      McpResources.template[F](pattern, name, description)(handler)

  // === Prompt Constructors ===

  /** Namespaced prompt constructors */
  object Prompt:

    /** Create a prompt with no arguments.
      *
      * Example:
      * {{{
      * val greeting = Prompt[IO]("greet", "A greeting")(
      *   user("Hello!"),
      *   assistant("Hi there!")
      * )
      * }}}
      */
    def apply[F[_]: Concurrent](name: String, desc: String)(
        msgs: PromptMessage*
    ): McpPrompts[F] =
      McpPrompt.noArgs[F](name, desc)(
        Concurrent[F].pure(GetPromptResult(None, msgs.toList))
      )

    /** Create a prompt with a description.
      *
      * Example:
      * {{{
      * val greeting = Prompt.withDesc[IO]("greet", "A greeting", "Says hello")(
      *   user("Hello!"),
      *   assistant("Hi there!")
      * )
      * }}}
      */
    def withDesc[F[_]: Concurrent](name: String, desc: String, promptDesc: String)(
        msgs: PromptMessage*
    ): McpPrompts[F] =
      McpPrompt.noArgs[F](name, desc)(
        Concurrent[F].pure(GetPromptResult(Some(promptDesc), msgs.toList))
      )

    /** Create a prompt with typed arguments.
      *
      * Example:
      * {{{
      * case class GreetArgs(name: String) derives PromptInput
      * val greeting = Prompt[IO, GreetArgs]("greet", "A greeting") { args =>
      *   IO.pure(messages(user(s"Hello, ${args.name}!")))
      * }
      * }}}
      */
    def apply[F[_]: Concurrent, A: PromptInput](name: String, desc: String)(
        f: A => F[GetPromptResult]
    ): McpPrompts[F] =
      McpPrompt[F, A](name, desc)(f)

  // === Pure Lifting Extension ===

  /** Extension to lift pure values into any Applicative context.
    *
    * Example:
    * {{{
    * val result: IO[ToolResult] = ok("Success").pure[IO]
    * }}}
    */
  extension [A](a: A)
    def pure[F[_]: Applicative]: F[A] = Applicative[F].pure(a)
