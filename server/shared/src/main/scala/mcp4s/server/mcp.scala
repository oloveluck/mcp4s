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

import cats.effect.Concurrent
import mcp4s.protocol.*

/** Unified DSL for MCP server construction.
  *
  * This object provides a single import (`import mcp4s.server.mcp.*`) that gives access to all DSL
  * constructors for building MCP servers with minimal boilerplate.
  *
  * Example usage:
  * {{{
  * import mcp4s.server.mcp.*
  *
  * case class GreetArgs(name: String) derives ToolInput
  *
  * val textTools =
  *   Tool.text[IO]("echo", "Echo text") {
  *     "Hello, world!"
  *   } |+|
  *   Tool[IO, GreetArgs]("greet", "Greet someone") { args =>
  *     IO.pure(ok(s"Hello, $${args.name}!"))
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
  * val server = Server.from[IO](
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
    def text[F[_]: Concurrent](name: String, desc: String)(f: => String): Tools[F] =
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
    def text[F[_]: Concurrent, A: ToolInput](name: String, desc: String)(f: A => String): Tools[F] =
      McpTool.pureText[F, A](name, desc)(f)

    /** Create a text tool — name + description derived from args type. */
    inline def text[F[_]: Concurrent, A: ToolInput](
        handler: A => String
    ): Tools[F] =
      val name = ToolInput.deriveName(ToolInput.typeName[A])
      val desc = ToolInput.classDescription[A].getOrElse("")
      McpTool.pureText[F, A](name, desc)(handler)

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
    ): Tools[F] =
      McpTool[F, A](name, desc)(f)

    /** Create a tool — name + description derived from the args type. Name: class name converted to
      * snake_case (common suffixes stripped). Description: class-level @description annotation
      * (empty string if absent).
      */
    inline def apply[F[_]: Concurrent, A: ToolInput](
        handler: A => F[ToolResult]
    ): Tools[F] =
      val name = ToolInput.deriveName(ToolInput.typeName[A])
      val desc = ToolInput.classDescription[A].getOrElse("")
      McpTool[F, A](name, desc)(handler)

    /** Create a tool with a custom name — description derived from the args type. */
    inline def apply[F[_]: Concurrent, A: ToolInput](name: String)(
        handler: A => F[ToolResult]
    ): Tools[F] =
      val desc = ToolInput.classDescription[A].getOrElse("")
      McpTool[F, A](name, desc)(handler)

    /** Create a context-aware tool with no typed arguments.
      *
      * Context-aware tools can access server-to-client operations like sampling, progress
      * notifications, and logging via the ToolContext.
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
    ): Tools[F] =
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
    ): Tools[F] =
      McpTool.withContext[F, A](name, desc)(f)

    /** Create a context-aware tool — name + description derived from args type. */
    inline def withContext[F[_]: Concurrent, A: ToolInput](
        handler: (A, ToolContext[F]) => F[ToolResult]
    ): Tools[F] =
      val name = ToolInput.deriveName(ToolInput.typeName[A])
      val desc = ToolInput.classDescription[A].getOrElse("")
      McpTool.withContext[F, A](name, desc)(handler)

    /** Create a context-aware tool with a custom name — description derived from args type. */
    inline def withContext[F[_]: Concurrent, A: ToolInput](name: String)(
        handler: (A, ToolContext[F]) => F[ToolResult]
    ): Tools[F] =
      val desc = ToolInput.classDescription[A].getOrElse("")
      McpTool.withContext[F, A](name, desc)(handler)

    /** Create a streaming tool with typed arguments.
      *
      * Streaming tools emit multiple results over time. When called via `call`, the last emitted
      * result is returned.
      *
      * Example:
      * {{{
      * case class SearchArgs(query: String) derives ToolInput
      * val search = Tool.stream[IO, SearchArgs]("search", "Stream results") { args =>
      *   searchService.results(args.query).map(r => ok(r.toString))
      * }
      * }}}
      */
    def stream[F[_]: Concurrent, A: ToolInput](name: String, desc: String)(
        f: A => fs2.Stream[F, ToolResult]
    ): Tools[F] =
      McpTool.stream[F, A](name, desc)(f)

    /** Create a streaming tool with no arguments.
      *
      * Example:
      * {{{
      * val ticks = Tool.stream[IO]("tick", "Emit ticks") {
      *   Stream.emits(List(ok("tick 1"), ok("tick 2")))
      * }
      * }}}
      */
    def stream[F[_]: Concurrent](name: String, desc: String)(
        f: fs2.Stream[F, ToolResult]
    ): Tools[F] =
      McpTool.streamNoArgs[F](name, desc)(f)

    /** Create a streaming tool with typed arguments and context support.
      *
      * The handler additionally receives the [[ToolContext]] for progress reporting, logging,
      * sampling, and elicitation while it streams.
      *
      * Example:
      * {{{
      * case class SearchArgs(query: String) derives ToolInput
      * val search = Tool.streamWithContext[IO, SearchArgs]("search", "Search with progress") {
      *   (args, ctx) =>
      *     searchService.results(args.query).evalMap(r => ctx.progress(1, None).as(ok(r.toString)))
      * }
      * }}}
      */
    def streamWithContext[F[_]: Concurrent, A: ToolInput](name: String, desc: String)(
        f: (A, ToolContext[F]) => fs2.Stream[F, ToolResult]
    ): Tools[F] =
      McpTool.streamWithContext[F, A](name, desc)(f)

    /** Create a streaming tool with no arguments and context support. */
    def streamWithContext[F[_]: Concurrent](name: String, desc: String)(
        f: ToolContext[F] => fs2.Stream[F, ToolResult]
    ): Tools[F] =
      McpTool.streamWithContextNoArgs[F](name, desc)(f)

    /** Create a tool with typed output. */
    def typed[F[_]: Concurrent, A: ToolInput, B](name: String, desc: String)(
        handler: A => F[B]
    )(using ToolOutput[B]): Tools[F] =
      McpTool.typed[F, A, B](name, desc)(handler)

    /** Create a tool with annotations. */
    def annotated[F[_]: Concurrent, A: ToolInput](
        name: String,
        desc: String,
        annotations: ToolAnnotations
    )(handler: A => F[ToolResult]): Tools[F] =
      McpTool.annotated[F, A](name, desc, annotations)(handler)

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
    def text[F[_]: Concurrent](uri: String, name: String)(content: => String): Resources[F] =
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
    ): Resources[F] =
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
    ): Resources[F] =
      Resources.template[F](pattern, name, description)(handler)

    /** Create a resource with a handler function. */
    def handler[F[_]: Concurrent](uri: String, name: String, mimeType: String = "text/plain")(
        handler: String => F[ResourceContent]
    ): Resources[F] =
      McpResource.handler[F](uri, name, mimeType)(handler)

    /** Create a resource from a Resource definition and handler. */
    def single[F[_]: Concurrent](resource: mcp4s.protocol.Resource)(
        handler: String => F[ResourceContent]
    ): Resources[F] =
      McpResource.single[F](resource)(handler)

    /** Create a subscribable resource that emits change notifications.
      *
      * Example:
      * {{{
      * val config = Resource.subscribable[IO](
      *   "file:///config", "Config",
      *   fileWatcher.events.void
      * ) { _ => loadConfig().map(mcp.text("file:///config", _)) }
      * }}}
      */
    def subscribable[F[_]: Concurrent](
        uri: String,
        name: String,
        changeStream: fs2.Stream[F, Unit]
    )(
        handler: String => F[ResourceContent]
    ): Resources[F] =
      McpResource.subscribable[F](uri, name, changeStream)(handler)

    /** Create a subscribable resource that polls for changes.
      *
      * Example:
      * {{{
      * val metrics = Resource.polling[IO](
      *   "metrics://cpu", "CPU",
      *   5.seconds, checkChanged
      * ) { _ => getMetrics.map(mcp.text("metrics://cpu", _)) }
      * }}}
      */
    def polling[F[_]: cats.effect.Temporal](
        uri: String,
        name: String,
        pollInterval: scala.concurrent.duration.FiniteDuration,
        hasChanged: F[Boolean]
    )(handler: String => F[ResourceContent]): Resources[F] =
      McpResource.polling[F](uri, name, pollInterval, hasChanged)(handler)

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
    ): Prompts[F] =
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
    ): Prompts[F] =
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
    ): Prompts[F] =
      McpPrompt[F, A](name, desc)(f)

    /** Create a prompt — name + description derived from args type. */
    inline def apply[F[_]: Concurrent, A: PromptInput](
        handler: A => F[GetPromptResult]
    ): Prompts[F] =
      val name = ToolInput.deriveName(ToolInput.typeName[A])
      val desc = ToolInput.classDescription[A].getOrElse("")
      McpPrompt[F, A](name, desc)(handler)

    /** Create a prompt with a custom name — description derived from args type. */
    inline def apply[F[_]: Concurrent, A: PromptInput](name: String)(
        handler: A => F[GetPromptResult]
    ): Prompts[F] =
      val desc = ToolInput.classDescription[A].getOrElse("")
      McpPrompt[F, A](name, desc)(handler)

    /** Create a prompt from a raw Prompt definition and a map-based handler. */
    def single[F[_]: Concurrent](prompt: mcp4s.protocol.Prompt)(
        handler: Map[String, String] => F[GetPromptResult]
    ): Prompts[F] =
      Prompts.single[F](prompt)(handler)
