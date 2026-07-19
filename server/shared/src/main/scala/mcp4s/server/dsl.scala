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

import cats.Applicative
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*
import mcp4s.schema.{PromptCodec, PromptEndpoint, ToolEndpoint}

/** The MCP server DSL: one import gives you everything needed to define tools, prompts, and
  * resources and attach handlers.
  *
  * Tools and prompts are defined as endpoint values ([[mcp4s.schema.ToolEndpoint]] /
  * [[mcp4s.schema.PromptEndpoint]]) and become routes by attaching one of exactly four handler
  * shapes: `handle` (effectful), `handleWith` (effectful + [[ToolContext]]), `stream`, or
  * `streamWith`.
  *
  * {{{
  * import mcp4s.server.dsl.*
  *
  * @description("Greet someone")
  * case class Greet(@description("Who to greet") name: String) derives Schema
  *
  * val tools =
  *   Tool.from[Greet].handle[IO] { args => IO.pure(ok(s"Hello, $${args.name}!")) } |+|
  *   Tool("version").withDescription("Get version").handle[IO](_ => IO.pure(ok("1.0.0")))
  *
  * val prompts =
  *   Prompt("greeting").withDescription("A greeting").messages[IO](user("Hello!"))
  *
  * val server = Server.from[IO](ServerInfo("my-server", "1.0.0"), tools, Resources.empty, prompts)
  * }}}
  */
object dsl:

  export mcp4s.schema.{Prompt, PromptEndpoint, Schema, Tool, ToolEndpoint}
  export mcp4s.protocol.description

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

  // === Tool Handler Attachment ===

  extension [I, O](e: ToolEndpoint[I, O])

    /** Attach an effectful handler. */
    def handle[F[_]: Concurrent](f: I => F[O]): Tools[F] =
      Tools.single(e.toTool) { json =>
        decodeInput(e, json).flatMap(i => f(i).map(e.outputEncoder.encode))
      }

    /** Attach an effectful handler that also receives the [[ToolContext]] (sampling,
      * elicitation, progress, logging).
      */
    def handleWith[F[_]: Concurrent](f: (I, ToolContext[F]) => F[O]): Tools[F] =
      Tools.singleWithContext(e.toTool) { (json, ctx) =>
        decodeInput(e, json).flatMap(i => f(i, ctx).map(e.outputEncoder.encode))
      }

    /** Attach a streaming handler. On the plain request/response call path the last emitted
      * value is the tool result.
      */
    def stream[F[_]: Concurrent](f: I => fs2.Stream[F, O]): Tools[F] =
      Tools.single(e.toTool) { json =>
        decodeInput(e, json).flatMap(i => f(i).map(e.outputEncoder.encode).compile.lastOrError)
      }

    /** Attach a streaming handler that also receives the [[ToolContext]]. */
    def streamWith[F[_]: Concurrent](f: (I, ToolContext[F]) => fs2.Stream[F, O]): Tools[F] =
      Tools.singleWithContext(e.toTool) { (json, ctx) =>
        decodeInput(e, json).flatMap(i => f(i, ctx).map(e.outputEncoder.encode).compile.lastOrError)
      }

  private def decodeInput[F[_]: Concurrent, I, O](e: ToolEndpoint[I, O], json: Json): F[I] =
    e.inputSchema.decoder.decodeJson(json) match
      case Right(i)  => i.pure[F]
      case Left(err) => McpError.InvalidToolArguments(e.name, err.getMessage).raiseError[F, I]

  // === Prompt Handler Attachment ===

  extension [I](e: PromptEndpoint[I])

    /** Attach an effectful handler receiving the decoded prompt input. */
    def handle[F[_]: Concurrent](f: I => F[GetPromptResult]): Prompts[F] =
      val prompt = e.toPrompt
      new Prompts[F]:
        def list: F[List[Prompt]] = Applicative[F].pure(List(prompt))
        def get(name: String, args: Map[String, String]): OptionT[F, GetPromptResult] =
          if name == e.name then
            PromptCodec.decode(e.inputSchema, args) match
              case Right(i) => OptionT.liftF(f(i))
              case Left(err) =>
                OptionT.liftF(
                  Concurrent[F].raiseError(McpError.InvalidPromptArguments(e.name, err))
                )
          else OptionT.none[F, GetPromptResult]

  extension (e: PromptEndpoint[Unit])

    /** A prompt that always returns the same messages. */
    def messages[F[_]: Concurrent](msgs: PromptMessage*): Prompts[F] =
      e.handle[F](_ => Concurrent[F].pure(GetPromptResult(None, msgs.toList)))

    /** A prompt that always returns the given result. */
    def static[F[_]: Concurrent](result: GetPromptResult): Prompts[F] =
      e.handle[F](_ => Concurrent[F].pure(result))

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

    /** Create a resource with an effectful handler. */
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
      *   IO.pure(text(uri, s"""{"id":"$id"}"""))
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

    /** Create a subscribable resource that emits change notifications. */
    def subscribable[F[_]: Concurrent](
        uri: String,
        name: String,
        changeStream: fs2.Stream[F, Unit]
    )(
        handler: String => F[ResourceContent]
    ): Resources[F] =
      McpResource.subscribable[F](uri, name, changeStream)(handler)

    /** Create a subscribable resource that polls for changes. */
    def polling[F[_]: cats.effect.Temporal](
        uri: String,
        name: String,
        pollInterval: scala.concurrent.duration.FiniteDuration,
        hasChanged: F[Boolean]
    )(handler: String => F[ResourceContent]): Resources[F] =
      McpResource.polling[F](uri, name, pollInterval, hasChanged)(handler)
