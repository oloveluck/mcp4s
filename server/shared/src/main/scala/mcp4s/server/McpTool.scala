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
import io.circe.Json
import mcp4s.protocol.*

/** Internal tool factory. Use `Tool` from `import mcp4s.server.mcp.*` instead.
  *
  * {{{
  * import mcp4s.server.mcp.*
  *
  * case class SearchArgs(query: String, limit: Option[Int]) derives ToolInput
  * val search = Tool[IO, SearchArgs]("search", "Search") { args =>
  *   IO.pure(ToolResult.text(s"Searching: ${args.query}"))
  * }
  * }}}
  */
private[server] object McpTool:

  /** Create a tool from derived ToolInput */
  def apply[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => F[ToolResult]
  ): Tools[F] =
    val ti   = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    Tools.single(tool) { json =>
      ti.decode(json) match
        case Right(a)  => handler(a)
        case Left(err) => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a tool with typed output */
  def typed[F[_]: Concurrent, A: ToolInput, B](name: String, description: String)(
      handler: A => F[B]
  )(using to: ToolOutput[B]): Tools[F] =
    val ti   = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema, outputSchema = Some(to.schema))
    Tools.single(tool) { json =>
      ti.decode(json) match
        case Right(a) =>
          Concurrent[F].map(handler(a))(to.encode)
        case Left(err) =>
          Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a context-aware tool.
    *
    * Context-aware tools can access server-to-client operations like sampling, progress
    * notifications, and logging via the ToolContext.
    *
    * These tools can be composed with regular tools using `|+|`.
    */
  def withContext[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: (A, ToolContext[F]) => F[ToolResult]
  ): Tools[F] =
    val ti   = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    Tools.singleWithContext(tool) { (json, ctx) =>
      ti.decode(json) match
        case Right(a)  => handler(a, ctx)
        case Left(err) => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a context-aware tool with no typed arguments.
    *
    * Context-aware tools can access server-to-client operations like sampling, progress
    * notifications, and logging via the ToolContext.
    *
    * These tools can be composed with regular tools using `|+|`.
    */
  def withContextNoArgs[F[_]: Concurrent](name: String, description: String)(
      handler: ToolContext[F] => F[ToolResult]
  ): Tools[F] =
    val tool = Tool(name, Some(description), JsonSchema.empty)
    Tools.singleWithContext(tool)((_, ctx) => handler(ctx))

  /** Create a tool with annotations */
  def annotated[F[_]: Concurrent, A: ToolInput](
      name: String,
      description: String,
      annotations: ToolAnnotations
  )(handler: A => F[ToolResult]): Tools[F] =
    val ti   = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema, annotations = Some(annotations))
    Tools.single(tool) { json =>
      ti.decode(json) match
        case Right(a)  => handler(a)
        case Left(err) => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a no-argument tool */
  def noArgs[F[_]: Concurrent](name: String, description: String)(
      handler: F[ToolResult]
  ): Tools[F] =
    val tool = Tool(name, Some(description), JsonSchema.empty)
    Tools.single(tool)(_ => handler)

  // === Pure Result Helpers ===

  /** Create a tool with a pure string handler (auto-wrapped in F[ToolResult]).
    *
    * Example:
    * {{{
    * val echo = Tool.text[IO, EchoArgs]("echo", "Echo input") { args =>
    *   args.message
    * }
    * }}}
    */
  def pureText[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => String
  ): Tools[F] =
    apply[F, A](name, description)(a => Concurrent[F].pure(ToolResult.text(handler(a))))

  /** Create a no-argument tool with a pure string result.
    *
    * Example:
    * {{{
    * val version = Tool.text[IO]("version", "Get version") {
    *   "1.0.0"
    * }
    * }}}
    */
  def pureTextNoArgs[F[_]: Concurrent](name: String, description: String)(
      result: => String
  ): Tools[F] =
    noArgs[F](name, description)(Concurrent[F].pure(ToolResult.text(result)))

  // === Streaming Constructors ===

  /** Create a streaming tool from a stream-returning handler.
    *
    * The handler returns a Stream that emits ToolResult chunks. The final result is produced by
    * compiling the stream (last emitted value).
    */
  def streaming[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => fs2.Stream[F, ToolResult]
  ): Tools[F] =
    val ti   = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    new Tools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(List(tool))

      def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
        if name == tool.name then
          OptionT.liftF {
            ti.decode(args) match
              case Right(a) => handler(a).compile.lastOrError
              case Left(err) =>
                Concurrent[F].raiseError(McpError.InvalidToolArguments(tool.name, err))
          }
        else OptionT.none[F, ToolResult]

  /** Create a streaming tool with context support. */
  def streamingWithContext[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: (A, ToolContext[F]) => fs2.Stream[F, ToolResult]
  ): Tools[F] =
    val ti   = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    new Tools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(List(tool))

      def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
        if name == tool.name then
          OptionT.liftF {
            ti.decode(args) match
              case Right(a) => handler(a, ctx).compile.lastOrError
              case Left(err) =>
                Concurrent[F].raiseError(McpError.InvalidToolArguments(tool.name, err))
          }
        else OptionT.none[F, ToolResult]

  /** Create a streaming tool with no arguments. */
  def streamingNoArgs[F[_]: Concurrent](name: String, description: String)(
      handler: fs2.Stream[F, ToolResult]
  ): Tools[F] =
    val tool = Tool(name, Some(description), JsonSchema.empty)
    new Tools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(List(tool))

      def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
        if name == tool.name then OptionT.liftF(handler.compile.lastOrError)
        else OptionT.none[F, ToolResult]
