package mcp4s.server

import cats.Applicative
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream
import io.circe.Json
import mcp4s.protocol.*

/** Streaming tools that produce results incrementally.
  *
  * Streaming tools emit multiple `ToolResult` chunks over time, which are
  * delivered to the client via SSE events. This is useful for:
  * - Long-running operations with progress updates
  * - Tools that produce large outputs incrementally
  * - Real-time data streaming
  *
  * Streaming is a capability on `Tools[F]` — streaming tools compose with
  * regular tools via `|+|`. Use `callStreaming` to get the stream, falling
  * back to `call` for non-streaming tools.
  *
  * Example:
  * {{{
  * val streamingSearch = StreamingTool[IO, SearchArgs]("search", "Stream search results") { args =>
  *   searchService.streamResults(args.query).map { result =>
  *     ToolResult.text(s"Found: ${result.title}")
  *   }
  * }
  *
  * // Compose streaming and non-streaming tools together
  * val allTools = streamingSearch |+| regularTool
  * }}}
  */
object StreamingTool:

  /** Create a streaming tool from a stream-returning handler.
    *
    * The handler returns a Stream that emits ToolResult chunks.
    * Each chunk is sent to the client as a separate SSE event.
    * The final chunk signals completion.
    *
    * @param name Tool name
    * @param description Tool description
    * @param handler Function that takes typed args and returns a stream of results
    */
  def apply[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => Stream[F, ToolResult]
  ): Tools[F] =
    val ti = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    streaming(tool) { json =>
      ti.decode(json) match
        case Right(a)  => handler(a)
        case Left(err) => Stream.raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a streaming tool with context support.
    *
    * The handler receives both the arguments and a ToolContext for
    * server-to-client operations (sampling, progress, logging).
    */
  def withContext[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: (A, ToolContext[F]) => Stream[F, ToolResult]
  ): Tools[F] =
    val ti = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    streamingWithContext(tool) { (json, ctx) =>
      ti.decode(json) match
        case Right(a)  => handler(a, ctx)
        case Left(err) => Stream.raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a streaming tool with no arguments. */
  def noArgs[F[_]: Concurrent](name: String, description: String)(
      handler: Stream[F, ToolResult]
  ): Tools[F] =
    val tool = Tool(name, Some(description), JsonSchema.empty)
    streaming(tool)(_ => handler)

  /** Create a streaming tool from a regular tool handler that produces a single chunk.
    * This is useful for compatibility with existing tools.
    */
  def fromNonStreaming[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => F[ToolResult]
  ): Tools[F] =
    apply[F, A](name, description) { args =>
      Stream.eval(handler(args))
    }

  /** Create streaming tool routes from a single tool */
  private def streaming[F[_]: Concurrent](tool: Tool)(
      handler: Json => Stream[F, ToolResult]
  ): Tools[F] =
    new Tools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(List(tool))

      def call(name: String, args: Json): OptionT[F, ToolResult] =
        if name == tool.name then
          OptionT.liftF(handler(args).compile.lastOrError)
        else OptionT.none[F, ToolResult]

      override def callStreaming(name: String, args: Json): Option[Stream[F, ToolResult]] =
        if name == tool.name then Some(handler(args))
        else None

  /** Create context-aware streaming tool routes from a single tool */
  private def streamingWithContext[F[_]: Concurrent](tool: Tool)(
      handler: (Json, ToolContext[F]) => Stream[F, ToolResult]
  ): Tools[F] =
    new Tools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(List(tool))

      def call(name: String, args: Json): OptionT[F, ToolResult] =
        if name == tool.name then
          val ctx = ToolContext.minimal[F](SamplingRequester.unsupported[F], RequestId.NullId)
          OptionT.liftF(handler(args, ctx).compile.lastOrError)
        else OptionT.none[F, ToolResult]

      override def callWithContext(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
        if name == tool.name then OptionT.liftF(handler(args, ctx).compile.lastOrError)
        else OptionT.none[F, ToolResult]

      override def callStreaming(name: String, args: Json): Option[Stream[F, ToolResult]] =
        if name == tool.name then
          val ctx = ToolContext.minimal[F](SamplingRequester.unsupported[F], RequestId.NullId)
          Some(handler(args, ctx))
        else None

      override def callStreamingWithContext(name: String, args: Json, ctx: ToolContext[F]): Option[Stream[F, ToolResult]] =
        if name == tool.name then Some(handler(args, ctx))
        else None
