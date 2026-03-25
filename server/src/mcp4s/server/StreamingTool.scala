package mcp4s.server

import cats.Applicative
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
  * Example:
  * {{{
  * val streamingSearch = StreamingTool[IO, SearchArgs]("search", "Stream search results") { args =>
  *   searchService.streamResults(args.query).map { result =>
  *     ToolResult.text(s"Found: ${result.title}")
  *   }
  * }
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
  ): StreamingTools[F] =
    val ti = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    StreamingTools.single(tool) { json =>
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
  ): StreamingTools[F] =
    val ti = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    StreamingTools.singleWithContext(tool) { (json, ctx) =>
      ti.decode(json) match
        case Right(a)  => handler(a, ctx)
        case Left(err) => Stream.raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a streaming tool with no arguments. */
  def noArgs[F[_]: Concurrent](name: String, description: String)(
      handler: Stream[F, ToolResult]
  ): StreamingTools[F] =
    val tool = Tool(name, Some(description), JsonSchema.empty)
    StreamingTools.single(tool)(_ => handler)

  /** Create a streaming tool from a regular tool handler that produces a single chunk.
    * This is useful for compatibility with existing tools.
    */
  def fromNonStreaming[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => F[ToolResult]
  ): StreamingTools[F] =
    apply[F, A](name, description) { args =>
      Stream.eval(handler(args))
    }

/** Composable streaming tool routes for MCP servers.
  *
  * Similar to Tools but produces streams of results instead of single results.
  */
trait StreamingTools[F[_]]:
  /** List all streaming tools */
  def list: F[List[Tool]]

  /** Call a streaming tool, returning None if not handled */
  def callStreaming(name: String, args: Json): Option[Stream[F, ToolResult]]

  /** Call a streaming tool with context */
  def callStreamingWithContext(name: String, args: Json, ctx: ToolContext[F]): Option[Stream[F, ToolResult]] =
    callStreaming(name, args)

object StreamingTools:

  /** Empty streaming tools */
  def empty[F[_]: Applicative]: StreamingTools[F] =
    new StreamingTools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(Nil)
      def callStreaming(name: String, args: Json): Option[Stream[F, ToolResult]] = None

  /** Create streaming tool routes from a single tool */
  def single[F[_]: Concurrent](tool: Tool)(
      handler: Json => Stream[F, ToolResult]
  ): StreamingTools[F] =
    new StreamingTools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(List(tool))
      def callStreaming(name: String, args: Json): Option[Stream[F, ToolResult]] =
        if name == tool.name then Some(handler(args))
        else None

  /** Create context-aware streaming tool routes from a single tool */
  def singleWithContext[F[_]: Concurrent](tool: Tool)(
      handler: (Json, ToolContext[F]) => Stream[F, ToolResult]
  ): StreamingTools[F] =
    new StreamingTools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(List(tool))
      def callStreaming(name: String, args: Json): Option[Stream[F, ToolResult]] =
        if name == tool.name then
          val ctx = ToolContext.minimal[F](SamplingRequester.unsupported[F], RequestId.NullId)
          Some(handler(args, ctx))
        else None

      override def callStreamingWithContext(name: String, args: Json, ctx: ToolContext[F]): Option[Stream[F, ToolResult]] =
        if name == tool.name then Some(handler(args, ctx))
        else None

  /** Combine two StreamingTools instances (first match wins) */
  def combine[F[_]: Concurrent](x: StreamingTools[F], y: StreamingTools[F]): StreamingTools[F] =
    new StreamingTools[F]:
      def list: F[List[Tool]] =
        for
          xTools <- x.list
          yTools <- y.list
          xNames = xTools.map(_.name).toSet
        yield xTools ++ yTools.filterNot(t => xNames.contains(t.name))

      def callStreaming(name: String, args: Json): Option[Stream[F, ToolResult]] =
        x.callStreaming(name, args).orElse(y.callStreaming(name, args))

      override def callStreamingWithContext(name: String, args: Json, ctx: ToolContext[F]): Option[Stream[F, ToolResult]] =
        x.callStreamingWithContext(name, args, ctx).orElse(y.callStreamingWithContext(name, args, ctx))

  /** Semigroup instance for composition via |+| */
  given [F[_]: Concurrent]: cats.Semigroup[StreamingTools[F]] with
    def combine(x: StreamingTools[F], y: StreamingTools[F]): StreamingTools[F] =
      StreamingTools.combine(x, y)

  extension [F[_]: Concurrent](tools: StreamingTools[F])
    def <+>(other: StreamingTools[F]): StreamingTools[F] =
      combine(tools, other)

/** Extension methods for converting regular tools to streaming tools */
extension [F[_]: Concurrent](tools: Tools[F])

  /** Convert regular tools to streaming tools.
    * Each tool call produces a stream with a single result.
    */
  def asStreaming: StreamingTools[F] =
    new StreamingTools[F]:
      def list: F[List[Tool]] = tools.list

      def callStreaming(name: String, args: Json): Option[Stream[F, ToolResult]] =
        // We can't know if the tool handles this name without running F,
        // so we return Some and let it fail in the stream if not found
        Some(Stream.eval(tools.call(name, args).getOrElseF(
          Concurrent[F].raiseError(McpError.ToolNotFound(name))
        )))

      override def callStreamingWithContext(name: String, args: Json, ctx: ToolContext[F]): Option[Stream[F, ToolResult]] =
        Some(Stream.eval(tools.callWithContext(name, args, ctx).getOrElseF(
          Concurrent[F].raiseError(McpError.ToolNotFound(name))
        )))
