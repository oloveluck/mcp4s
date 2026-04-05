package mcp4s.server

import cats.Applicative
import cats.data.OptionT
import cats.effect.Concurrent
import fs2.Stream
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
    val ti = summon[ToolInput[A]]
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
    val ti = summon[ToolInput[A]]
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
    * Context-aware tools can access server-to-client operations like sampling,
    * progress notifications, and logging via the ToolContext.
    *
    * These tools can be composed with regular tools using `|+|`.
    */
  def withContext[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: (A, ToolContext[F]) => F[ToolResult]
  ): Tools[F] =
    val ti = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    Tools.singleWithContext(tool) { (json, ctx) =>
      ti.decode(json) match
        case Right(a)  => handler(a, ctx)
        case Left(err) => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a context-aware tool with no typed arguments.
    *
    * Context-aware tools can access server-to-client operations like sampling,
    * progress notifications, and logging via the ToolContext.
    *
    * These tools can be composed with regular tools using `|+|`.
    */
  def withContextNoArgs[F[_]: Concurrent](name: String, description: String)(
      handler: ToolContext[F] => F[ToolResult]
  ): Tools[F] =
    val tool = Tool(name, Some(description), JsonSchema.empty)
    Tools.singleWithContext(tool) { (_, ctx) => handler(ctx) }

  /** Create a tool with annotations */
  def annotated[F[_]: Concurrent, A: ToolInput](
      name: String,
      description: String,
      annotations: ToolAnnotations
  )(handler: A => F[ToolResult]): Tools[F] =
    val ti = summon[ToolInput[A]]
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
    * The handler returns a Stream that emits ToolResult chunks.
    * Each chunk is sent to the client as a separate SSE event.
    * The final chunk signals completion.
    */
  def streaming[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => Stream[F, ToolResult]
  ): Tools[F] =
    val ti = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    singleStreaming(tool) { json =>
      ti.decode(json) match
        case Right(a)  => handler(a)
        case Left(err) => Stream.raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a streaming tool with context support. */
  def streamingWithContext[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: (A, ToolContext[F]) => Stream[F, ToolResult]
  ): Tools[F] =
    val ti = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    singleStreamingWithContext(tool) { (json, ctx) =>
      ti.decode(json) match
        case Right(a)  => handler(a, ctx)
        case Left(err) => Stream.raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a streaming tool with no arguments. */
  def streamingNoArgs[F[_]: Concurrent](name: String, description: String)(
      handler: Stream[F, ToolResult]
  ): Tools[F] =
    val tool = Tool(name, Some(description), JsonSchema.empty)
    singleStreaming(tool)(_ => handler)

  /** Create a streaming tool from a regular tool handler that produces a single chunk. */
  def fromNonStreaming[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => F[ToolResult]
  ): Tools[F] =
    streaming[F, A](name, description) { args =>
      Stream.eval(handler(args))
    }

  /** Create streaming tool routes from a single tool */
  private def singleStreaming[F[_]: Concurrent](tool: Tool)(
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
  private def singleStreamingWithContext[F[_]: Concurrent](tool: Tool)(
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
