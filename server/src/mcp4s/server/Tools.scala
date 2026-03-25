package mcp4s.server

import cats.{Applicative, Semigroup}
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream
import io.circe.Json
import mcp4s.protocol.*

/** Composable tool routes for MCP servers.
  *
  * Tools provides http4s-style partial function routing for tools, enabling:
  *   - Pattern matching on tool names and arguments
  *   - Composition via `<+>` (first match wins)
  *   - Easy modular organization of tools
  *
  * Both regular tools and context-aware tools can be composed together using
  * the `callWithContext` method, which allows regular tools and context tools
  * to work in the same pipeline.
  *
  * Example:
  * {{{
  * val mathTools = Tools.of[IO](
  *   Tool("add", Some("Add numbers"), addSchema),
  *   Tool("subtract", Some("Subtract numbers"), subtractSchema)
  * ) {
  *   case ("add", args) => handleAdd(args)
  *   case ("subtract", args) => handleSubtract(args)
  * }
  *
  * val stringTools = Tools.of[IO](Tool("concat", Some("Concatenate"), schema)) {
  *   case ("concat", args) => handleConcat(args)
  * }
  *
  * val allTools = mathTools <+> stringTools
  * }}}
  */
trait Tools[F[_]]:
  /** List all tools provided by these routes */
  def list: F[List[Tool]]

  /** Call a tool, returning None if not handled */
  def call(name: String, args: Json): OptionT[F, ToolResult]

  /** Call a tool with context, returning None if not handled.
    *
    * For regular tools, the context is ignored.
    * For context-aware tools, the context is passed to the handler.
    * This method enables composition of both regular and context-aware tools.
    */
  def callWithContext(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
    call(name, args) // Default: ignore context

  /** Call a tool returning a stream of results. None if not handled.
    *
    * For non-streaming tools, this returns None by default.
    * Streaming tools override this to emit multiple results over time.
    */
  def callStreaming(name: String, args: Json): Option[Stream[F, ToolResult]] = None

  /** Call a streaming tool with context. None if not handled.
    *
    * For non-streaming tools, this returns None by default.
    * Streaming tools override this to emit multiple results over time.
    */
  def callStreamingWithContext(name: String, args: Json, ctx: ToolContext[F]): Option[Stream[F, ToolResult]] =
    callStreaming(name, args)

object Tools:

  /** Create tool routes from a list of tools and a partial function handler */
  def of[F[_]: Concurrent](tools: Tool*)(
      pf: PartialFunction[(String, Json), F[ToolResult]]
  ): Tools[F] =
    new Tools[F]:
      private val toolList = tools.toList
      private val toolNames = toolList.map(_.name).toSet

      def list: F[List[Tool]] = Applicative[F].pure(toolList)

      def call(name: String, args: Json): OptionT[F, ToolResult] =
        if toolNames.contains(name) && pf.isDefinedAt((name, args)) then
          OptionT.liftF(pf((name, args)))
        else OptionT.none[F, ToolResult]

  /** Create tool routes from a single tool */
  def single[F[_]: Concurrent](tool: Tool)(handler: Json => F[ToolResult]): Tools[F] =
    of(tool) { case (name, args) if name == tool.name => handler(args) }

  /** Create context-aware tool routes from a single tool.
    *
    * The handler receives the ToolContext for server-to-client operations.
    * This tool can be composed with regular tools using `|+|`.
    */
  def singleWithContext[F[_]: Concurrent](tool: Tool)(
      handler: (Json, ToolContext[F]) => F[ToolResult]
  ): Tools[F] =
    new Tools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(List(tool))

      def call(name: String, args: Json): OptionT[F, ToolResult] =
        // When called without context, provide minimal context
        if name == tool.name then
          val ctx = ToolContext.minimal[F](SamplingRequester.unsupported[F], RequestId.NullId)
          OptionT.liftF(handler(args, ctx))
        else OptionT.none[F, ToolResult]

      override def callWithContext(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
        if name == tool.name then OptionT.liftF(handler(args, ctx))
        else OptionT.none[F, ToolResult]

  /** Empty tool routes */
  def empty[F[_]: Applicative]: Tools[F] =
    new Tools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(Nil)
      def call(name: String, args: Json): OptionT[F, ToolResult] = OptionT.none

  /** Combine two Tools instances (first match wins) */
  def combine[F[_]: Concurrent](x: Tools[F], y: Tools[F]): Tools[F] =
    new Tools[F]:
      def list: F[List[Tool]] =
        for
          xTools <- x.list
          yTools <- y.list
          xNames = xTools.map(_.name).toSet
        yield xTools ++ yTools.filterNot(t => xNames.contains(t.name))

      def call(name: String, args: Json): OptionT[F, ToolResult] =
        x.call(name, args).orElse(y.call(name, args))

      override def callWithContext(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
        x.callWithContext(name, args, ctx).orElse(y.callWithContext(name, args, ctx))

      override def callStreaming(name: String, args: Json): Option[Stream[F, ToolResult]] =
        x.callStreaming(name, args).orElse(y.callStreaming(name, args))

      override def callStreamingWithContext(name: String, args: Json, ctx: ToolContext[F]): Option[Stream[F, ToolResult]] =
        x.callStreamingWithContext(name, args, ctx).orElse(y.callStreamingWithContext(name, args, ctx))

  /** Semigroup instance for Tools composition via |+| */
  given [F[_]: Concurrent]: Semigroup[Tools[F]] with
    def combine(x: Tools[F], y: Tools[F]): Tools[F] =
      Tools.combine(x, y)

  extension [F[_]: Concurrent](tools: Tools[F])
    /** Combine with another Tools, this one takes precedence */
    def <+>(other: Tools[F]): Tools[F] =
      combine(tools, other)
