package mcp4s.server

import cats.Applicative
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*

/** Composable tool routes for MCP servers.
  *
  * McpTools provides http4s-style partial function routing for tools, enabling:
  *   - Pattern matching on tool names and arguments
  *   - Composition via `<+>` (first match wins)
  *   - Easy modular organization of tools
  *
  * Example:
  * {{{
  * val mathTools = McpTools.of[IO](
  *   Tool("add", Some("Add numbers"), addSchema),
  *   Tool("subtract", Some("Subtract numbers"), subtractSchema)
  * ) {
  *   case ("add", args) => handleAdd(args)
  *   case ("subtract", args) => handleSubtract(args)
  * }
  *
  * val stringTools = McpTools.of[IO](Tool("concat", Some("Concatenate"), schema)) {
  *   case ("concat", args) => handleConcat(args)
  * }
  *
  * val allTools = mathTools <+> stringTools
  * }}}
  */
trait McpTools[F[_]]:
  /** List all tools provided by these routes */
  def list: F[List[Tool]]

  /** Call a tool, returning None if not handled */
  def call(name: String, args: Json): OptionT[F, ToolResult]

object McpTools:

  /** Create tool routes from a list of tools and a partial function handler */
  def of[F[_]: Concurrent](tools: Tool*)(
      pf: PartialFunction[(String, Json), F[ToolResult]]
  ): McpTools[F] =
    new McpTools[F]:
      private val toolList = tools.toList
      private val toolNames = toolList.map(_.name).toSet

      def list: F[List[Tool]] = Applicative[F].pure(toolList)

      def call(name: String, args: Json): OptionT[F, ToolResult] =
        if toolNames.contains(name) && pf.isDefinedAt((name, args)) then
          OptionT.liftF(pf((name, args)))
        else OptionT.none[F, ToolResult]

  /** Create tool routes from a single tool */
  def single[F[_]: Concurrent](tool: Tool)(handler: Json => F[ToolResult]): McpTools[F] =
    of(tool) { case (name, args) if name == tool.name => handler(args) }

  /** Empty tool routes */
  def empty[F[_]: Applicative]: McpTools[F] =
    new McpTools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(Nil)
      def call(name: String, args: Json): OptionT[F, ToolResult] = OptionT.none

  /** Combine two McpTools instances (first match wins) */
  def combine[F[_]: Concurrent](x: McpTools[F], y: McpTools[F]): McpTools[F] =
    new McpTools[F]:
      def list: F[List[Tool]] =
        for
          xTools <- x.list
          yTools <- y.list
          xNames = xTools.map(_.name).toSet
        yield xTools ++ yTools.filterNot(t => xNames.contains(t.name))

      def call(name: String, args: Json): OptionT[F, ToolResult] =
        x.call(name, args).orElse(y.call(name, args))

  extension [F[_]: Concurrent](tools: McpTools[F])
    /** Combine with another McpTools, this one takes precedence */
    def <+>(other: McpTools[F]): McpTools[F] =
      combine(tools, other)
