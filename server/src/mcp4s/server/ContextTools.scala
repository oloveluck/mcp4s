package mcp4s.server

import cats.Applicative
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*

/** Composable context-aware tool routes for MCP servers.
  *
  * Like Tools but handlers receive a ToolContext for server-to-client operations
  * (sampling, progress, logging).
  */
trait ContextTools[F[_]]:
  /** List all tools provided by these routes */
  def list: F[List[Tool]]

  /** Call a tool with context, returning None if not handled */
  def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult]

object ContextTools:

  /** Create context-aware tool routes from a single tool */
  def single[F[_]: Concurrent](tool: Tool)(handler: (Json, ToolContext[F]) => F[ToolResult]): ContextTools[F] =
    new ContextTools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(List(tool))

      def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
        if name == tool.name then OptionT.liftF(handler(args, ctx))
        else OptionT.none[F, ToolResult]

  /** Empty context-aware tool routes */
  def empty[F[_]: Applicative]: ContextTools[F] =
    new ContextTools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(Nil)
      def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] = OptionT.none

  /** Combine two ContextTools instances */
  def combine[F[_]: Concurrent](x: ContextTools[F], y: ContextTools[F]): ContextTools[F] =
    new ContextTools[F]:
      def list: F[List[Tool]] =
        for
          xTools <- x.list
          yTools <- y.list
          xNames = xTools.map(_.name).toSet
        yield xTools ++ yTools.filterNot(t => xNames.contains(t.name))

      def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
        x.call(name, args, ctx).orElse(y.call(name, args, ctx))

  extension [F[_]: Concurrent](tools: ContextTools[F])
    def <+>(other: ContextTools[F]): ContextTools[F] =
      combine(tools, other)
