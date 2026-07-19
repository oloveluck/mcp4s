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

import cats.{Applicative, Semigroup}
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*

/** Composable tool routes for MCP servers.
  *
  * Tools provides http4s-style partial function routing for tools, enabling:
  *   - Pattern matching on tool names and arguments
  *   - Composition via `|+|` (first match wins)
  *   - Easy modular organization of tools
  *
  * All tools receive a ToolContext for server-to-client operations (sampling, progress, logging).
  * Tools that don't need context simply ignore it.
  *
  * Example:
  * {{{
  * val mathTools = Tools.single(
  *   Tool("add", Some("Add numbers"), addSchema)
  * ) { args => handleAdd(args) }
  *
  * val stringTools = Tools.single(
  *   Tool("concat", Some("Concatenate"), schema)
  * ) { args => handleConcat(args) }
  *
  * val allTools = mathTools |+| stringTools
  * }}}
  */
trait Tools[F[_]]:
  /** List all tools provided by these routes */
  def list: F[List[Tool]]

  /** Call a tool with context, returning None if not handled */
  def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult]

  /** True when no tools are registered. Used to derive server capabilities: a server built from
    * empty routes does not advertise the `tools` capability.
    */
  def isEmpty: Boolean = false

  /** Tool definitions statically known at construction time (empty for fully dynamic
    * implementations). Used by `ServiceRoutes` for construction-time completeness checks.
    */
  def definitions: List[Tool] = Nil

object Tools:

  /** Create tool routes from a single tool (ignores context) */
  def single[F[_]: Concurrent](tool: Tool)(handler: Json => F[ToolResult]): Tools[F] =
    new Tools[F]:
      def list: F[List[Tool]]              = Applicative[F].pure(List(tool))
      override def definitions: List[Tool] = List(tool)

      def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
        if name == tool.name then OptionT.liftF(handler(args))
        else OptionT.none[F, ToolResult]

  /** Create context-aware tool routes from a single tool.
    *
    * The handler receives the ToolContext for server-to-client operations. This tool can be
    * composed with regular tools using `|+|`.
    */
  def singleWithContext[F[_]: Concurrent](tool: Tool)(
      handler: (Json, ToolContext[F]) => F[ToolResult]
  ): Tools[F] =
    new Tools[F]:
      def list: F[List[Tool]]              = Applicative[F].pure(List(tool))
      override def definitions: List[Tool] = List(tool)

      def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
        if name == tool.name then OptionT.liftF(handler(args, ctx))
        else OptionT.none[F, ToolResult]

  /** Empty tool routes */
  def empty[F[_]: Applicative]: Tools[F] =
    new Tools[F]:
      def list: F[List[Tool]] = Applicative[F].pure(Nil)
      def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] = OptionT.none
      override def isEmpty: Boolean = true

  /** Combine two Tools instances (first match wins) */
  def combine[F[_]: Concurrent](x: Tools[F], y: Tools[F]): Tools[F] =
    new Tools[F]:
      override def isEmpty: Boolean = x.isEmpty && y.isEmpty
      override def definitions: List[Tool] =
        val xNames = x.definitions.map(_.name).toSet
        x.definitions ++ y.definitions.filterNot(t => xNames.contains(t.name))
      def list: F[List[Tool]] =
        for
          xTools <- x.list
          yTools <- y.list
          xNames = xTools.map(_.name).toSet
        yield xTools ++ yTools.filterNot(t => xNames.contains(t.name))

      def call(name: String, args: Json, ctx: ToolContext[F]): OptionT[F, ToolResult] =
        x.call(name, args, ctx).orElse(y.call(name, args, ctx))

  /** Semigroup instance for Tools composition via |+| */
  given [F[_]: Concurrent]: Semigroup[Tools[F]] with
    def combine(x: Tools[F], y: Tools[F]): Tools[F] =
      Tools.combine(x, y)
