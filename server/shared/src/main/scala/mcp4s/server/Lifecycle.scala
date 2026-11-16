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

import cats.effect.{Concurrent, Resource as CatsResource}
import cats.syntax.all.*

/** Tools backed by a managed resource (a connection pool, an HTTP client, ...): the resource is
  * acquired when the returned `Resource[F, Tools[F]]` is used and released with it.
  */
object McpLifecycleTool:
  import dsl.{handle, handleWith}
  import mcp4s.schema.ToolEndpoint

  /** Create a tool with an acquired resource.
    *
    * The resource is acquired when the server starts and released when it stops. The handler
    * receives the acquired resource along with the tool arguments.
    *
    * Example:
    * {{{
    * case class QueryArgs(sql: String) derives Schema
    *
    * val dbTools: Resource[IO, Tools[IO]] = McpLifecycleTool(
    *   Tool.from[QueryArgs].withDescription("Run SQL query"),
    *   acquire = HikariCP.resource(config)
    * ) { (args, dataSource) =>
    *   IO.blocking(runQuery(dataSource, args.sql))
    * }
    * }}}
    */
  def apply[F[_]: Concurrent, I, O, R](
      endpoint: ToolEndpoint[I, O],
      acquire: CatsResource[F, R]
  )(handler: (I, R) => F[O]): CatsResource[F, Tools[F]] =
    acquire.map: resource =>
      endpoint.handle[F](args => handler(args, resource))

  /** Create a context-aware tool with an acquired resource.
    *
    * The handler receives the ToolContext for server-to-client operations, as well as the acquired
    * resource.
    */
  def withContext[F[_]: Concurrent, I, O, R](
      endpoint: ToolEndpoint[I, O],
      acquire: CatsResource[F, R]
  )(handler: (I, R, ToolContext[F]) => F[O]): CatsResource[F, Tools[F]] =
    acquire.map: resource =>
      endpoint.handleWith[F]((args, ctx) => handler(args, resource, ctx))

  /** Combine multiple lifecycle tools into a single resource.
    *
    * All tools' resources will be acquired together and released together.
    *
    * Example:
    * {{{
    * val allTools: Resource[IO, Tools[IO]] = McpLifecycleTool.combine(
    *   dbQueryTool,
    *   httpClientTool,
    *   cacheClientTool
    * )
    * }}}
    */
  def combine[F[_]: Concurrent](
      tools: CatsResource[F, Tools[F]]*
  ): CatsResource[F, Tools[F]] =
    tools.toList.sequence.map: toolsList =>
      toolsList.foldLeft(Tools.empty[F])(_ |+| _)

  /** Combine a lifecycle tool with a static tool.
    *
    * Example:
    * {{{
    * val combined: Resource[IO, Tools[IO]] = McpLifecycleTool.combineWith(
    *   lifecycleTool,
    *   staticTool1 |+| staticTool2
    * )
    * }}}
    */
  def combineWith[F[_]: Concurrent](
      lifecycle: CatsResource[F, Tools[F]],
      static: Tools[F]
  ): CatsResource[F, Tools[F]] =
    lifecycle.map(_ |+| static)
