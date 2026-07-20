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

import cats.effect.Concurrent
import mcp4s.schema.McpService

/** Server-side implementation of an [[mcp4s.schema.McpService]].
  *
  * Bind one handler per declared endpoint; assembly verifies at construction time that the handlers
  * cover the service's endpoint list exactly — a missing, duplicate, or foreign handler is an
  * immediate `IllegalArgumentException` rather than a runtime `ToolNotFound`.
  *
  * {{{
  * import mcp4s.server.*
  * import mcp4s.server.dsl.*
  *
  * val calculatorRoutes: Tools[IO] = ServiceRoutes(Calculator)(
  *   Calculator.add.handle[IO](args => IO.pure(AddResult(args.a + args.b))),
  *   Calculator.greet.handle[IO](args => IO.pure(ok(s"Hello, $${args.name}!")))
  * )
  *
  * val server = McpServer[IO](Calculator.info).withTools(calculatorRoutes)
  * }}}
  */
object ServiceRoutes:

  /** Assemble the service's tool routes from one handler per endpoint.
    *
    * @throws IllegalArgumentException
    *   if any declared endpoint lacks a handler, any handler implements a tool the service does not
    *   declare, or the same endpoint is bound twice.
    */
  def apply[F[_]: Concurrent](service: McpService)(handlers: Tools[F]*): Tools[F] =
    val declared    = service.endpoints.map(_.name)
    val boundNames  = handlers.toList.flatMap(_.definitions.map(_.name))
    val declaredSet = declared.toSet
    val boundSet    = boundNames.toSet

    val duplicates = boundNames.groupBy(identity).collect { case (n, occ) if occ.size > 1 => n }
    val missing    = declared.filterNot(boundSet.contains)
    val extra      = boundNames.filterNot(declaredSet.contains)

    if duplicates.nonEmpty then
      throw new IllegalArgumentException(
        s"Service '${service.name}': endpoints bound more than once: ${duplicates.mkString(", ")}"
      )
    if missing.nonEmpty then
      throw new IllegalArgumentException(
        s"Service '${service.name}': no handler for endpoints: ${missing.mkString(", ")}"
      )
    if extra.nonEmpty then
      throw new IllegalArgumentException(
        s"Service '${service.name}': handlers for undeclared tools: ${extra.mkString(", ")}"
      )

    handlers.reduceOption(Tools.combine(_, _)).getOrElse(Tools.empty[F])
