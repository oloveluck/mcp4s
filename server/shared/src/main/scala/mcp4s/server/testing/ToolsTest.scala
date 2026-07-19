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

package mcp4s.server.testing

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.{Encoder, Json}
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.server.{SamplingRequester, ToolContext, Tools}

/** Test utilities for Tools.
  *
  * Provides a convenient way to test tools in isolation.
  *
  * {{{
  * import mcp4s.server.dsl.*
  * case class AddArgs(a: Double, b: Double) derives Schema
  * val tools = Tool.from[AddArgs].withDescription("Add").handle[IO] { args =>
  *   IO.pure(ToolResult.text(s"${args.a + args.b}"))
  * }
  *
  * test("add tool calculates correctly") {
  *   for
  *     result <- tools.testCall("add", AddArgs(2.0, 3.0))
  *     _ = assertEquals(result.textContent, "5.0")
  *   yield ()
  * }
  * }}}
  */
object ToolsTest:

  extension [F[_]: Concurrent](tools: Tools[F])

    /** Call a tool with typed arguments for testing.
      *
      * Raises ToolNotFound if tool doesn't exist.
      */
    def testCall[A: Encoder](name: String, arguments: A): F[ToolResult] =
      val ctx = ToolContext.minimal[F](SamplingRequester.unsupported[F], RequestId.NullId)
      tools
        .call(name, arguments.asJson, ctx)
        .getOrElseF(
          Concurrent[F].raiseError(McpError.ToolNotFound(name))
        )

    /** Call a tool with JSON arguments for testing.
      *
      * Raises ToolNotFound if tool doesn't exist.
      */
    def testCallJson(name: String, arguments: Json): F[ToolResult] =
      val ctx = ToolContext.minimal[F](SamplingRequester.unsupported[F], RequestId.NullId)
      tools
        .call(name, arguments, ctx)
        .getOrElseF(
          Concurrent[F].raiseError(McpError.ToolNotFound(name))
        )

    /** Check if a tool with the given name exists */
    def hasTool(name: String): F[Boolean] =
      tools.list.map(_.exists(_.name == name))

    /** Get tool definition by name */
    def getTool(name: String): F[Option[Tool]] =
      tools.list.map(_.find(_.name == name))

    /** Assert a tool exists and return its definition */
    def assertTool(name: String): F[Tool] =
      getTool(name).flatMap {
        case Some(tool) => Concurrent[F].pure(tool)
        case None =>
          Concurrent[F].raiseError(
            new AssertionError(s"Expected tool '$name' to exist")
          )
      }

/** Tuple-based argument syntax for concise test calls.
  *
  * {{{
  * import mcp4s.server.testing.args
  *
  * tools.testCall("add", args("a" -> 2.0, "b" -> 3.0))
  * }}}
  */
object args:

  /** Convert a single key-value pair to Json */
  def apply[A: Encoder](kv: (String, A)): Json =
    Json.obj(kv._1 -> kv._2.asJson)

  /** Convert two key-value pairs to Json */
  def apply[A: Encoder, B: Encoder](kv1: (String, A), kv2: (String, B)): Json =
    Json.obj(kv1._1 -> kv1._2.asJson, kv2._1 -> kv2._2.asJson)

  /** Convert three key-value pairs to Json */
  def apply[A: Encoder, B: Encoder, C: Encoder](
      kv1: (String, A),
      kv2: (String, B),
      kv3: (String, C)
  ): Json =
    Json.obj(
      kv1._1 -> kv1._2.asJson,
      kv2._1 -> kv2._2.asJson,
      kv3._1 -> kv3._2.asJson
    )

  /** Convert four key-value pairs to Json */
  def apply[A: Encoder, B: Encoder, C: Encoder, D: Encoder](
      kv1: (String, A),
      kv2: (String, B),
      kv3: (String, C),
      kv4: (String, D)
  ): Json =
    Json.obj(
      kv1._1 -> kv1._2.asJson,
      kv2._1 -> kv2._2.asJson,
      kv3._1 -> kv3._2.asJson,
      kv4._1 -> kv4._2.asJson
    )

  /** Empty arguments */
  def empty: Json = Json.obj()
