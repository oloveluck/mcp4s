package mcp4s.server.testing

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.{Encoder, Json}
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.server.McpTools

/** Test utilities for McpTools.
  *
  * Provides a convenient way to test tools in isolation.
  *
  * {{{
  * val tools = McpTool.twoNumbers[IO]("add", "Add") { (a, b) =>
  *   IO.pure(ToolResult.text(s"${a + b}"))
  * }
  *
  * test("add tool calculates correctly") {
  *   for
  *     result <- tools.testCall("add", ("a" -> 2.0, "b" -> 3.0))
  *     _ = assertEquals(result.textContent, "5.0")
  *   yield ()
  * }
  * }}}
  */
object McpToolsTest:

  extension [F[_]: Concurrent](tools: McpTools[F])

    /** Call a tool with typed arguments for testing.
      *
      * Raises ToolNotFound if tool doesn't exist.
      */
    def testCall[A: Encoder](name: String, arguments: A): F[ToolResult] =
      tools.call(name, arguments.asJson).getOrElseF(
        Concurrent[F].raiseError(McpError.ToolNotFound(name))
      )

    /** Call a tool with JSON arguments for testing.
      *
      * Raises ToolNotFound if tool doesn't exist.
      */
    def testCallJson(name: String, arguments: Json): F[ToolResult] =
      tools.call(name, arguments).getOrElseF(
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
        case None => Concurrent[F].raiseError(
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
