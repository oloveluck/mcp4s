package mcp4s.server

import cats.effect.IO
import cats.effect.Ref
import io.circe.Json
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpMiddlewareSpec extends CatsEffectSuite:

  test("logging middleware logs tool calls") {
    for
      logs <- Ref.of[IO, List[String]](Nil)
      middleware = McpMiddleware.logging[IO](msg => logs.update(_ :+ msg))

      tools = McpTool.twoNumbersPure[IO]("add", "Add") { (a, b) => s"${a + b}" }
        .withMiddleware(middleware)

      result <- tools.call("add", Json.obj("a" -> 1.asJson, "b" -> 2.asJson)).value
      logMessages <- logs.get

      _ = assert(logMessages.exists(_.contains("Calling tool: add")))
      _ = assert(logMessages.exists(_.contains("completed successfully")))
      _ = assertEquals(result.map(_.textContent), Some("3.0"))
    yield ()
  }

  test("logging middleware logs errors") {
    for
      logs <- Ref.of[IO, List[String]](Nil)
      middleware = McpMiddleware.logging[IO](msg => logs.update(_ :+ msg))

      tools = McpTool.noArgs[IO]("fail", "Fails") {
        IO.raiseError(new RuntimeException("boom"))
      }.withMiddleware(middleware)

      result <- tools.call("fail", Json.obj()).value.attempt
      logMessages <- logs.get

      _ = assert(logMessages.exists(_.contains("Calling tool: fail")))
      _ = assert(logMessages.exists(_.contains("failed with exception")))
      _ = assert(result.isLeft)
    yield ()
  }

  test("timed middleware measures duration") {
    for
      durations <- Ref.of[IO, List[(String, Long)]](Nil)
      middleware = McpMiddleware.timed[IO] { (name, duration) =>
        durations.update(_ :+ (name, duration.toMillis))
      }

      tools = McpTool.noArgs[IO]("slow", "Slow") {
        IO.sleep(scala.concurrent.duration.Duration(50, "ms")) *>
          IO.pure(ToolResult.text("done"))
      }.withMiddleware(middleware)

      _ <- tools.call("slow", Json.obj()).value
      recorded <- durations.get

      _ = assertEquals(recorded.length, 1)
      _ = assertEquals(recorded.head._1, "slow")
      _ = assert(recorded.head._2 >= 50, s"Expected >= 50ms, got ${recorded.head._2}ms")
    yield ()
  }

  test("catchErrors middleware converts exceptions to error results") {
    val tools = McpTool.noArgs[IO]("fail", "Fails") {
      IO.raiseError(new RuntimeException("something went wrong"))
    }.withMiddleware(McpMiddleware.catchErrors[IO])

    for
      result <- tools.call("fail", Json.obj()).value
      _ = assert(result.isDefined)
      _ = assert(result.get.isError.getOrElse(false))
      _ = assert(result.get.textContent.contains("something went wrong"))
    yield ()
  }

  test("catchErrorsPartial middleware handles specific exceptions") {
    // Use standard library exceptions that can be matched at runtime
    val middleware = McpMiddleware.catchErrorsPartial[IO] {
      case e: IllegalArgumentException => s"Invalid: ${e.getMessage}"
    }

    val illegalFail = McpTool.noArgs[IO]("illegal", "Illegal arg fail") {
      IO.raiseError(new IllegalArgumentException("bad input"))
    }.withMiddleware(middleware)

    val otherFail = McpTool.noArgs[IO]("other", "Other fail") {
      IO.raiseError(new RuntimeException("boom"))
    }.withMiddleware(middleware)

    for
      illegalResult <- illegalFail.call("illegal", Json.obj()).value
      _ = assert(illegalResult.isDefined)
      _ = assert(illegalResult.get.isError.getOrElse(false))
      _ = assertEquals(illegalResult.get.textContent, "Invalid: bad input")

      otherResult <- otherFail.call("other", Json.obj()).value.attempt
      _ = assert(otherResult.isLeft, "Non-illegal error should propagate")
    yield ()
  }

  test("validate middleware rejects invalid calls") {
    val middleware = McpMiddleware.validate[IO] { (name, args) =>
      val hasA = args.hcursor.get[Double]("a").isRight
      IO.pure(if hasA then None else Some("Missing required parameter 'a'"))
    }

    val tools = McpTool.singleNumber[IO]("double", "Double") { n =>
      IO.pure(ToolResult.text(s"${n * 2}"))
    }.withMiddleware(middleware)

    for
      // Valid call passes validation
      validResult <- tools.call("double", Json.obj("a" -> 5.asJson, "value" -> 5.asJson)).value
      // Invalid call fails validation
      invalidResult <- tools.call("double", Json.obj("value" -> 5.asJson)).value

      _ = assert(validResult.isDefined)
      _ = assert(invalidResult.isDefined)
      _ = assert(invalidResult.get.isError.getOrElse(false))
      _ = assert(invalidResult.get.textContent.contains("Missing required parameter"))
    yield ()
  }

  test("combined middleware applies in order") {
    for
      order <- Ref.of[IO, List[String]](Nil)

      first = new McpMiddleware[IO]:
        def apply(name: String, args: Json)(next: => IO[ToolResult]): IO[ToolResult] =
          order.update(_ :+ "first-before") *> next <* order.update(_ :+ "first-after")

      second = new McpMiddleware[IO]:
        def apply(name: String, args: Json)(next: => IO[ToolResult]): IO[ToolResult] =
          order.update(_ :+ "second-before") *> next <* order.update(_ :+ "second-after")

      combined = McpMiddleware.combine(first, second)

      tools = McpTool.pureTextNoArgs[IO]("test", "Test")("result")
        .withMiddleware(combined)

      _ <- tools.call("test", Json.obj()).value
      callOrder <- order.get

      // First middleware is outermost
      _ = assertEquals(callOrder, List("first-before", "second-before", "second-after", "first-after"))
    yield ()
  }

  test("middleware does not affect non-existent tools") {
    for
      called <- Ref.of[IO, Boolean](false)
      middleware = new McpMiddleware[IO]:
        def apply(name: String, args: Json)(next: => IO[ToolResult]): IO[ToolResult] =
          called.set(true) *> next

      tools = McpTool.pureTextNoArgs[IO]("exists", "Exists")("ok")
        .withMiddleware(middleware)

      result <- tools.call("nonexistent", Json.obj()).value
      wasCalled <- called.get

      _ = assertEquals(result, None)
      _ = assertEquals(wasCalled, false)
    yield ()
  }
