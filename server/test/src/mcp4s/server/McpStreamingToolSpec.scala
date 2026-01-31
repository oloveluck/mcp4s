package mcp4s.server

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpStreamingToolSpec extends CatsEffectSuite:

  // === McpStreamingTool Tests ===

  test("McpStreamingTool creates tool with streaming handler") {
    case class CountArgs(count: Int) derives ToolInput

    val streamingTool = McpStreamingTool[IO, CountArgs]("count", "Count to N") { args =>
      Stream.range(1, args.count + 1).map(n => ToolResult.text(s"Count: $n"))
    }

    for
      tools <- streamingTool.list
      _ = assertEquals(tools.size, 1)
      _ = assertEquals(tools.head.name, "count")

      // Call the streaming tool
      results <- streamingTool.callStreaming(
        "count",
        Json.obj("count" -> 3.asJson)
      ).get.compile.toList

      _ = assertEquals(results.size, 3)
      _ = assertEquals(results.map(_.textContent), List("Count: 1", "Count: 2", "Count: 3"))
    yield ()
  }

  test("McpStreamingTool.noArgs creates streaming tool without args") {
    val streamingTool = McpStreamingTool.noArgs[IO]("tick", "Emit ticks") {
      Stream.emits(List(
        ToolResult.text("tick 1"),
        ToolResult.text("tick 2"),
        ToolResult.text("tick 3")
      ))
    }

    for
      tools <- streamingTool.list
      _ = assertEquals(tools.head.inputSchema, JsonSchema.empty)

      results <- streamingTool.callStreaming("tick", Json.obj()).get.compile.toList
      _ = assertEquals(results.size, 3)
    yield ()
  }

  test("McpStreamingTool returns None for unknown tool") {
    case class Args(x: Int) derives ToolInput

    val tool = McpStreamingTool[IO, Args]("known", "Known tool") { args =>
      Stream.emit(ToolResult.text("ok"))
    }

    val result = tool.callStreaming("unknown", Json.obj())
    assertEquals(result, None)
  }

  test("McpStreamingTool handles errors in stream") {
    case class Args(fail: Boolean) derives ToolInput

    val tool = McpStreamingTool[IO, Args]("maybe-fail", "Maybe fails") { args =>
      if args.fail then
        Stream.raiseError[IO](new RuntimeException("Intentional failure"))
      else
        Stream.emit(ToolResult.text("success"))
    }

    for
      // Success case
      successResult <- tool.callStreaming("maybe-fail", Json.obj("fail" -> false.asJson)).get.compile.toList
      _ = assertEquals(successResult.head.textContent, "success")

      // Failure case
      failResult <- tool.callStreaming("maybe-fail", Json.obj("fail" -> true.asJson)).get.compile.toList.attempt
      _ = assert(failResult.isLeft)
    yield ()
  }

  // === McpStreamingTools Composition Tests ===

  test("McpStreamingTools.combine merges tools") {
    case class Args1(x: Int) derives ToolInput
    case class Args2(y: String) derives ToolInput

    val tool1 = McpStreamingTool[IO, Args1]("tool1", "First tool") { args =>
      Stream.emit(ToolResult.text(s"Tool1: ${args.x}"))
    }

    val tool2 = McpStreamingTool[IO, Args2]("tool2", "Second tool") { args =>
      Stream.emit(ToolResult.text(s"Tool2: ${args.y}"))
    }

    val combined = tool1 <+> tool2

    for
      tools <- combined.list
      _ = assertEquals(tools.map(_.name).toSet, Set("tool1", "tool2"))

      r1 <- combined.callStreaming("tool1", Json.obj("x" -> 42.asJson)).get.compile.toList
      r2 <- combined.callStreaming("tool2", Json.obj("y" -> "hello".asJson)).get.compile.toList

      _ = assertEquals(r1.head.textContent, "Tool1: 42")
      _ = assertEquals(r2.head.textContent, "Tool2: hello")
    yield ()
  }

  test("McpStreamingTools.empty returns empty list") {
    val empty = McpStreamingTools.empty[IO]

    for
      tools <- empty.list
      _ = assertEquals(tools, Nil)
      _ = assertEquals(empty.callStreaming("any", Json.obj()), None)
    yield ()
  }

  // === Conversion Tests ===

  test("McpTools.asStreaming converts regular tools to streaming") {
    val regularTool = McpTool.singleNumberPure[IO]("double", "Double a number") { n =>
      s"${n * 2}"
    }

    val streamingTool = regularTool.asStreaming

    for
      tools <- streamingTool.list
      _ = assertEquals(tools.head.name, "double")

      results <- streamingTool.callStreaming("double", Json.obj("value" -> 5.asJson)).get.compile.toList
      _ = assertEquals(results.size, 1)
      _ = assertEquals(results.head.textContent, "10.0")
    yield ()
  }

  test("McpStreamingTool.fromNonStreaming wraps regular handler") {
    case class EchoArgs(message: String) derives ToolInput

    val tool = McpStreamingTool.fromNonStreaming[IO, EchoArgs]("echo", "Echo message") { args =>
      IO.pure(ToolResult.text(args.message))
    }

    for
      results <- tool.callStreaming("echo", Json.obj("message" -> "hello".asJson)).get.compile.toList
      _ = assertEquals(results.size, 1)
      _ = assertEquals(results.head.textContent, "hello")
    yield ()
  }

  // === Context-aware Streaming Tool Tests ===

  test("McpStreamingTool.withContext passes context to handler") {
    case class LogArgs(count: Int) derives ToolInput

    val tool = McpStreamingTool.withContext[IO, LogArgs]("log", "Log with context") { (args, ctx) =>
      Stream.range(1, args.count + 1).evalMap { n =>
        ctx.log(LogLevel.Info, s"Processing $n").as(ToolResult.text(s"Done: $n"))
      }
    }

    for
      tools <- tool.list
      _ = assertEquals(tools.head.name, "log")

      results <- tool.callStreaming("log", Json.obj("count" -> 2.asJson)).get.compile.toList
      _ = assertEquals(results.size, 2)
    yield ()
  }
