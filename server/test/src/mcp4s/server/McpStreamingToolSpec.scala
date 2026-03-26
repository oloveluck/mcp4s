package mcp4s.server

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpStreamingToolSpec extends CatsEffectSuite:

  // === McpTool Streaming Tests ===

  test("McpTool.streaming creates tool with streaming handler") {
    case class CountArgs(count: Int) derives ToolInput

    val streamingTool = McpTool.streaming[IO, CountArgs]("count", "Count to N") { args =>
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

  test("McpTool.streamingNoArgs creates streaming tool without args") {
    val streamingTool = McpTool.streamingNoArgs[IO]("tick", "Emit ticks") {
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

  test("McpTool.streaming returns None for unknown tool") {
    case class Args(x: Int) derives ToolInput

    val tool = McpTool.streaming[IO, Args]("known", "Known tool") { args =>
      Stream.emit(ToolResult.text("ok"))
    }

    val result = tool.callStreaming("unknown", Json.obj())
    assertEquals(result, None)
  }

  test("McpTool.streaming handles errors in stream") {
    case class Args(fail: Boolean) derives ToolInput

    val tool = McpTool.streaming[IO, Args]("maybe-fail", "Maybe fails") { args =>
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

  // === Streaming + Regular Tools Composition ===

  test("streaming and regular tools compose with |+|") {
    case class Args1(x: Int) derives ToolInput
    case class Args2(y: String) derives ToolInput

    val streamingTool = McpTool.streaming[IO, Args1]("stream-tool", "Streaming tool") { args =>
      Stream.range(1, args.x + 1).map(n => ToolResult.text(s"chunk $n"))
    }

    val regularTool = McpTool[IO, Args2]("regular-tool", "Regular tool") { args =>
      IO.pure(ToolResult.text(s"result: ${args.y}"))
    }

    val combined = streamingTool |+| regularTool

    for
      tools <- combined.list
      _ = assertEquals(tools.map(_.name).toSet, Set("stream-tool", "regular-tool"))

      // Streaming tool returns stream
      streamResults <- combined.callStreaming("stream-tool", Json.obj("x" -> 3.asJson)).get.compile.toList
      _ = assertEquals(streamResults.map(_.textContent), List("chunk 1", "chunk 2", "chunk 3"))

      // Regular tool has no streaming capability
      _ = assertEquals(combined.callStreaming("regular-tool", Json.obj("y" -> "hello".asJson)), None)

      // Regular tool works via call
      regularResult <- combined.call("regular-tool", Json.obj("y" -> "hello".asJson)).value
      _ = assertEquals(regularResult.map(_.textContent), Some("result: hello"))

      // Streaming tool also works via call (returns last result)
      callResult <- combined.call("stream-tool", Json.obj("x" -> 2.asJson)).value
      _ = assertEquals(callResult.map(_.textContent), Some("chunk 2"))
    yield ()
  }

  test("two streaming tools compose with |+|") {
    case class Args1(x: Int) derives ToolInput
    case class Args2(y: String) derives ToolInput

    val tool1 = McpTool.streaming[IO, Args1]("tool1", "First tool") { args =>
      Stream.emit(ToolResult.text(s"Tool1: ${args.x}"))
    }

    val tool2 = McpTool.streaming[IO, Args2]("tool2", "Second tool") { args =>
      Stream.emit(ToolResult.text(s"Tool2: ${args.y}"))
    }

    val combined = tool1 |+| tool2

    for
      tools <- combined.list
      _ = assertEquals(tools.map(_.name).toSet, Set("tool1", "tool2"))

      r1 <- combined.callStreaming("tool1", Json.obj("x" -> 42.asJson)).get.compile.toList
      r2 <- combined.callStreaming("tool2", Json.obj("y" -> "hello".asJson)).get.compile.toList

      _ = assertEquals(r1.head.textContent, "Tool1: 42")
      _ = assertEquals(r2.head.textContent, "Tool2: hello")
    yield ()
  }

  test("non-streaming tools return None for callStreaming") {
    val tool = McpTool.singleNumberPure[IO]("double", "Double a number") { n =>
      s"${n * 2}"
    }

    assertEquals(tool.callStreaming("double", Json.obj("value" -> 5.asJson)), None)
  }

  test("McpTool.fromNonStreaming wraps regular handler") {
    case class EchoArgs(message: String) derives ToolInput

    val tool = McpTool.fromNonStreaming[IO, EchoArgs]("echo", "Echo message") { args =>
      IO.pure(ToolResult.text(args.message))
    }

    for
      results <- tool.callStreaming("echo", Json.obj("message" -> "hello".asJson)).get.compile.toList
      _ = assertEquals(results.size, 1)
      _ = assertEquals(results.head.textContent, "hello")
    yield ()
  }

  // === Context-aware Streaming Tool Tests ===

  test("McpTool.streamingWithContext passes context to handler") {
    case class LogArgs(count: Int) derives ToolInput

    val tool = McpTool.streamingWithContext[IO, LogArgs]("log", "Log with context") { (args, ctx) =>
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
