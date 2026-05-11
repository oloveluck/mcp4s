package mcp4s.server

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpStreamingToolSpec extends CatsEffectSuite:

  private val minimalCtx = ToolContext.minimal[IO](SamplingRequester.unsupported[IO], RequestId.NullId)

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

      // Call returns the last emitted result
      result <- streamingTool.call(
        "count",
        Json.obj("count" -> 3.asJson),
        minimalCtx
      ).value

      _ = assertEquals(result.map(_.textContent), Some("Count: 3"))
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

      result <- streamingTool.call("tick", Json.obj(), minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("tick 3"))
    yield ()
  }

  test("McpTool.streaming returns None for unknown tool") {
    case class Args(x: Int) derives ToolInput

    val tool = McpTool.streaming[IO, Args]("known", "Known tool") { _ =>
      Stream.emit(ToolResult.text("ok"))
    }

    for
      result <- tool.call("unknown", Json.obj(), minimalCtx).value
      _ = assertEquals(result, None)
    yield ()
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
      successResult <- tool.call("maybe-fail", Json.obj("fail" -> false.asJson), minimalCtx).value
      _ = assertEquals(successResult.map(_.textContent), Some("success"))

      // Failure case
      failResult <- tool.call("maybe-fail", Json.obj("fail" -> true.asJson), minimalCtx).value.attempt
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

      // Regular tool works via call
      regularResult <- combined.call("regular-tool", Json.obj("y" -> "hello".asJson), minimalCtx).value
      _ = assertEquals(regularResult.map(_.textContent), Some("result: hello"))

      // Streaming tool also works via call (returns last result)
      callResult <- combined.call("stream-tool", Json.obj("x" -> 2.asJson), minimalCtx).value
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

      r1 <- combined.call("tool1", Json.obj("x" -> 42.asJson), minimalCtx).value
      r2 <- combined.call("tool2", Json.obj("y" -> "hello".asJson), minimalCtx).value

      _ = assertEquals(r1.map(_.textContent), Some("Tool1: 42"))
      _ = assertEquals(r2.map(_.textContent), Some("Tool2: hello"))
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

      result <- tool.call("log", Json.obj("count" -> 2.asJson), minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("Done: 2"))
    yield ()
  }
