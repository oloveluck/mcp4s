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

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpStreamingToolSpec extends CatsEffectSuite:

  import mcp4s.server.dsl.*

  private val minimalCtx =
    ToolContext.minimal[IO](SamplingRequester.unsupported[IO], RequestId.NullId)

  // === Streaming Tool Tests ===

  test("stream creates tool with streaming handler") {
    case class CountArgs(count: Int) derives Schema

    val streamingTool = Tool("count").withDescription("Count to N").input[CountArgs].stream[IO] {
      args =>
        Stream.range(1, args.count + 1).map(n => ToolResult.text(s"Count: $n"))
    }

    for
      tools <- streamingTool.list
      _ = assertEquals(tools.size, 1)
      _ = assertEquals(tools.head.name, "count")

      // Call returns the last emitted result
      result <- streamingTool
        .call(
          "count",
          Json.obj("count" -> 3.asJson),
          minimalCtx
        )
        .value

      _ = assertEquals(result.map(_.textContent), Some("Count: 3"))
    yield ()
  }

  test("stream creates streaming tool without args") {
    val streamingTool = Tool("tick").withDescription("Emit ticks").stream[IO] { _ =>
      Stream.emits(
        List(
          ToolResult.text("tick 1"),
          ToolResult.text("tick 2"),
          ToolResult.text("tick 3")
        )
      )
    }

    for
      tools <- streamingTool.list
      _ = assertEquals(tools.head.inputSchema, JsonSchema.empty)

      result <- streamingTool.call("tick", Json.obj(), minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("tick 3"))
    yield ()
  }

  test("stream returns None for unknown tool") {
    case class Args(x: Int) derives Schema

    val tool = Tool("known").withDescription("Known tool").input[Args].stream[IO] { _ =>
      Stream.emit(ToolResult.text("ok"))
    }

    for
      result <- tool.call("unknown", Json.obj(), minimalCtx).value
      _ = assertEquals(result, None)
    yield ()
  }

  test("stream handles errors in stream") {
    case class Args(fail: Boolean) derives Schema

    val tool = Tool("maybe-fail").withDescription("Maybe fails").input[Args].stream[IO] { args =>
      if args.fail then Stream.raiseError[IO](new RuntimeException("Intentional failure"))
      else Stream.emit(ToolResult.text("success"))
    }

    for
      // Success case
      successResult <- tool.call("maybe-fail", Json.obj("fail" -> false.asJson), minimalCtx).value
      _ = assertEquals(successResult.map(_.textContent), Some("success"))

      // Failure case
      failResult <- tool
        .call("maybe-fail", Json.obj("fail" -> true.asJson), minimalCtx)
        .value
        .attempt
      _ = assert(failResult.isLeft)
    yield ()
  }

  // === Streaming + Regular Tools Composition ===

  test("streaming and regular tools compose with |+|") {
    case class Args1(x: Int) derives Schema
    case class Args2(y: String) derives Schema

    val streamingTool = Tool("stream-tool").withDescription("Streaming tool").input[Args1].stream[IO] {
      args =>
        Stream.range(1, args.x + 1).map(n => ToolResult.text(s"chunk $n"))
    }

    val regularTool = Tool("regular-tool").withDescription("Regular tool").input[Args2].handle[IO] {
      args =>
        IO.pure(ToolResult.text(s"result: ${args.y}"))
    }

    val combined = streamingTool |+| regularTool

    for
      tools <- combined.list
      _ = assertEquals(tools.map(_.name).toSet, Set("stream-tool", "regular-tool"))

      // Regular tool works via call
      regularResult <- combined
        .call("regular-tool", Json.obj("y" -> "hello".asJson), minimalCtx)
        .value
      _ = assertEquals(regularResult.map(_.textContent), Some("result: hello"))

      // Streaming tool also works via call (returns last result)
      callResult <- combined.call("stream-tool", Json.obj("x" -> 2.asJson), minimalCtx).value
      _ = assertEquals(callResult.map(_.textContent), Some("chunk 2"))
    yield ()
  }

  test("two streaming tools compose with |+|") {
    case class Args1(x: Int) derives Schema
    case class Args2(y: String) derives Schema

    val tool1 = Tool("tool1").withDescription("First tool").input[Args1].stream[IO] { args =>
      Stream.emit(ToolResult.text(s"Tool1: ${args.x}"))
    }

    val tool2 = Tool("tool2").withDescription("Second tool").input[Args2].stream[IO] { args =>
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

  test("streamWith passes context to handler") {
    case class LogArgs(count: Int) derives Schema

    val tool = Tool("log").withDescription("Log with context").input[LogArgs].streamWith[IO] {
      (args, ctx) =>
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

  test("streamWith supports derived names via Tool.from") {
    @mcp4s.protocol.description("DSL log with context")
    case class DslLogArgs(count: Int) derives Schema

    val tool = Tool.from[DslLogArgs].streamWith[IO] { (args, ctx) =>
      Stream.range(1, args.count + 1).evalMap { n =>
        ctx.log(LogLevel.Info, s"step $n").as(ToolResult.text(s"Done: $n"))
      }
    }

    for
      tools <- tool.list
      _ = assertEquals(tools.head.name, "dsl_log")
      _ = assertEquals(tools.head.description, Some("DSL log with context"))
      result <- tool.call("dsl_log", Json.obj("count" -> 3.asJson), minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("Done: 3"))
    yield ()
  }

  test("streamWith supports the no-arg context + streaming variant") {
    val tool = Tool("ticks").withDescription("Stream ticks with context").streamWith[IO] {
      (_, ctx) =>
        Stream
          .range(1, 3)
          .evalMap(n => ctx.log(LogLevel.Info, s"tick $n").as(ToolResult.text(s"tick $n")))
    }

    for
      tools <- tool.list
      _ = assertEquals(tools.head.inputSchema, JsonSchema.empty)
      result <- tool.call("ticks", Json.obj(), minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("tick 2"))
    yield ()
  }
