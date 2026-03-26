package mcp4s.agent

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.Json
import munit.CatsEffectSuite

class TokenEstimatorSpec extends CatsEffectSuite:

  private val estimator = TokenEstimator.charBased[IO]

  test("charBased estimates User message") {
    val msg = Message.User("hello world") // 11 chars -> 11/4 = 2
    estimator.estimate(msg).map(t => assertEquals(t.value, 2L))
  }

  test("charBased estimates Assistant message") {
    val msg = Message.Assistant("twelve chars") // 12 chars -> 12/4 = 3
    estimator.estimate(msg).map(t => assertEquals(t.value, 3L))
  }

  test("charBased estimates ToolUse message") {
    val call = ToolCall("id", "myTool", Json.obj("key" -> Json.fromString("val")))
    val msg = Message.ToolUse(NonEmptyList.one(call))
    // "myTool" = 6, {"key":"val"} noSpaces = 13 -> (6+13)/4 = 4
    estimator.estimate(msg).map(t => assertEquals(t.value, 4L))
  }

  test("charBased estimates ToolResult message") {
    val msg = Message.ToolResult("id", "tool", Json.fromString("result"))
    // "tool" = 4, "result" (as JSON string noSpaces) = 8 -> (4+8)/4 = 3
    estimator.estimate(msg).map(t => assertEquals(t.value, 3L))
  }

  test("estimateAll sums correctly") {
    val msgs = List(
      Message.User("aaaa"),     // 4 chars -> 1
      Message.User("bbbbbbbb")  // 8 chars -> 2
    )
    estimator.estimateAll(msgs).map(t => assertEquals(t.value, 3L))
  }

  test("estimateAll on empty list returns zero") {
    estimator.estimateAll(Nil).map(t => assertEquals(t.value, 0L))
  }

  test("custom estimator via apply factory") {
    val custom = TokenEstimator[IO](_ => IO.pure(Tokens(10L)))
    val msg = Message.User("anything")
    custom.estimate(msg).map(t => assertEquals(t.value, 10L))
  }
