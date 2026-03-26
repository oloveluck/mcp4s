package mcp4s.agent

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.Json
import munit.CatsEffectSuite

class LlmClientSpec extends CatsEffectSuite:

  private val request = LlmRequest(Nil, Nil, LlmConfig.default)

  test("LlmClient.apply creates client from function") {
    val client = LlmClient[IO](req => IO.pure(LlmResponse.Text("hello")))
    client.complete(request).map { response =>
      assertEquals(response, LlmResponse.Text("hello"))
    }
  }

  test("request passes through faithfully") {
    val config = LlmConfig(systemPrompt = Some("test"), maxTurns = 5)
    val messages = List(Message.User("hi"))
    val client = LlmClient[IO] { req =>
      IO.pure(LlmResponse.Text(s"got ${req.messages.size} messages, maxTurns=${req.config.maxTurns}"))
    }
    val req = LlmRequest(messages, Nil, config)
    client.complete(req).map { response =>
      assertEquals(response, LlmResponse.Text("got 1 messages, maxTurns=5"))
    }
  }

  test("default stream wraps Text response into TextDelta + Done") {
    val client = LlmClient[IO](_ => IO.pure(LlmResponse.Text("hello", Some("endTurn"), Some(Usage(Some(10L))))))
    client.stream(request).compile.toList.map { chunks =>
      assertEquals(chunks.size, 2)
      assertEquals(chunks(0), LlmResponseChunk.TextDelta("hello"))
      assertEquals(chunks(1), LlmResponseChunk.Done(Some("endTurn"), Some(Usage(Some(10L)))))
    }
  }

  test("default stream wraps ToolUse response into ToolCallDelta + Done") {
    val call = ToolCall("id1", "myTool", Json.obj("x" -> Json.fromInt(1)))
    val client = LlmClient[IO](_ => IO.pure(LlmResponse.ToolUse(call, Some("toolUse"), None)))
    client.stream(request).compile.toList.map { chunks =>
      assertEquals(chunks.size, 2)
      val delta = chunks(0).asInstanceOf[LlmResponseChunk.ToolCallDelta]
      assertEquals(delta.index, 0)
      assertEquals(delta.id, Some("id1"))
      assertEquals(delta.name, Some("myTool"))
      assertEquals(chunks(1), LlmResponseChunk.Done(Some("toolUse"), None))
    }
  }

  test("default stream wraps ToolUseMany into multiple ToolCallDeltas + Done") {
    val calls = NonEmptyList.of(
      ToolCall("1", "a", Json.obj()),
      ToolCall("2", "b", Json.obj())
    )
    val client = LlmClient[IO](_ => IO.pure(LlmResponse.ToolUseMany(calls)))
    client.stream(request).compile.toList.map { chunks =>
      assertEquals(chunks.size, 3)
      assert(chunks(0).isInstanceOf[LlmResponseChunk.ToolCallDelta])
      assert(chunks(1).isInstanceOf[LlmResponseChunk.ToolCallDelta])
      assertEquals(chunks(2), LlmResponseChunk.Done(None, None))
    }
  }

  test("LlmClient.streaming factory provides custom stream") {
    val customChunks = List(
      LlmResponseChunk.TextDelta("hel"),
      LlmResponseChunk.TextDelta("lo"),
      LlmResponseChunk.Done(Some("endTurn"))
    )
    val client = LlmClient.streaming[IO](
      completeF = _ => IO.pure(LlmResponse.Text("hello")),
      streamF = _ => fs2.Stream.emits(customChunks)
    )
    client.stream(request).compile.toList.map { chunks =>
      assertEquals(chunks, customChunks)
    }
  }
