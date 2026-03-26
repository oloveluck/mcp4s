package mcp4s.agent

import cats.effect.{IO, Ref}
import io.circe.Json
import mcp4s.protocol.*
import munit.CatsEffectSuite

class LlmSamplingSpec extends CatsEffectSuite:

  private def capturingLlm(ref: Ref[IO, List[LlmRequest]], response: LlmResponse): LlmClient[IO] =
    new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        ref.update(_ :+ request).as(response)

  private def mkParams(msgs: SamplingMessage*): CreateMessageParams =
    CreateMessageParams(messages = msgs.toList, maxTokens = 100)

  // === Message conversion (SamplingMessage → Message) ===

  test("text content converts to User/Assistant message") {
    Ref.of[IO, List[LlmRequest]](Nil).flatMap { captured =>
      val llm = capturingLlm(captured, LlmResponse.Text("reply"))
      val sampling = llm.asSampling("test-model")
      val params = mkParams(
        SamplingMessage(Role.User, SamplingTextContent("hello")),
        SamplingMessage(Role.Assistant, SamplingTextContent("world"))
      )
      sampling.handle(params).value.flatMap { _ =>
        captured.get.map { reqs =>
          val msgs = reqs.head.messages
          assertEquals(msgs(0), Message.User("hello"))
          assertEquals(msgs(1), Message.Assistant("world"))
        }
      }
    }
  }

  test("image content converts to [image] placeholder") {
    Ref.of[IO, List[LlmRequest]](Nil).flatMap { captured =>
      val llm = capturingLlm(captured, LlmResponse.Text("ok"))
      val sampling = llm.asSampling("m")
      val params = mkParams(SamplingMessage(Role.User, SamplingImageContent("base64data", "image/png")))
      sampling.handle(params).value.flatMap { _ =>
        captured.get.map { reqs =>
          assertEquals(reqs.head.messages, List(Message.User("[image]")))
        }
      }
    }
  }

  test("audio content converts to [audio] placeholder") {
    Ref.of[IO, List[LlmRequest]](Nil).flatMap { captured =>
      val llm = capturingLlm(captured, LlmResponse.Text("ok"))
      val sampling = llm.asSampling("m")
      val params = mkParams(SamplingMessage(Role.User, SamplingAudioContent("base64data", "audio/wav")))
      sampling.handle(params).value.flatMap { _ =>
        captured.get.map { reqs =>
          assertEquals(reqs.head.messages, List(Message.User("[audio]")))
        }
      }
    }
  }

  test("ToolUseContent converts to formatted string") {
    Ref.of[IO, List[LlmRequest]](Nil).flatMap { captured =>
      val llm = capturingLlm(captured, LlmResponse.Text("ok"))
      val sampling = llm.asSampling("m")
      val input = Json.obj("key" -> Json.fromString("val"))
      val params = mkParams(SamplingMessage(Role.User, ToolUseContent("id-1", "toolName", input)))
      sampling.handle(params).value.flatMap { _ =>
        captured.get.map { reqs =>
          assertEquals(reqs.head.messages, List(Message.User(s"[tool_use: toolName(id-1) ${input.noSpaces}]")))
        }
      }
    }
  }

  test("ToolResultContent converts to formatted string") {
    Ref.of[IO, List[LlmRequest]](Nil).flatMap { captured =>
      val llm = capturingLlm(captured, LlmResponse.Text("ok"))
      val sampling = llm.asSampling("m")
      val params = mkParams(SamplingMessage(Role.User,
        ToolResultContent("id-1", List(TextContent("result")), isError = false)
      ))
      sampling.handle(params).value.flatMap { _ =>
        captured.get.map { reqs =>
          assertEquals(reqs.head.messages, List(Message.User("[tool_result: id-1] result")))
        }
      }
    }
  }

  test("ToolResultContent with isError=true uses error prefix") {
    Ref.of[IO, List[LlmRequest]](Nil).flatMap { captured =>
      val llm = capturingLlm(captured, LlmResponse.Text("ok"))
      val sampling = llm.asSampling("m")
      val params = mkParams(SamplingMessage(Role.User,
        ToolResultContent("id-1", List(TextContent("fail reason")), isError = true)
      ))
      sampling.handle(params).value.flatMap { _ =>
        captured.get.map { reqs =>
          assertEquals(reqs.head.messages, List(Message.User("[tool_error: id-1] fail reason")))
        }
      }
    }
  }

  // === Response conversion (LlmResponse → CreateMessageResult) ===

  test("Text response maps to SamplingTextContent with endTurn") {
    val llm: LlmClient[IO] = LlmClient[IO](_ => IO.pure(LlmResponse.Text("hi")))
    val sampling = llm.asSampling("test-model")
    sampling.handle(mkParams(SamplingMessage(Role.User, SamplingTextContent("q")))).value.map { result =>
      val r = result.get
      assertEquals(r.content, SamplingTextContent("hi"))
      assertEquals(r.stopReason, Some("endTurn"))
      assertEquals(r.model, "test-model")
      assertEquals(r.role, Role.Assistant)
    }
  }

  test("Text response preserves explicit stopReason") {
    val llm: LlmClient[IO] = LlmClient[IO](_ => IO.pure(LlmResponse.Text("hi", stopReason = Some("maxTokens"))))
    val sampling = llm.asSampling("m")
    sampling.handle(mkParams(SamplingMessage(Role.User, SamplingTextContent("q")))).value.map { result =>
      assertEquals(result.get.stopReason, Some("maxTokens"))
    }
  }

  test("ToolUse response maps to formatted text with toolUse stopReason") {
    val call = ToolCall("c1", "myTool", Json.obj("a" -> Json.fromInt(1)))
    val llm: LlmClient[IO] = LlmClient[IO](_ => IO.pure(LlmResponse.ToolUse(call)))
    val sampling = llm.asSampling("m")
    sampling.handle(mkParams(SamplingMessage(Role.User, SamplingTextContent("q")))).value.map { result =>
      val r = result.get
      r.content match
        case SamplingTextContent(text) =>
          assert(text.contains("[tool_use: myTool(c1)"))
          assert(text.contains(call.arguments.noSpaces))
        case other => fail(s"Expected SamplingTextContent, got $other")
      assertEquals(r.stopReason, Some("toolUse"))
    }
  }

  test("ToolUseMany response maps to newline-separated tool uses") {
    import cats.data.NonEmptyList
    val c1 = ToolCall("c1", "tool1", Json.obj())
    val c2 = ToolCall("c2", "tool2", Json.obj())
    val llm: LlmClient[IO] = LlmClient[IO](_ => IO.pure(LlmResponse.ToolUseMany(NonEmptyList.of(c1, c2))))
    val sampling = llm.asSampling("m")
    sampling.handle(mkParams(SamplingMessage(Role.User, SamplingTextContent("q")))).value.map { result =>
      val r = result.get
      r.content match
        case SamplingTextContent(text) =>
          val lines = text.split("\n").toList
          assertEquals(lines.size, 2)
          assert(lines(0).contains("tool1(c1)"))
          assert(lines(1).contains("tool2(c2)"))
        case other => fail(s"Expected SamplingTextContent, got $other")
      assertEquals(r.stopReason, Some("toolUse"))
    }
  }

  // === Config ===

  test("asSampling with explicit model uses that model name") {
    val llm: LlmClient[IO] = LlmClient[IO](_ => IO.pure(LlmResponse.Text("hi")))
    val sampling = llm.asSampling("gpt-4")
    sampling.handle(mkParams(SamplingMessage(Role.User, SamplingTextContent("q")))).value.map { result =>
      assertEquals(result.get.model, "gpt-4")
    }
  }

  test("asSampling with LlmConfig without model defaults to unknown") {
    val llm: LlmClient[IO] = LlmClient[IO](_ => IO.pure(LlmResponse.Text("hi")))
    val sampling = llm.asSampling(LlmConfig())
    sampling.handle(mkParams(SamplingMessage(Role.User, SamplingTextContent("q")))).value.map { result =>
      assertEquals(result.get.model, "unknown")
    }
  }
