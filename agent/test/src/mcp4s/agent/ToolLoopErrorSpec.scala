package mcp4s.agent

import scala.annotation.targetName
import cats.data.NonEmptyList
import cats.effect.{IO, Ref}
import io.circe.{Encoder, Json}
import mcp4s.client.McpConnection
import mcp4s.protocol.*
import munit.CatsEffectSuite

class ToolLoopErrorSpec extends CatsEffectSuite:

  private val dummyTool = Tool(name = "echo", description = Some("Echo"), inputSchema = JsonSchema.empty)
  private val dummyTools = List(dummyTool)
  private val dummySchemas = ToolSchema.fromTools(dummyTools)

  private def mockConnection(handler: (String, Json) => IO[ToolResult]): IO[McpConnection[IO]] =
    Ref.of[IO, Map[RequestId, ProgressParams => IO[Unit]]](Map.empty).map { pHandlers =>
      new McpConnection[IO]:
        def serverInfo: ServerInfo = ServerInfo("mock", "0.0.0")
        def serverCapabilities: ServerCapabilities = ServerCapabilities()
        def progressHandlers: Ref[IO, Map[RequestId, ProgressParams => IO[Unit]]] = pHandlers
        def listTools: IO[List[Tool]] = IO.pure(dummyTools)
        def callTool[A: Encoder](name: ToolName, arguments: A): IO[ToolResult] =
          handler(name.value, Encoder[A].apply(arguments))
        @targetName("callToolString")
        def callTool[A: Encoder](name: String, arguments: A): IO[ToolResult] =
          handler(name, Encoder[A].apply(arguments))
        def callTool[A: Encoder](name: ToolName, arguments: A, onProgress: ProgressParams => IO[Unit]): IO[ToolResult] =
          handler(name.value, Encoder[A].apply(arguments))
        @targetName("callToolStringWithProgress")
        def callTool[A: Encoder](name: String, arguments: A, onProgress: ProgressParams => IO[Unit]): IO[ToolResult] =
          handler(name, Encoder[A].apply(arguments))
        def callToolIfSupported[A: Encoder](name: ToolName, arguments: A): IO[Option[ToolResult]] =
          callTool(name, arguments).map(Some(_))
        def listResources: IO[List[Resource]] = IO.pure(Nil)
        def listResourceTemplates: IO[List[ResourceTemplate]] = IO.pure(Nil)
        def readResource(uri: ResourceUri): IO[ResourceContent] =
          IO.raiseError(McpError.ResourceNotFound(uri.value))
        @targetName("readResourceString")
        def readResource(uri: String): IO[ResourceContent] =
          IO.raiseError(McpError.ResourceNotFound(uri))
        def readResourceIfSupported(uri: ResourceUri): IO[Option[ResourceContent]] = IO.pure(None)
        def listPrompts: IO[List[Prompt]] = IO.pure(Nil)
        def getPrompt[A: Encoder](name: PromptName, arguments: A): IO[GetPromptResult] =
          IO.raiseError(McpError.PromptNotFound(name.value))
        @targetName("getPromptString")
        def getPrompt[A: Encoder](name: String, arguments: A): IO[GetPromptResult] =
          IO.raiseError(McpError.PromptNotFound(name))
        def getPromptIfSupported[A: Encoder](name: PromptName, arguments: A): IO[Option[GetPromptResult]] = IO.pure(None)
        def ping: IO[Unit] = IO.unit
        def shutdown: IO[Unit] = IO.unit
        def cancel(requestId: RequestId, reason: Option[String]): IO[Unit] = IO.unit
    }

  private def textOnlyLlm(response: String): LlmClient[IO] =
    new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        IO.pure(LlmResponse.Text(response))

  private def countingToolUseLlm(ref: Ref[IO, Int], toolUsesBeforeText: Int): LlmClient[IO] =
    new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        ref.getAndUpdate(_ + 1).map { count =>
          if count < toolUsesBeforeText then
            LlmResponse.ToolUse(ToolCall(s"call-$count", "echo", Json.obj()))
          else
            LlmResponse.Text("Done")
        }

  // === Error propagation ===

  test("LLM error on first call propagates to stream") {
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        IO.raiseError(new RuntimeException("llm-boom"))

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val agent = Agent.fromSchemas[IO](llm, conn, dummySchemas)
      agent.run("Hi").compile.toList.attempt.map { result =>
        assert(result.isLeft)
        assertEquals(result.left.toOption.get.getMessage, "llm-boom")
      }
    }
  }

  test("LLM error on second call (after successful tool use) propagates") {
    Ref.of[IO, Int](0).flatMap { callCount =>
      val llm = new LlmClient[IO]:
        def complete(request: LlmRequest): IO[LlmResponse] =
          callCount.getAndUpdate(_ + 1).flatMap { count =>
            if count == 0 then
              IO.pure(LlmResponse.ToolUse(ToolCall("c1", "echo", Json.obj())))
            else
              IO.raiseError(new RuntimeException("llm-boom-2"))
          }

      mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
        val agent = Agent.fromSchemas[IO](llm, conn, dummySchemas)
        agent.run("Go").compile.toList.attempt.map { result =>
          assert(result.isLeft)
          assertEquals(result.left.toOption.get.getMessage, "llm-boom-2")
        }
      }
    }
  }

  test("tool execution error propagates to stream") {
    Ref.of[IO, Int](0).flatMap { callCount =>
      val llm = new LlmClient[IO]:
        def complete(request: LlmRequest): IO[LlmResponse] =
          callCount.getAndUpdate(_ + 1).map { count =>
            if count == 0 then
              LlmResponse.ToolUse(ToolCall("c1", "echo", Json.obj()))
            else
              LlmResponse.Text("Done")
          }

      mockConnection((_, _) => IO.raiseError(new RuntimeException("tool-boom"))).flatMap { conn =>
        val agent = Agent.fromSchemas[IO](llm, conn, dummySchemas)
        agent.run("Go").compile.toList.attempt.map { result =>
          assert(result.isLeft)
          assertEquals(result.left.toOption.get.getMessage, "tool-boom")
        }
      }
    }
  }

  test("tool execution error with ToolUseMany propagates") {
    Ref.of[IO, Int](0).flatMap { toolCallCount =>
      val llm = new LlmClient[IO]:
        def complete(request: LlmRequest): IO[LlmResponse] =
          IO.pure(LlmResponse.ToolUseMany(NonEmptyList.of(
            ToolCall("c1", "echo", Json.obj()),
            ToolCall("c2", "fail", Json.obj())
          )))

      mockConnection { (name, _) =>
        if name == "fail" then IO.raiseError(new RuntimeException("parallel-boom"))
        else IO.pure(ToolResult.text("ok"))
      }.flatMap { conn =>
        val agent = Agent.fromSchemas[IO](llm, conn, dummySchemas)
        agent.run("Go").compile.toList.attempt.map { result =>
          assert(result.isLeft)
          assertEquals(result.left.toOption.get.getMessage, "parallel-boom")
        }
      }
    }
  }

  // === catchErrors middleware ===

  test("catchErrors middleware converts LLM error to Finished event") {
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        IO.raiseError(new RuntimeException("llm-fail"))

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      Agent.builder[IO](llm, conn)
        .withToolSchemas(dummySchemas)
        .withMiddleware(LoopMiddleware.catchErrors[IO](_ => IO.unit))
        .build
        .flatMap { agent =>
          agent.run("Hi").compile.toList.map { events =>
            assertEquals(events.size, 1)
            val finished = events.head.asInstanceOf[AgentEvent.Finished]
            assert(finished.content.contains("llm-fail"))
          }
        }
    }
  }

  test("catchErrors middleware converts tool error to Finished event") {
    Ref.of[IO, Int](0).flatMap { callCount =>
      val llm = new LlmClient[IO]:
        def complete(request: LlmRequest): IO[LlmResponse] =
          callCount.getAndUpdate(_ + 1).map { count =>
            if count == 0 then
              LlmResponse.ToolUse(ToolCall("c1", "echo", Json.obj()))
            else
              LlmResponse.Text("Done")
          }

      mockConnection((_, _) => IO.raiseError(new RuntimeException("tool-fail"))).flatMap { conn =>
        Agent.builder[IO](llm, conn)
          .withToolSchemas(dummySchemas)
          .withMiddleware(LoopMiddleware.catchErrors[IO](_ => IO.unit))
          .build
          .flatMap { agent =>
            agent.run("Go").compile.toList.map { events =>
              val finished = events.collect { case f: AgentEvent.Finished => f }
              assertEquals(finished.size, 1)
              assert(finished.head.content.contains("tool-fail"))
            }
          }
      }
    }
  }

  // === Edge cases ===

  test("maxTurns = 0 emits Finished immediately without calling LLM") {
    Ref.of[IO, Int](0).flatMap { callCount =>
      val llm = new LlmClient[IO]:
        def complete(request: LlmRequest): IO[LlmResponse] =
          callCount.update(_ + 1).as(LlmResponse.Text("should-not-run"))

      mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
        val config = LlmConfig(maxTurns = 0)
        val agent = Agent.fromSchemas[IO](llm, conn, dummySchemas, config)
        agent.run("Hi").compile.toList.flatMap { events =>
          assertEquals(events.size, 1)
          assert(events.head.asInstanceOf[AgentEvent.Finished].content.contains("Max turns"))
          callCount.get.map(count => assertEquals(count, 0))
        }
      }
    }
  }

  test("maxTurns = 1 allows exactly one tool turn then terminates") {
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        IO.pure(LlmResponse.ToolUse(ToolCall("id", "echo", Json.obj())))

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val config = LlmConfig(maxTurns = 1)
      val agent = Agent.fromSchemas[IO](llm, conn, dummySchemas, config)
      agent.run("Go").compile.toList.map { events =>
        val toolCalled = events.collect { case tc: AgentEvent.ToolCalled => tc }
        val toolResults = events.collect { case tr: AgentEvent.ToolResultReceived => tr }
        val finished = events.collect { case f: AgentEvent.Finished => f }
        assertEquals(toolCalled.size, 1)
        assertEquals(toolResults.size, 1)
        assertEquals(finished.size, 1)
        assert(finished.head.content.contains("Max turns"))
      }
    }
  }

  test("text response on first call terminates immediately") {
    Ref.of[IO, Int](0).flatMap { callCount =>
      val llm = new LlmClient[IO]:
        def complete(request: LlmRequest): IO[LlmResponse] =
          callCount.update(_ + 1).as(LlmResponse.Text("immediate"))

      mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
        val agent = Agent.fromSchemas[IO](llm, conn, dummySchemas)
        agent.run("Hi").compile.toList.flatMap { events =>
          assertEquals(events, List(AgentEvent.Finished("immediate")))
          callCount.get.map(count => assertEquals(count, 1))
        }
      }
    }
  }

  // === Hook error handling ===

  test("beforeTurn hook error propagates to stream") {
    val llm = textOnlyLlm("ok")
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      Agent.builder[IO](llm, conn)
        .withToolSchemas(dummySchemas)
        .withHook(_ => TurnHook.before[IO]((_, _) =>
          IO.raiseError(new RuntimeException("hook-before-boom"))
        ))
        .build
        .flatMap { agent =>
          agent.run("Hi").compile.toList.attempt.map { result =>
            assert(result.isLeft)
            assertEquals(result.left.toOption.get.getMessage, "hook-before-boom")
          }
        }
    }
  }

  test("afterTurn hook error propagates to stream") {
    Ref.of[IO, Int](0).flatMap { callCount =>
      val llm = new LlmClient[IO]:
        def complete(request: LlmRequest): IO[LlmResponse] =
          callCount.getAndUpdate(_ + 1).map { count =>
            if count == 0 then
              LlmResponse.ToolUse(ToolCall("c1", "echo", Json.obj()))
            else
              LlmResponse.Text("Done")
          }

      mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
        Agent.builder[IO](llm, conn)
          .withToolSchemas(dummySchemas)
          .withHook(_ => TurnHook.after[IO]((_, _) =>
            IO.raiseError(new RuntimeException("hook-after-boom"))
          ))
          .build
          .flatMap { agent =>
            agent.run("Go").compile.toList.attempt.map { result =>
              assert(result.isLeft)
              assertEquals(result.left.toOption.get.getMessage, "hook-after-boom")
            }
          }
      }
    }
  }

  test("hook receives correct turn number across iterations") {
    Ref.of[IO, List[Int]](Nil).flatMap { turnLog =>
      Ref.of[IO, Int](0).flatMap { callCount =>
        val llm = countingToolUseLlm(callCount, 3) // 3 tool uses then text

        mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
          Agent.builder[IO](llm, conn)
            .withToolSchemas(dummySchemas)
            .withHook(_ => TurnHook.before[IO] { (view, _) =>
              turnLog.update(_ :+ view.turn).as(view.messages)
            })
            .build
            .flatMap { agent =>
              agent.run("Go").compile.toList.flatMap { _ =>
                turnLog.get.map { turns =>
                  // turn 0 (tool use), turn 1 (tool use), turn 2 (tool use), turn 3 (text)
                  assertEquals(turns, List(0, 1, 2, 3))
                }
              }
            }
        }
      }
    }
  }
