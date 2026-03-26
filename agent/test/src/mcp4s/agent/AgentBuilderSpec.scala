package mcp4s.agent

import scala.annotation.targetName
import cats.effect.{IO, Ref}
import io.circe.{Encoder, Json}
import mcp4s.client.{Elicitations, McpConnection, Roots}
import mcp4s.protocol.*
import mcp4s.server.{mcp => dsl}
import munit.CatsEffectSuite

class AgentBuilderSpec extends CatsEffectSuite:

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

  // === Loop resolution ===

  test("withLoop overrides accumulated hooks") {
    Ref.of[IO, List[String]](Nil).flatMap { log =>
      val llm = textOnlyLlm("hi")
      mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
        Agent.builder[IO](llm, conn)
          .withToolSchemas(dummySchemas)
          .withHook(_ => TurnHook.before[IO] { (view, _) =>
            log.update(_ :+ "hook-before").as(view.messages)
          })
          .withLoop { ctx =>
            AgentLoop[IO] { (messages, emit) =>
              log.update(_ :+ "custom-loop") *>
                emit(AgentEvent.Finished("custom")) *>
                IO.pure(messages)
            }
          }
          .build
          .flatMap { agent =>
            agent.run("Go").compile.toList.flatMap { events =>
              log.get.map { entries =>
                assert(entries.contains("custom-loop"))
                assert(!entries.contains("hook-before"))
                assertEquals(events, List(AgentEvent.Finished("custom")))
              }
            }
          }
      }
    }
  }

  test("multiple withHook calls compose left-to-right") {
    Ref.of[IO, List[String]](Nil).flatMap { log =>
      Ref.of[IO, Int](0).flatMap { callCount =>
        val llm = countingToolUseLlm(callCount, 1) // one tool use then text
        mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
          Agent.builder[IO](llm, conn)
            .withToolSchemas(dummySchemas)
            .withHook(_ => TurnHook.before[IO] { (view, _) =>
              log.update(_ :+ "hook-A").as(view.messages)
            })
            .withHook(_ => TurnHook.before[IO] { (view, _) =>
              log.update(_ :+ "hook-B").as(view.messages)
            })
            .build
            .flatMap { agent =>
              agent.run("Go").compile.toList.flatMap { _ =>
                log.get.map { entries =>
                  // First turn: A then B, second turn: A then B
                  val filtered = entries.filter(e => e == "hook-A" || e == "hook-B")
                  assertEquals(filtered.take(2), List("hook-A", "hook-B"))
                }
              }
            }
        }
      }
    }
  }

  test("withMiddleware wraps resolved loop including hooks") {
    Ref.of[IO, List[String]](Nil).flatMap { log =>
      val llm = textOnlyLlm("hi")
      mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
        val mw = LoopMiddleware[IO] { (messages, _, next) =>
          log.update(_ :+ "mw-before") *> next.flatTap(_ => log.update(_ :+ "mw-after"))
        }
        Agent.builder[IO](llm, conn)
          .withToolSchemas(dummySchemas)
          .withHook(_ => TurnHook.before[IO] { (view, _) =>
            log.update(_ :+ "hook-before").as(view.messages)
          })
          .withMiddleware(mw)
          .build
          .flatMap { agent =>
            agent.run("Go").compile.toList.flatMap { _ =>
              log.get.map { entries =>
                assertEquals(entries, List("mw-before", "hook-before", "mw-after"))
              }
            }
          }
      }
    }
  }

  test("multiple withMiddleware calls compose (first is outermost)") {
    Ref.of[IO, List[String]](Nil).flatMap { log =>
      val llm = textOnlyLlm("hi")
      mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
        val mw1 = LoopMiddleware[IO] { (messages, _, next) =>
          log.update(_ :+ "mw1-before") *> next.flatTap(_ => log.update(_ :+ "mw1-after"))
        }
        val mw2 = LoopMiddleware[IO] { (messages, _, next) =>
          log.update(_ :+ "mw2-before") *> next.flatTap(_ => log.update(_ :+ "mw2-after"))
        }
        Agent.builder[IO](llm, conn)
          .withToolSchemas(dummySchemas)
          .withMiddleware(mw1)
          .withMiddleware(mw2)
          .build
          .flatMap { agent =>
            agent.run("Go").compile.toList.flatMap { _ =>
              log.get.map { entries =>
                assertEquals(entries, List("mw1-before", "mw2-before", "mw2-after", "mw1-after"))
              }
            }
          }
      }
    }
  }

  // LLM that returns ToolUse for tool-equipped requests and Text for hook requests (no tools)
  private def hookAwareLlm(ref: Ref[IO, Int], toolUsesBeforeText: Int): LlmClient[IO] =
    new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        if request.tools.isEmpty then
          // Hook call (thinking/reflection) — no tools provided
          IO.pure(LlmResponse.Text("thought"))
        else
          ref.getAndUpdate(_ + 1).map { count =>
            if count < toolUsesBeforeText then
              LlmResponse.ToolUse(ToolCall(s"call-$count", "echo", Json.obj()))
            else
              LlmResponse.Text("Done")
          }

  // === Combined strategies ===

  test("withReflection + withChainOfThought composes both hooks") {
    Ref.of[IO, Int](0).flatMap { callCount =>
      // Need 4+ tool turns to trigger reflection (default reflectEveryNTurns=3)
      val llm = hookAwareLlm(callCount, 4)
      mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
        Agent.builder[IO](llm, conn)
          .withToolSchemas(dummySchemas)
          .withConfig(LlmConfig(maxTurns = 10))
          .withChainOfThought
          .withReflection
          .build
          .flatMap { agent =>
            agent.run("Go").compile.toList.map { events =>
              val thinkingEvents = events.collect { case t: AgentEvent.Thinking => t }
              val reflectionEvents = events.collect { case r: AgentEvent.Reflection => r }
              assert(thinkingEvents.nonEmpty, "Expected at least one Thinking event")
              assert(reflectionEvents.nonEmpty, "Expected at least one Reflection event")
            }
          }
      }
    }
  }

  test("withChainOfThought(thinkBeforeEveryTurn=false) thinks only on turn 0") {
    Ref.of[IO, Int](0).flatMap { callCount =>
      val llm = hookAwareLlm(callCount, 3) // 3 tool turns then text
      mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
        Agent.builder[IO](llm, conn)
          .withToolSchemas(dummySchemas)
          .withChainOfThought(ChainOfThoughtConfig(thinkBeforeEveryTurn = false))
          .build
          .flatMap { agent =>
            agent.run("Go").compile.toList.map { events =>
              val thinkingEvents = events.collect { case t: AgentEvent.Thinking => t }
              assertEquals(thinkingEvents.size, 1)
            }
          }
      }
    }
  }

  test("withReflection(reflectEveryNTurns=1) reflects after every tool turn") {
    Ref.of[IO, Int](0).flatMap { callCount =>
      val llm = hookAwareLlm(callCount, 3)
      mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
        Agent.builder[IO](llm, conn)
          .withToolSchemas(dummySchemas)
          .withReflection(ReflectionConfig(reflectEveryNTurns = 1))
          .build
          .flatMap { agent =>
            agent.run("Go").compile.toList.map { events =>
              val reflectionEvents = events.collect { case r: AgentEvent.Reflection => r }
              assertEquals(reflectionEvents.size, 3)
            }
          }
      }
    }
  }

  // === Context window ===

  test("withContextWindow triggers compression when over budget") {
    Ref.of[IO, Int](0).flatMap { callCount =>
      // Use a text LLM but inject a long system prompt via beforeTurn to blow the budget
      val llm = textOnlyLlm("done")
      mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
        val tinyBudget = TokenBudget(maxTokens = 20, reservedForResponse = 0) // ~20 tokens available
        Agent.builder[IO](llm, conn)
          .withToolSchemas(dummySchemas)
          .withContextWindow(tinyBudget, _ => ContextPolicy.slidingWindow[IO])
          .build
          .flatMap { agent =>
            // A long prompt that exceeds the tiny budget (>80 chars = ~20 tokens at 4 chars/token)
            val longPrompt = "x" * 200
            agent.run(longPrompt).compile.toList.map { events =>
              val compressed = events.collect { case c: AgentEvent.ContextCompressed => c }
              assertEquals(compressed.size, 1)
              assert(compressed.head.tokensAfter <= compressed.head.tokensBefore)
            }
          }
      }
    }
  }

  test("withContextWindow passes through when within budget") {
    val llm = textOnlyLlm("done")
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val largeBudget = TokenBudget(maxTokens = 100000, reservedForResponse = 0)
      Agent.builder[IO](llm, conn)
        .withToolSchemas(dummySchemas)
        .withContextWindow(largeBudget, _ => ContextPolicy.slidingWindow[IO])
        .build
        .flatMap { agent =>
          agent.run("short").compile.toList.map { events =>
            val compressed = events.collect { case c: AgentEvent.ContextCompressed => c }
            assertEquals(compressed.size, 0)
            assertEquals(events, List(AgentEvent.Finished("done")))
          }
        }
    }
  }

  // === Server-side configuration ===

  test("withServerTools called twice composes via |+|") {
    val tool1 = dsl.Tool.text[IO]("ping", "Ping") { "pong" }
    val tool2 = dsl.Tool.text[IO]("status", "Status") { "ok" }
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      Agent.builder[IO](textOnlyLlm("hi"), conn)
        .withToolSchemas(dummySchemas)
        .withServerTools(tool1)
        .withServerTools(tool2)
        .build
        .flatMap(_.toServer)
        .flatMap { server =>
          server.listTools.map { tools =>
            val names = tools.map(_.name).toSet
            assert(names.contains("ping"))
            assert(names.contains("status"))
          }
        }
    }
  }

  test("withServerResources + withServerPrompts are exposed on toServer") {
    val resources = dsl.Resource[IO]("test://info", "Info resource") {
      IO.pure(ResourceContent.text("test://info", "hello"))
    }
    val prompts = dsl.Prompt[IO]("greet", "Greeting prompt")(
      PromptMessage(Role.User, TextContent("hi"))
    )
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      Agent.builder[IO](textOnlyLlm("hi"), conn)
        .withToolSchemas(dummySchemas)
        .withServerResources(resources)
        .withServerPrompts(prompts)
        .build
        .flatMap(_.toServer)
        .flatMap { server =>
          for
            resList <- server.listResources
            promptList <- server.listPrompts
          yield
            assertEquals(resList.map(_.uri), List("test://info"))
            assertEquals(promptList.map(_.name), List("greet"))
        }
    }
  }

  // === Capability advertisement ===

  test("capabilities reflect registered handlers") {
    val llm = textOnlyLlm("hi")
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      // With all handlers
      Agent.builder[IO](llm, conn)
        .withToolSchemas(dummySchemas)
        .withSampling(llm.asSampling("test"))
        .withRoots(Roots[IO]("file:///tmp", "tmp"))
        .withElicitation(Elicitations[IO](_ => IO.pure(ElicitResult(ElicitAction.Accept))))
        .build
        .flatMap { agent =>
          assert(agent.capabilities.sampling.isDefined, "sampling should be Some")
          assert(agent.capabilities.roots.isDefined, "roots should be Some")
          assert(agent.capabilities.elicitation.isDefined, "elicitation should be Some")
          // Without handlers
          Agent.builder[IO](llm, conn)
            .withToolSchemas(dummySchemas)
            .build
            .map { agent2 =>
              assert(agent2.capabilities.sampling.isEmpty, "sampling should be None")
              assert(agent2.capabilities.roots.isEmpty, "roots should be None")
              assert(agent2.capabilities.elicitation.isEmpty, "elicitation should be None")
            }
        }
    }
  }
