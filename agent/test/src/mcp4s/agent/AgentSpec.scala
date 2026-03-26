package mcp4s.agent

import scala.annotation.targetName
import cats.data.NonEmptyList
import cats.effect.{IO, Ref}
import io.circe.{Encoder, Json}
import mcp4s.client.McpConnection
import mcp4s.protocol.*
import munit.CatsEffectSuite

class AgentSpec extends CatsEffectSuite:

  private val dummyTool = Tool(name = "echo", description = Some("Echo"), inputSchema = JsonSchema.empty)
  private val dummyTools = List(dummyTool)

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


  test("text-only response emits single Finished event") {
    val llm = textOnlyLlm("Hello!")
    mockConnection((_, _) => IO.pure(ToolResult.text("unused"))).flatMap { conn =>
      val agent = Agent.fromSchemas[IO](llm, conn, ToolSchema.fromTools(dummyTools))
      agent.run("Hi").compile.toList.map { events =>
        assertEquals(events, List(AgentEvent.Finished("Hello!")))
      }
    }
  }

  test("tool-use-then-text emits ToolCalled, ToolResultReceived, Finished") {
    var callCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        callCount += 1
        if callCount == 1 then
          LlmResponse.ToolUse(ToolCall("call-1", "echo", Json.obj("msg" -> Json.fromString("hi"))))
        else
          LlmResponse.Text("Done")
      }

    mockConnection { (name, args) =>
      IO.pure(ToolResult.text(s"echoed: ${args.noSpaces}"))
    }.flatMap { conn =>
      val agent = Agent.fromSchemas[IO](llm, conn, ToolSchema.fromTools(dummyTools))
      agent.run("Go").compile.toList.map { events =>
        assertEquals(events.size, 3)
        assert(events(0).isInstanceOf[AgentEvent.ToolCalled])
        assert(events(1).isInstanceOf[AgentEvent.ToolResultReceived])
        assert(events(2) == AgentEvent.Finished("Done"))
      }
    }
  }

  test("maxTurns enforcement terminates after N turns") {
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        IO.pure(LlmResponse.ToolUse(ToolCall("id", "echo", Json.obj())))

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val config = LlmConfig(maxTurns = 2)
      val agent = Agent.fromSchemas[IO](llm, conn, ToolSchema.fromTools(dummyTools), config)
      agent.run("Go").compile.toList.map { events =>
        val finished = events.collect { case f: AgentEvent.Finished => f }
        assertEquals(finished.size, 1)
        assert(finished.head.content.contains("Max turns"))
      }
    }
  }

  test("parallel dispatch emits multiple ToolCalled and ToolResultReceived for ToolUseMany") {
    var callCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        callCount += 1
        if callCount == 1 then
          LlmResponse.ToolUseMany(NonEmptyList.of(
            ToolCall("c1", "echo", Json.obj("n" -> Json.fromInt(1))),
            ToolCall("c2", "echo", Json.obj("n" -> Json.fromInt(2)))
          ))
        else
          LlmResponse.Text("All done")
      }

    mockConnection { (_, args) =>
      IO.pure(ToolResult.text(s"result-${args.noSpaces}"))
    }.flatMap { conn =>
      val agent = Agent.fromSchemas[IO](llm, conn, ToolSchema.fromTools(dummyTools))
      agent.run("Go").compile.toList.map { events =>
        val toolCalled = events.collect { case tc: AgentEvent.ToolCalled => tc }
        val toolResults = events.collect { case tr: AgentEvent.ToolResultReceived => tr }
        assertEquals(toolCalled.size, 2)
        assertEquals(toolResults.size, 2)
        assert(events.last == AgentEvent.Finished("All done"))
      }
    }
  }

  test("message history includes Message.ToolUse and Message.ToolResult on second LLM call") {
    var capturedRequests = List.empty[LlmRequest]
    var callCount = 0

    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        capturedRequests = capturedRequests :+ request
        callCount += 1
        if callCount == 1 then
          LlmResponse.ToolUse(ToolCall("c1", "echo", Json.obj()))
        else
          LlmResponse.Text("Final")
      }

    mockConnection((_, _) => IO.pure(ToolResult.text("tool-output"))).flatMap { conn =>
      val agent = Agent.fromSchemas[IO](llm, conn, ToolSchema.fromTools(dummyTools))
      agent.run("Start").compile.toList.map { _ =>
        assertEquals(capturedRequests.size, 2)
        val secondMessages = capturedRequests(1).messages
        assertEquals(secondMessages.size, 3)
        assert(secondMessages(0).isInstanceOf[Message.User])
        assert(secondMessages(1).isInstanceOf[Message.ToolUse])
        assert(secondMessages(2).isInstanceOf[Message.ToolResult])
      }
    }
  }

  test("Agent.create fetches tools from connection") {
    val llm = textOnlyLlm("hi")
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      Agent.create[IO](llm, conn).flatMap { agent =>
        agent.run("test").compile.toList.map { events =>
          assertEquals(events, List(AgentEvent.Finished("hi")))
        }
      }
    }
  }

  test("structuredContent is preferred over text in tool result conversion") {
    var callCount = 0
    val structuredJson = Json.obj("key" -> Json.fromString("value"))

    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        callCount += 1
        if callCount == 1 then
          LlmResponse.ToolUse(ToolCall("c1", "echo", Json.obj()))
        else
          LlmResponse.Text("Done")
      }

    mockConnection { (_, _) =>
      IO.pure(ToolResult(
        content = List(TextContent("fallback")),
        structuredContent = Some(structuredJson)
      ))
    }.flatMap { conn =>
      val agent = Agent.fromSchemas[IO](llm, conn, ToolSchema.fromTools(dummyTools))
      agent.run("Go").compile.toList.map { events =>
        val results = events.collect { case tr: AgentEvent.ToolResultReceived => tr }
        assertEquals(results.head.content, structuredJson)
      }
    }
  }

  test("Agent without sampling raises MethodNotSupported on createMessage") {
    val llm = textOnlyLlm("hi")
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val agent = Agent.fromSchemas[IO](llm, conn, ToolSchema.fromTools(dummyTools))
      val params = CreateMessageParams(
        messages = List(SamplingMessage(Role.User, SamplingTextContent("hello"))),
        maxTokens = 100
      )
      agent.createMessage(params).attempt.map { result =>
        assert(result.isLeft)
        assert(result.left.exists(_.isInstanceOf[McpError.MethodNotSupported]))
      }
    }
  }

  test("Agent with builder and withDefaultSampling delegates createMessage to LLM") {
    val llm = textOnlyLlm("LLM response")
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      Agent.builder[IO](llm, conn)
        .withDefaultSampling("test-model")
        .withToolSchemas(ToolSchema.fromTools(dummyTools))
        .build
        .flatMap { agent =>
          val params = CreateMessageParams(
            messages = List(SamplingMessage(Role.User, SamplingTextContent("hello"))),
            maxTokens = 100
          )
          agent.createMessage(params).map { result =>
            assertEquals(result.model, "test-model")
            assertEquals(result.role, Role.Assistant)
            result.content match
              case SamplingTextContent(text) => assertEquals(text, "LLM response")
              case other                     => fail(s"Expected SamplingTextContent, got $other")
          }
        }
    }
  }
