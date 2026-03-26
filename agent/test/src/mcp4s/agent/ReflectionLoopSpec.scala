package mcp4s.agent

import scala.annotation.targetName
import cats.effect.{IO, Ref}
import io.circe.{Encoder, Json}
import mcp4s.client.McpConnection
import mcp4s.protocol.*
import munit.CatsEffectSuite

class ReflectionLoopSpec extends CatsEffectSuite:

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

  private def collectEvents(loop: AgentLoop[IO], messages: List[Message]): IO[List[AgentEvent]] =
    Ref.of[IO, List[AgentEvent]](Nil).flatMap { ref =>
      val emit: AgentEvent => IO[Unit] = event => ref.update(_ :+ event)
      loop.run(messages, emit) *> ref.get
    }

  test("reflection fires after N tool turns") {
    // reflectEveryNTurns = 2: after 2 tool turns, reflect, then text finish
    var callCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        callCount += 1
        // Calls 1-2: tool use; call 3: reflection response (no tools); call 4: text finish
        if request.tools.isEmpty then
          // This is a reflection call
          LlmResponse.Text("Looks good so far.")
        else if callCount <= 2 then
          LlmResponse.ToolUse(ToolCall(s"c$callCount", "echo", Json.obj()))
        else
          LlmResponse.Text("All done")
      }

    val config = ReflectionConfig(reflectEveryNTurns = 2)
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig(maxTurns = 10))
      val loop = ReflectionLoop(ctx, config)
      collectEvents(loop, List(Message.User("Go"))).map { events =>
        val reflections = events.collect { case r: AgentEvent.Reflection => r }
        assertEquals(reflections.size, 1)
        assertEquals(reflections.head.content, "Looks good so far.")
        // Should also have a Finished at the end
        val finished = events.collect { case f: AgentEvent.Finished => f }
        assertEquals(finished.size, 1)
      }
    }
  }

  test("reflection does not count toward maxTurns") {
    // maxTurns=3, reflectEvery=1: tool, reflect, tool, reflect, tool, reflect, text
    // Without reflection not counting, this would fail after 3 tool turns
    var toolCallCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        if request.tools.isEmpty then
          LlmResponse.Text("Reflecting...")
        else
          toolCallCount += 1
          if toolCallCount <= 3 then
            LlmResponse.ToolUse(ToolCall(s"c$toolCallCount", "echo", Json.obj()))
          else
            LlmResponse.Text("Finished after 3 tool turns and reflections")
        }

    val config = ReflectionConfig(reflectEveryNTurns = 1)
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig(maxTurns = 4))
      val loop = ReflectionLoop(ctx, config)
      collectEvents(loop, List(Message.User("Go"))).map { events =>
        val reflections = events.collect { case r: AgentEvent.Reflection => r }
        assertEquals(reflections.size, 3)
        val finished = events.collect { case f: AgentEvent.Finished => f }
        assertEquals(finished.size, 1)
        assertEquals(finished.head.content, "Finished after 3 tool turns and reflections")
      }
    }
  }

  test("event ordering: ToolCalled, ToolResult, Reflection, ToolCalled, ToolResult, Finished") {
    var toolCallCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        if request.tools.isEmpty then
          LlmResponse.Text("reflection text")
        else
          toolCallCount += 1
          if toolCallCount <= 1 then
            LlmResponse.ToolUse(ToolCall("c1", "echo", Json.obj()))
          else
            LlmResponse.Text("Done")
      }

    val config = ReflectionConfig(reflectEveryNTurns = 1)
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig(maxTurns = 10))
      val loop = ReflectionLoop(ctx, config)
      collectEvents(loop, List(Message.User("Go"))).map { events =>
        // Expected: ToolCalled, ToolResultReceived, Reflection, Finished(Done)
        assertEquals(events.size, 4)
        assert(events(0).isInstanceOf[AgentEvent.ToolCalled])
        assert(events(1).isInstanceOf[AgentEvent.ToolResultReceived])
        assert(events(2).isInstanceOf[AgentEvent.Reflection])
        assert(events(3) == AgentEvent.Finished("Done"))
      }
    }
  }

  test("maxTurns exceeded emits Finished with max turns message") {
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        if request.tools.isEmpty then IO.pure(LlmResponse.Text("reflecting"))
        else IO.pure(LlmResponse.ToolUse(ToolCall("id", "echo", Json.obj())))

    val config = ReflectionConfig(reflectEveryNTurns = 5)
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig(maxTurns = 2))
      val loop = ReflectionLoop(ctx, config)
      collectEvents(loop, List(Message.User("Go"))).map { events =>
        val finished = events.collect { case f: AgentEvent.Finished => f }
        assertEquals(finished.size, 1)
        assert(finished.head.content.contains("Max turns"))
      }
    }
  }

  test("text-only LLM with reflection loop emits only Finished") {
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        IO.pure(LlmResponse.Text("Hello!"))

    val config = ReflectionConfig(reflectEveryNTurns = 2)
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig.default)
      val loop = ReflectionLoop(ctx, config)
      collectEvents(loop, List(Message.User("Hi"))).map { events =>
        assertEquals(events, List(AgentEvent.Finished("Hello!")))
      }
    }
  }

  test("builder withReflection creates agent with ReflectionLoop") {
    var callCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        callCount += 1
        if request.tools.isEmpty then
          LlmResponse.Text("reflection")
        else if callCount <= 1 then
          LlmResponse.ToolUse(ToolCall("c1", "echo", Json.obj()))
        else
          LlmResponse.Text("Done")
      }

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      Agent.builder[IO](llm, conn)
        .withReflection(ReflectionConfig(reflectEveryNTurns = 1))
        .withToolSchemas(dummySchemas)
        .build
        .flatMap { agent =>
          agent.run("Go").compile.toList.map { events =>
            val reflections = events.collect { case r: AgentEvent.Reflection => r }
            assertEquals(reflections.size, 1)
            assert(events.last == AgentEvent.Finished("Done"))
          }
        }
    }
  }
