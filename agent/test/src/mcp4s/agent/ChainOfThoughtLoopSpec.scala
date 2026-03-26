package mcp4s.agent

import scala.annotation.targetName
import cats.effect.{IO, Ref}
import io.circe.{Encoder, Json}
import mcp4s.client.McpConnection
import mcp4s.protocol.*
import munit.CatsEffectSuite

class ChainOfThoughtLoopSpec extends CatsEffectSuite:

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

  test("thinking fires before tool turn") {
    var callCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        callCount += 1
        if request.tools.isEmpty then
          LlmResponse.Text("I should use echo tool.")
        else if callCount <= 3 then
          // callCount 2 is the first tool call (after thinking)
          LlmResponse.ToolUse(ToolCall("c1", "echo", Json.obj()))
        else
          LlmResponse.Text("Done")
      }

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig(maxTurns = 10))
      val loop = ChainOfThoughtLoop(ctx, ChainOfThoughtConfig())
      collectEvents(loop, List(Message.User("Go"))).map { events =>
        // First event should be Thinking
        assert(events.head.isInstanceOf[AgentEvent.Thinking])
        // Then ToolCalled
        assert(events(1).isInstanceOf[AgentEvent.ToolCalled])
      }
    }
  }

  test("thinking does not count toward maxTurns") {
    // maxTurns=2, thinkBeforeEveryTurn=true
    // Flow: think, tool1, think, tool2, maxTurns exceeded
    // If thinking DID count, we'd exhaust maxTurns before completing 2 tool turns.
    // We verify that 2 tool calls complete (proving thinking didn't consume the budget).
    var toolCallCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        if request.tools.isEmpty then
          LlmResponse.Text("Thinking...")
        else
          toolCallCount += 1
          LlmResponse.ToolUse(ToolCall(s"c$toolCallCount", "echo", Json.obj()))
        }

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig(maxTurns = 2))
      val loop = ChainOfThoughtLoop(ctx, ChainOfThoughtConfig())
      collectEvents(loop, List(Message.User("Go"))).map { events =>
        val thinkings = events.collect { case t: AgentEvent.Thinking => t }
        assertEquals(thinkings.size, 2)  // before each of 2 tool turns
        val toolCalls = events.collect { case t: AgentEvent.ToolCalled => t }
        assertEquals(toolCalls.size, 2)  // 2 tool turns completed (thinking didn't count)
        val finished = events.collect { case f: AgentEvent.Finished => f }
        assertEquals(finished.size, 1)
        assert(finished.head.content.contains("Max turns"))
      }
    }
  }

  test("event ordering: Thinking, ToolCalled, ToolResult, Thinking, Finished") {
    var callCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        callCount += 1
        if request.tools.isEmpty then
          LlmResponse.Text("thinking text")
        else if callCount <= 2 then
          // callCount 2 = first tool call (after first thinking at callCount 1)
          LlmResponse.ToolUse(ToolCall("c1", "echo", Json.obj()))
        else
          LlmResponse.Text("Done")
      }

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig(maxTurns = 10))
      val loop = ChainOfThoughtLoop(ctx, ChainOfThoughtConfig())
      collectEvents(loop, List(Message.User("Go"))).map { events =>
        assertEquals(events.size, 5)
        assert(events(0).isInstanceOf[AgentEvent.Thinking])
        assert(events(1).isInstanceOf[AgentEvent.ToolCalled])
        assert(events(2).isInstanceOf[AgentEvent.ToolResultReceived])
        assert(events(3).isInstanceOf[AgentEvent.Thinking])
        assert(events(4) == AgentEvent.Finished("Done"))
      }
    }
  }

  test("thinkBeforeEveryTurn=false thinks only once") {
    var toolCallCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        if request.tools.isEmpty then
          LlmResponse.Text("initial thinking")
        else
          toolCallCount += 1
          if toolCallCount <= 2 then
            LlmResponse.ToolUse(ToolCall(s"c$toolCallCount", "echo", Json.obj()))
          else
            LlmResponse.Text("Done")
      }

    val config = ChainOfThoughtConfig(thinkBeforeEveryTurn = false)
    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig(maxTurns = 10))
      val loop = ChainOfThoughtLoop(ctx, config)
      collectEvents(loop, List(Message.User("Go"))).map { events =>
        val thinkings = events.collect { case t: AgentEvent.Thinking => t }
        assertEquals(thinkings.size, 1)
        // Should still have tool calls and finish
        val toolCalls = events.collect { case t: AgentEvent.ToolCalled => t }
        assertEquals(toolCalls.size, 2)
        assert(events.last == AgentEvent.Finished("Done"))
      }
    }
  }

  test("maxTurns exceeded emits Finished with max turns message") {
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        if request.tools.isEmpty then IO.pure(LlmResponse.Text("thinking"))
        else IO.pure(LlmResponse.ToolUse(ToolCall("id", "echo", Json.obj())))

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig(maxTurns = 2))
      val loop = ChainOfThoughtLoop(ctx, ChainOfThoughtConfig())
      collectEvents(loop, List(Message.User("Go"))).map { events =>
        val finished = events.collect { case f: AgentEvent.Finished => f }
        assertEquals(finished.size, 1)
        assert(finished.head.content.contains("Max turns"))
      }
    }
  }

  test("text-only LLM emits Thinking then Finished") {
    var callCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        callCount += 1
        if request.tools.isEmpty then
          LlmResponse.Text("let me think about this")
        else
          LlmResponse.Text("Hello!")
      }

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig.default)
      val loop = ChainOfThoughtLoop(ctx, ChainOfThoughtConfig())
      collectEvents(loop, List(Message.User("Hi"))).map { events =>
        assertEquals(events.size, 2)
        assert(events(0).isInstanceOf[AgentEvent.Thinking])
        assert(events(1) == AgentEvent.Finished("Hello!"))
      }
    }
  }

  test("builder withChainOfThought creates working agent") {
    var callCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        callCount += 1
        if request.tools.isEmpty then
          LlmResponse.Text("thinking")
        else if callCount <= 2 then
          LlmResponse.ToolUse(ToolCall("c1", "echo", Json.obj()))
        else
          LlmResponse.Text("Done")
      }

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      Agent.builder[IO](llm, conn)
        .withChainOfThought
        .withToolSchemas(dummySchemas)
        .build
        .flatMap { agent =>
          agent.run("Go").compile.toList.map { events =>
            val thinkings = events.collect { case t: AgentEvent.Thinking => t }
            assert(thinkings.nonEmpty)
            assert(events.last == AgentEvent.Finished("Done"))
          }
        }
    }
  }
