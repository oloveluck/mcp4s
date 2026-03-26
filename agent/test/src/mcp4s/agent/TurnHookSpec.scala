package mcp4s.agent

import scala.annotation.targetName
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import io.circe.{Encoder, Json}
import mcp4s.client.McpConnection
import mcp4s.protocol.*
import munit.CatsEffectSuite

class TurnHookSpec extends CatsEffectSuite:

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

  private val noopEmit: AgentEvent => IO[Unit] = _ => IO.unit

  test("identity hook passes messages through unchanged") {
    val hook = TurnHook.identity[IO]
    val msgs = List(Message.User("hello"))
    for
      before <- hook.beforeTurn(TurnView(msgs, 0), noopEmit)
      after  <- hook.afterTurn(TurnView(msgs, 0), noopEmit)
    yield
      assertEquals(before, msgs)
      assertEquals(after, msgs)
  }

  test("before factory fires on beforeTurn only") {
    Ref.of[IO, List[String]](Nil).flatMap { ref =>
      val hook = TurnHook.before[IO] { (view, _) =>
        ref.update(_ :+ s"before-${view.turn}").as(view.messages)
      }
      val msgs = List(Message.User("hi"))
      for
        _ <- hook.beforeTurn(TurnView(msgs, 0), noopEmit)
        _ <- hook.afterTurn(TurnView(msgs, 0), noopEmit)
        log <- ref.get
      yield assertEquals(log, List("before-0"))
    }
  }

  test("after factory fires on afterTurn only") {
    Ref.of[IO, List[String]](Nil).flatMap { ref =>
      val hook = TurnHook.after[IO] { (view, _) =>
        ref.update(_ :+ s"after-${view.turn}").as(view.messages)
      }
      val msgs = List(Message.User("hi"))
      for
        _ <- hook.beforeTurn(TurnView(msgs, 0), noopEmit)
        _ <- hook.afterTurn(TurnView(msgs, 0), noopEmit)
        log <- ref.get
      yield assertEquals(log, List("after-0"))
    }
  }

  test("thinking hook emits Thinking event and appends Assistant message") {
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        IO.pure(LlmResponse.Text("I should use the echo tool"))

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig.default)
      val hook = TurnHook.thinking(ctx, ChainOfThoughtConfig())
      Ref.of[IO, List[AgentEvent]](Nil).flatMap { ref =>
        val emit: AgentEvent => IO[Unit] = event => ref.update(_ :+ event)
        val msgs = List(Message.User("go"))
        for
          result <- hook.beforeTurn(TurnView(msgs, 0), emit)
          events <- ref.get
        yield
          // Should have emitted a Thinking event
          assertEquals(events.size, 1)
          assert(events.head.isInstanceOf[AgentEvent.Thinking])
          // Should have appended an Assistant message
          assertEquals(result.size, 2)
          assertEquals(result.last, Message.Assistant("I should use the echo tool"))
      }
    }
  }

  test("thinking hook with thinkBeforeEveryTurn=false fires only on turn 0") {
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        IO.pure(LlmResponse.Text("thinking..."))

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig.default)
      val config = ChainOfThoughtConfig(thinkBeforeEveryTurn = false)
      val hook = TurnHook.thinking(ctx, config)
      Ref.of[IO, List[AgentEvent]](Nil).flatMap { ref =>
        val emit: AgentEvent => IO[Unit] = event => ref.update(_ :+ event)
        val msgs = List(Message.User("go"))
        for
          result0 <- hook.beforeTurn(TurnView(msgs, 0), emit)
          result1 <- hook.beforeTurn(TurnView(msgs, 1), emit)
          result2 <- hook.beforeTurn(TurnView(msgs, 2), emit)
          events <- ref.get
        yield
          // Only turn 0 should have produced a Thinking event
          assertEquals(events.size, 1)
          // Turn 0 should have appended assistant message
          assertEquals(result0.size, 2)
          // Turn 1 and 2 should pass through unchanged
          assertEquals(result1, msgs)
          assertEquals(result2, msgs)
      }
    }
  }

  test("reflection hook fires on correct turns") {
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        IO.pure(LlmResponse.Text("looks good"))

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig.default)
      val config = ReflectionConfig(reflectEveryNTurns = 2)
      val hook = TurnHook.reflection(ctx, config)
      Ref.of[IO, List[AgentEvent]](Nil).flatMap { ref =>
        val emit: AgentEvent => IO[Unit] = event => ref.update(_ :+ event)
        val msgs = List(Message.User("go"))
        for
          // (turn+1) % 2 == 0: fires on turn 1, 3, 5...
          r0 <- hook.afterTurn(TurnView(msgs, 0), emit) // (0+1)%2 = 1 != 0: no reflection
          r1 <- hook.afterTurn(TurnView(msgs, 1), emit) // (1+1)%2 = 0: reflection!
          r2 <- hook.afterTurn(TurnView(msgs, 2), emit) // (2+1)%2 = 1 != 0: no reflection
          r3 <- hook.afterTurn(TurnView(msgs, 3), emit) // (3+1)%2 = 0: reflection!
          events <- ref.get
        yield
          assertEquals(events.size, 2)
          assert(events.forall(_.isInstanceOf[AgentEvent.Reflection]))
          // Non-reflection turns pass through
          assertEquals(r0, msgs)
          assertEquals(r2, msgs)
          // Reflection turns append assistant message
          assertEquals(r1.size, 2)
          assertEquals(r3.size, 2)
      }
    }
  }

  test("reflection hook emits Reflection event") {
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        IO.pure(LlmResponse.Text("reflecting on progress"))

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig.default)
      val config = ReflectionConfig(reflectEveryNTurns = 1)
      val hook = TurnHook.reflection(ctx, config)
      Ref.of[IO, List[AgentEvent]](Nil).flatMap { ref =>
        val emit: AgentEvent => IO[Unit] = event => ref.update(_ :+ event)
        for
          _ <- hook.afterTurn(TurnView(List(Message.User("go")), 0), emit)
          events <- ref.get
        yield
          assertEquals(events, List(AgentEvent.Reflection("reflecting on progress")))
      }
    }
  }

  test("Semigroup composes both beforeTurn and afterTurn") {
    Ref.of[IO, List[String]](Nil).flatMap { ref =>
      val hook1 = new TurnHook[IO]:
        def beforeTurn(view: TurnView, emit: AgentEvent => IO[Unit]): IO[List[Message]] =
          ref.update(_ :+ "before1").as(view.messages)
        def afterTurn(view: TurnView, emit: AgentEvent => IO[Unit]): IO[List[Message]] =
          ref.update(_ :+ "after1").as(view.messages)

      val hook2 = new TurnHook[IO]:
        def beforeTurn(view: TurnView, emit: AgentEvent => IO[Unit]): IO[List[Message]] =
          ref.update(_ :+ "before2").as(view.messages)
        def afterTurn(view: TurnView, emit: AgentEvent => IO[Unit]): IO[List[Message]] =
          ref.update(_ :+ "after2").as(view.messages)

      val combined = hook1 |+| hook2
      val msgs = List(Message.User("hi"))
      for
        _ <- combined.beforeTurn(TurnView(msgs, 0), noopEmit)
        _ <- combined.afterTurn(TurnView(msgs, 0), noopEmit)
        log <- ref.get
      yield assertEquals(log, List("before1", "before2", "after1", "after2"))
    }
  }

  test("combined thinking |+| reflection produces both events in loop") {
    var callCount = 0
    val llm = new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] = IO {
        callCount += 1
        if request.tools.isEmpty then
          LlmResponse.Text("hook response")
        else if callCount <= 3 then
          // callCount 1: thinking (no tools), 2: tool call, 3: thinking (no tools)
          LlmResponse.ToolUse(ToolCall(s"c$callCount", "echo", Json.obj()))
        else
          LlmResponse.Text("Done")
      }

    mockConnection((_, _) => IO.pure(ToolResult.text("ok"))).flatMap { conn =>
      val ctx = LoopContext(llm, conn, dummySchemas, LlmConfig(maxTurns = 10))
      val hook = TurnHook.thinking(ctx, ChainOfThoughtConfig()) |+|
        TurnHook.reflection(ctx, ReflectionConfig(reflectEveryNTurns = 1))
      val loop = ToolLoop(ctx, hook)
      collectEvents(loop, List(Message.User("Go"))).map { events =>
        val thinkings = events.collect { case t: AgentEvent.Thinking => t }
        val reflections = events.collect { case r: AgentEvent.Reflection => r }
        assert(thinkings.nonEmpty, s"Expected Thinking events, got: $events")
        assert(reflections.nonEmpty, s"Expected Reflection events, got: $events")
      }
    }
  }
