package mcp4s.agent

import cats.data.{Chain, NonEmptyList}
import io.circe.Json
import mcp4s.protocol.{JsonSchema, Tool}
import munit.CatsEffectSuite

class ModelSpec extends CatsEffectSuite:

  test("LlmConfig.default has expected values") {
    val cfg = LlmConfig.default
    assertEquals(cfg.systemPrompt, None)
    assertEquals(cfg.temperature, None)
    assertEquals(cfg.maxTokens, None)
    assertEquals(cfg.maxTurns, 10)
    assertEquals(cfg.model, None)
  }

  test("LlmConfig.withModel sets model") {
    val cfg = LlmConfig.default.withModel("gpt-4")
    assertEquals(cfg.model, Some("gpt-4"))
  }

  test("ToolSchema.fromTool extracts name, description, inputSchema") {
    val tool = Tool(
      name = "calc",
      description = Some("A calculator"),
      inputSchema = JsonSchema.empty
    )
    val schema = ToolSchema.fromTool(tool)
    assertEquals(schema.name, "calc")
    assertEquals(schema.description, Some("A calculator"))
    assertEquals(schema.inputSchema, JsonSchema.empty)
  }

  test("ToolSchema.fromTools converts a list") {
    val tools = List(
      Tool(name = "a", inputSchema = JsonSchema.empty),
      Tool(name = "b", description = Some("B tool"), inputSchema = JsonSchema.empty)
    )
    val schemas = ToolSchema.fromTools(tools)
    assertEquals(schemas.size, 2)
    assertEquals(schemas.map(_.name), List("a", "b"))
  }

  test("ToolCall construction") {
    val call = ToolCall("id-1", "myTool", Json.obj("x" -> Json.fromInt(1)))
    assertEquals(call.id, "id-1")
    assertEquals(call.name, "myTool")
    assertEquals(call.arguments, Json.obj("x" -> Json.fromInt(1)))
  }

  test("LlmResponse.ToolUseMany holds NonEmptyList") {
    val calls = NonEmptyList.of(
      ToolCall("1", "a", Json.obj()),
      ToolCall("2", "b", Json.obj())
    )
    val response = LlmResponse.ToolUseMany(calls)
    assertEquals(response.calls.size, 2)
  }

  test("AgentEvent variants construct correctly") {
    val call = ToolCall("id", "tool", Json.obj())
    val toolCalled = AgentEvent.ToolCalled(call)
    assertEquals(toolCalled.call, call)

    val result = AgentEvent.ToolResultReceived("id", "tool", Json.fromString("ok"))
    assertEquals(result.id, "id")

    val finished = AgentEvent.Finished("done")
    assertEquals(finished.content, "done")

    val reflection = AgentEvent.Reflection("looks good")
    assertEquals(reflection.content, "looks good")

    val thinking = AgentEvent.Thinking("let me consider")
    assertEquals(thinking.content, "let me consider")

    val compressed = AgentEvent.ContextCompressed(Tokens(1000L), Tokens(500L), 20, 10)
    assertEquals(compressed.tokensBefore.value, 1000L)
    assertEquals(compressed.tokensAfter.value, 500L)
    assertEquals(compressed.messagesBefore, 20)
    assertEquals(compressed.messagesAfter, 10)
  }

  test("Message sealed trait variants") {
    val user: Message = Message.User("hello")
    val assistant: Message = Message.Assistant("hi")
    val toolUse: Message = Message.ToolUse(NonEmptyList.one(ToolCall("1", "t", Json.obj())))
    val toolResult: Message = Message.ToolResult("1", "t", Json.fromString("ok"))

    assert(user.isInstanceOf[Message.User])
    assert(assistant.isInstanceOf[Message.Assistant])
    assert(toolUse.isInstanceOf[Message.ToolUse])
    assert(toolResult.isInstanceOf[Message.ToolResult])
  }

  test("LlmConfig builder methods") {
    val cfg = LlmConfig.default
      .withSystemPrompt("You are helpful.")
      .withTemperature(0.7)
      .withMaxTokens(1024)
      .withMaxTurns(20)
    assertEquals(cfg.systemPrompt, Some("You are helpful."))
    assertEquals(cfg.temperature, Some(0.7))
    assertEquals(cfg.maxTokens, Some(1024))
    assertEquals(cfg.maxTurns, 20)
  }

  test("LoopState.fromMessages creates state with turn 0") {
    val msgs = List(Message.User("hello"), Message.Assistant("hi"))
    val state = LoopState.fromMessages(msgs)
    assertEquals(state.toMessageList, msgs)
    assertEquals(state.turn, 0)
  }

  test("LoopState.appendMessage appends a single message") {
    val state = LoopState.fromMessages(List(Message.User("hello")))
    val updated = state.appendMessage(Message.Assistant("hi"))
    assertEquals(updated.toMessageList, List(Message.User("hello"), Message.Assistant("hi")))
    assertEquals(updated.turn, 0)
  }

  test("LoopState.appendMessages appends multiple messages") {
    val state = LoopState.fromMessages(List(Message.User("hello")))
    val newMsgs = List(Message.Assistant("hi"), Message.Assistant("there"))
    val updated = state.appendMessages(newMsgs)
    assertEquals(updated.toMessageList.size, 3)
    assertEquals(updated.toMessageList.last, Message.Assistant("there"))
  }

  test("LoopState.incrementTurn advances turn counter") {
    val state = LoopState.fromMessages(Nil)
    assertEquals(state.turn, 0)
    assertEquals(state.incrementTurn.turn, 1)
    assertEquals(state.incrementTurn.incrementTurn.turn, 2)
  }

  test("LoopState.toView creates TurnView snapshot") {
    val msgs = List(Message.User("hello"))
    val state = LoopState(Chain.fromSeq(msgs), turn = 3)
    val view = state.toView
    assertEquals(view.messages, msgs)
    assertEquals(view.turn, 3)
  }

  test("TurnView construction") {
    val msgs = List(Message.User("hello"), Message.Assistant("world"))
    val view = TurnView(msgs, 5)
    assertEquals(view.messages, msgs)
    assertEquals(view.turn, 5)
  }

  test("Usage construction and defaults") {
    val empty = Usage()
    assertEquals(empty.promptTokens, None)
    assertEquals(empty.completionTokens, None)

    val full = Usage(promptTokens = Some(100L), completionTokens = Some(50L))
    assertEquals(full.promptTokens, Some(100L))
    assertEquals(full.completionTokens, Some(50L))
  }

  test("LlmResponse.Text with metadata fields") {
    val simple = LlmResponse.Text("hello")
    assertEquals(simple.content, "hello")
    assertEquals(simple.stopReason, None)
    assertEquals(simple.usage, None)

    val withMeta = LlmResponse.Text("hello", Some("endTurn"), Some(Usage(Some(10L), Some(5L))))
    assertEquals(withMeta.stopReason, Some("endTurn"))
    assertEquals(withMeta.usage.flatMap(_.promptTokens), Some(10L))
  }

  test("LlmResponse sealed trait exposes stopReason and usage") {
    val text: LlmResponse = LlmResponse.Text("hi", Some("stop"), Some(Usage()))
    assertEquals(text.stopReason, Some("stop"))
    assert(text.usage.isDefined)

    val toolUse: LlmResponse = LlmResponse.ToolUse(ToolCall("1", "t", Json.obj()), Some("toolUse"), None)
    assertEquals(toolUse.stopReason, Some("toolUse"))
    assertEquals(toolUse.usage, None)
  }

  test("LlmResponseChunk variants") {
    val textDelta = LlmResponseChunk.TextDelta("hello")
    assertEquals(textDelta.content, "hello")

    val toolDelta = LlmResponseChunk.ToolCallDelta(0, Some("id1"), Some("tool1"), """{"x":1}""")
    assertEquals(toolDelta.index, 0)
    assertEquals(toolDelta.id, Some("id1"))
    assertEquals(toolDelta.name, Some("tool1"))
    assertEquals(toolDelta.argumentsDelta, """{"x":1}""")

    val done = LlmResponseChunk.Done(Some("endTurn"), Some(Usage(Some(100L))))
    assertEquals(done.stopReason, Some("endTurn"))
    assertEquals(done.usage.flatMap(_.promptTokens), Some(100L))

    val emptyDone = LlmResponseChunk.Done()
    assertEquals(emptyDone.stopReason, None)
    assertEquals(emptyDone.usage, None)
  }
