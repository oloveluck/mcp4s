package mcp4s.agent

import cats.data.{Chain, NonEmptyList}
import io.circe.Json
import mcp4s.protocol.{JsonSchema, Tool}

/** LLM configuration for agent loops. */
final case class LlmConfig(
    systemPrompt: Option[String] = None,
    temperature: Option[Double] = None,
    maxTokens: Option[Int] = None,
    maxTurns: Int = 10,
    model: Option[String] = None
):
  def withSystemPrompt(prompt: String): LlmConfig = copy(systemPrompt = Some(prompt))
  def withTemperature(temp: Double): LlmConfig = copy(temperature = Some(temp))
  def withMaxTokens(tokens: Int): LlmConfig = copy(maxTokens = Some(tokens))
  def withMaxTurns(turns: Int): LlmConfig = copy(maxTurns = turns)
  def withModel(model: String): LlmConfig = copy(model = Some(model))

object LlmConfig:
  val default: LlmConfig = LlmConfig()

/** Token usage metadata from an LLM response. */
final case class Usage(
    promptTokens: Option[Long] = None,
    completionTokens: Option[Long] = None
)

/** Simplified tool schema projection for LLM providers. */
final case class ToolSchema(
    name: String,
    description: Option[String],
    inputSchema: JsonSchema
)

object ToolSchema:
  def fromTool(tool: Tool): ToolSchema =
    ToolSchema(tool.name, tool.description, tool.inputSchema)

  def fromTools(tools: List[Tool]): List[ToolSchema] =
    tools.map(fromTool)

/** A tool call request with id, name, and JSON arguments. */
final case class ToolCall(
    id: String,
    name: String,
    arguments: Json
)

/** Conversation message types for agent history. */
sealed trait Message

object Message:
  final case class User(content: String) extends Message
  final case class Assistant(content: String) extends Message
  final case class ToolUse(calls: NonEmptyList[ToolCall]) extends Message
  final case class ToolResult(id: String, name: String, content: Json) extends Message

/** Request sent to an LLM. */
final case class LlmRequest(
    messages: List[Message],
    tools: List[ToolSchema],
    config: LlmConfig
)

/** Response from an LLM. */
sealed trait LlmResponse:
  def stopReason: Option[String]
  def usage: Option[Usage]

object LlmResponse:
  final case class Text(content: String, stopReason: Option[String] = None, usage: Option[Usage] = None) extends LlmResponse
  final case class ToolUse(call: ToolCall, stopReason: Option[String] = None, usage: Option[Usage] = None) extends LlmResponse
  final case class ToolUseMany(calls: NonEmptyList[ToolCall], stopReason: Option[String] = None, usage: Option[Usage] = None) extends LlmResponse

/** Incremental chunk from an LLM streaming response. */
sealed trait LlmResponseChunk

object LlmResponseChunk:
  final case class TextDelta(content: String) extends LlmResponseChunk
  final case class ToolCallDelta(index: Int, id: Option[String] = None, name: Option[String] = None, argumentsDelta: String = "") extends LlmResponseChunk
  final case class Done(stopReason: Option[String] = None, usage: Option[Usage] = None) extends LlmResponseChunk

/** Read-only snapshot of loop state for TurnHook implementations. */
final case class TurnView(messages: List[Message], turn: Int)

/** Internal accumulating state for agent loop iterations.
  * Uses Chain[Message] for O(1) append during the tool-calling cycle.
  */
private[agent] final case class LoopState(
    messages: Chain[Message],
    turn: Int
):
  def appendMessage(msg: Message): LoopState =
    copy(messages = messages :+ msg)
  def appendMessages(msgs: Iterable[Message]): LoopState =
    copy(messages = messages ++ Chain.fromSeq(msgs.toSeq))
  def incrementTurn: LoopState =
    copy(turn = turn + 1)
  def toMessageList: List[Message] =
    messages.toList
  def toView: TurnView =
    TurnView(toMessageList, turn)

private[agent] object LoopState:
  def fromMessages(msgs: List[Message]): LoopState =
    LoopState(Chain.fromSeq(msgs), turn = 0)

/** Events emitted by the agent loop. */
sealed trait AgentEvent

object AgentEvent:
  final case class ToolCalled(call: ToolCall) extends AgentEvent
  final case class ToolResultReceived(id: String, name: String, content: Json) extends AgentEvent
  final case class Finished(content: String) extends AgentEvent
  final case class Reflection(content: String) extends AgentEvent
  final case class Thinking(content: String) extends AgentEvent
  final case class ContextCompressed(
      tokensBefore: Tokens,
      tokensAfter: Tokens,
      messagesBefore: Int,
      messagesAfter: Int
  ) extends AgentEvent
