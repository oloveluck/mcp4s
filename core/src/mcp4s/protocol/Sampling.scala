package mcp4s.protocol

import io.circe.Json

/** Sampling types for server-initiated LLM requests
  * Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/client/sampling
  * Spec ref: schema.ts sampling/createMessage
  */

/** Content that can appear in sampling messages
  * Includes text, image, audio, and tool use/result blocks
  */
sealed trait SamplingContent

final case class SamplingTextContent(
    text: String
) extends SamplingContent

final case class SamplingImageContent(
    data: String,     // Base64 encoded
    mimeType: String
) extends SamplingContent

final case class SamplingAudioContent(
    data: String,     // Base64 encoded
    mimeType: String
) extends SamplingContent

/** Tool use content for when the model wants to call a tool
  * Spec ref: schema.ts ToolUseContent
  */
final case class ToolUseContent(
    id: String,
    name: String,
    input: Json
) extends SamplingContent

/** Tool result content for returning tool call results
  * Spec ref: schema.ts ToolResultContent
  */
final case class ToolResultContent(
    toolUseId: String,
    content: List[Content],
    isError: Boolean = false
) extends SamplingContent

/** A message in a sampling conversation
  * Spec ref: schema.ts SamplingMessage
  */
final case class SamplingMessage(
    role: Role,
    content: SamplingContent,
    _meta: Option[Json] = None
)

/** Model selection preferences
  * Spec ref: schema.ts ModelPreferences
  */
final case class ModelPreferences(
    hints: Option[List[ModelHint]] = None,
    costPriority: Option[Double] = None,        // 0.0-1.0
    speedPriority: Option[Double] = None,       // 0.0-1.0
    intelligencePriority: Option[Double] = None // 0.0-1.0
)

/** Hint for model selection
  * Spec ref: schema.ts ModelHint
  */
final case class ModelHint(
    name: Option[String] = None
)

/** Tool choice specification
  * Spec ref: schema.ts ToolChoice
  */
sealed trait ToolChoice

object ToolChoice:
  case object Auto extends ToolChoice
  case object None extends ToolChoice
  final case class Specific(name: String) extends ToolChoice

/** Parameters for sampling/createMessage request
  * Spec ref: schema.ts CreateMessageRequestParams
  */
final case class CreateMessageParams(
    messages: List[SamplingMessage],
    maxTokens: Int,
    modelPreferences: Option[ModelPreferences] = None,
    systemPrompt: Option[String] = None,
    includeContext: Option[String] = None,  // "none" | "thisServer" | "allServers"
    temperature: Option[Double] = None,
    stopSequences: Option[List[String]] = None,
    metadata: Option[Json] = None,
    tools: Option[List[Tool]] = None,
    toolChoice: Option[ToolChoice] = None
)

/** Result of sampling/createMessage
  * Spec ref: schema.ts CreateMessageResult
  */
final case class CreateMessageResult(
    role: Role,
    content: SamplingContent,
    model: String,
    stopReason: Option[String] = None,  // "endTurn" | "stopSequence" | "maxTokens" | "toolUse"
    _meta: Option[Json] = None
)
