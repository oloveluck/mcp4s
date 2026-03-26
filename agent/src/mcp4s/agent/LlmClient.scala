package mcp4s.agent

import cats.effect.Concurrent
import cats.syntax.all.*
import mcp4s.client.Samplings
import mcp4s.protocol.*

/** Algebra for LLM interactions.
  *
  * Implementations adapt specific LLM providers (OpenAI, Anthropic, etc.)
  * to a uniform interface consumed by [[Agent]].
  */
trait LlmClient[F[_]]:
  /** Send a completion request and receive the full response. */
  def complete(request: LlmRequest): F[LlmResponse]

  /** Stream a completion request as incremental chunks.
    *
    * The default implementation wraps `complete` into a single-element stream
    * of synthetic chunks. Override for true streaming.
    */
  def stream(request: LlmRequest)(using Concurrent[F]): fs2.Stream[F, LlmResponseChunk] =
    fs2.Stream.eval(complete(request)).flatMap(LlmClient.toChunks(_))

object LlmClient:
  def apply[F[_]](f: LlmRequest => F[LlmResponse]): LlmClient[F] =
    new LlmClient[F]:
      def complete(request: LlmRequest): F[LlmResponse] = f(request)

  /** Create an LlmClient that provides both `complete` and a custom `stream`. */
  def streaming[F[_]](
      completeF: LlmRequest => F[LlmResponse],
      streamF: LlmRequest => fs2.Stream[F, LlmResponseChunk]
  ): LlmClient[F] =
    new LlmClient[F]:
      def complete(request: LlmRequest): F[LlmResponse] = completeF(request)
      override def stream(request: LlmRequest)(using Concurrent[F]): fs2.Stream[F, LlmResponseChunk] =
        streamF(request)

  /** Convert a complete LlmResponse into synthetic chunks. */
  private[agent] def toChunks[F[_]](response: LlmResponse): fs2.Stream[F, LlmResponseChunk] =
    response match
      case LlmResponse.Text(content, stopReason, usage) =>
        fs2.Stream(
          LlmResponseChunk.TextDelta(content),
          LlmResponseChunk.Done(stopReason, usage)
        )
      case LlmResponse.ToolUse(call, stopReason, usage) =>
        fs2.Stream(
          LlmResponseChunk.ToolCallDelta(0, Some(call.id), Some(call.name), call.arguments.noSpaces),
          LlmResponseChunk.Done(stopReason, usage)
        )
      case LlmResponse.ToolUseMany(calls, stopReason, usage) =>
        val deltas = calls.toList.zipWithIndex.map { case (call, idx) =>
          LlmResponseChunk.ToolCallDelta(idx, Some(call.id), Some(call.name), call.arguments.noSpaces)
        }
        fs2.Stream.emits(deltas) ++ fs2.Stream(LlmResponseChunk.Done(stopReason, usage))

  private[agent] def samplingMessageToMessage(msg: SamplingMessage): Message =
    val text = msg.content match
      case SamplingTextContent(t)     => t
      case SamplingImageContent(_, _) => "[image]"
      case SamplingAudioContent(_, _) => "[audio]"
      case ToolUseContent(id, name, input) =>
        s"[tool_use: $name($id) ${input.noSpaces}]"
      case ToolResultContent(toolUseId, content, isError) =>
        val prefix = if isError then "[tool_error" else "[tool_result"
        val text = content.collect { case TextContent(t, _, _) => t }.mkString("; ")
        s"$prefix: $toolUseId] $text"
    msg.role match
      case Role.User      => Message.User(text)
      case Role.Assistant => Message.Assistant(text)

  private[agent] def responseToResult(response: LlmResponse, model: String): CreateMessageResult =
    response match
      case LlmResponse.Text(content, stopReason, _) =>
        CreateMessageResult(
          role = Role.Assistant,
          content = SamplingTextContent(content),
          model = model,
          stopReason = stopReason.orElse(Some("endTurn"))
        )
      case LlmResponse.ToolUse(call, stopReason, _) =>
        CreateMessageResult(
          role = Role.Assistant,
          content = SamplingTextContent(s"[tool_use: ${call.name}(${call.id}) ${call.arguments.noSpaces}]"),
          model = model,
          stopReason = stopReason.orElse(Some("toolUse"))
        )
      case LlmResponse.ToolUseMany(calls, stopReason, _) =>
        val text = calls.toList.map(c => s"[tool_use: ${c.name}(${c.id}) ${c.arguments.noSpaces}]").mkString("\n")
        CreateMessageResult(
          role = Role.Assistant,
          content = SamplingTextContent(text),
          model = model,
          stopReason = stopReason.orElse(Some("toolUse"))
        )

extension [F[_]: Concurrent](client: LlmClient[F])

  /** Create a `Samplings` handler that delegates to this LLM client.
    *
    * Converts MCP sampling types to agent types, calls `complete`,
    * and converts the response back to MCP types.
    *
    * @param model model name to include in the `CreateMessageResult`
    */
  def asSampling(model: String): Samplings[F] =
    Samplings[F] { params =>
      val messages = params.messages.map(LlmClient.samplingMessageToMessage)
      val config = LlmConfig(
        systemPrompt = params.systemPrompt,
        temperature = params.temperature,
        maxTokens = Some(params.maxTokens)
      )
      val tools = params.tools.getOrElse(Nil).map(ToolSchema.fromTool)
      val request = LlmRequest(messages, tools, config)
      client.complete(request).map(LlmClient.responseToResult(_, model))
    }

  /** Create a `Samplings` handler, reading the model name from config.
    *
    * Falls back to "unknown" if `config.model` is not set.
    */
  def asSampling(config: LlmConfig): Samplings[F] =
    asSampling(config.model.getOrElse("unknown"))
