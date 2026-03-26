package mcp4s.agent

import cats.data.{Chain, NonEmptyList}
import cats.effect.Concurrent
import cats.syntax.all.*
import mcp4s.protocol.ToolResult

/** Standard tool-calling loop extracted from the original `Agent` implementation.
  *
  * Repeatedly sends conversation history + tool schemas to the LLM.
  * When the LLM responds with tool calls, executes them via the MCP connection
  * and appends results to history. Terminates when the LLM returns text or
  * `maxTurns` is reached.
  */
object ToolLoop:

  def apply[F[_]: Concurrent](ctx: LoopContext[F]): AgentLoop[F] =
    new AgentLoop[F]:
      def run(messages: List[Message], emit: AgentEvent => F[Unit]): F[List[Message]] =
        Concurrent[F].tailRecM(LoopState.fromMessages(messages)) { state =>
          if state.turn >= ctx.config.maxTurns then
            emit(AgentEvent.Finished(s"Max turns (${ctx.config.maxTurns}) exceeded"))
              .as(Right(state.toMessageList))
          else
            val request = LlmRequest(state.toMessageList, ctx.toolSchemas, ctx.config)
            ctx.llmClient.complete(request).flatMap {
              case LlmResponse.Text(content, _, _) =>
                emit(AgentEvent.Finished(content))
                  .as(Right(state.appendMessage(Message.Assistant(content)).toMessageList))

              case LlmResponse.ToolUse(call, _, _) =>
                ctx.handleToolCalls(NonEmptyList.one(call), state, emit)
                  .map(updated => Left(updated.incrementTurn))

              case LlmResponse.ToolUseMany(calls, _, _) =>
                ctx.handleToolCalls(calls, state, emit)
                  .map(updated => Left(updated.incrementTurn))
            }
        }

  def apply[F[_]: Concurrent](ctx: LoopContext[F], hook: TurnHook[F]): AgentLoop[F] =
    new AgentLoop[F]:
      def run(messages: List[Message], emit: AgentEvent => F[Unit]): F[List[Message]] =
        Concurrent[F].tailRecM(LoopState.fromMessages(messages)) { state =>
          if state.turn >= ctx.config.maxTurns then
            emit(AgentEvent.Finished(s"Max turns (${ctx.config.maxTurns}) exceeded"))
              .as(Right(state.toMessageList))
          else
            hook.beforeTurn(state.toView, emit).flatMap { hookedMsgs =>
              val hookedState = LoopState(Chain.fromSeq(hookedMsgs), state.turn)
              val request = LlmRequest(hookedMsgs, ctx.toolSchemas, ctx.config)
              ctx.llmClient.complete(request).flatMap {
                case LlmResponse.Text(content, _, _) =>
                  emit(AgentEvent.Finished(content))
                    .as(Right(hookedState.appendMessage(Message.Assistant(content)).toMessageList))

                case LlmResponse.ToolUse(call, _, _) =>
                  ctx.handleToolCalls(NonEmptyList.one(call), hookedState, emit)
                    .flatMap(updated => hook.afterTurn(updated.toView, emit))
                    .map(afterMsgs => Left(LoopState(Chain.fromSeq(afterMsgs), state.turn + 1)))

                case LlmResponse.ToolUseMany(calls, _, _) =>
                  ctx.handleToolCalls(calls, hookedState, emit)
                    .flatMap(updated => hook.afterTurn(updated.toView, emit))
                    .map(afterMsgs => Left(LoopState(Chain.fromSeq(afterMsgs), state.turn + 1)))
              }
            }
        }

  private[agent] def toolResultToJson(result: ToolResult): io.circe.Json =
    result.structuredContent.getOrElse(
      result.asText.fold(io.circe.Json.Null)(io.circe.Json.fromString)
    )
