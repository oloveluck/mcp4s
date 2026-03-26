package mcp4s.agent

import cats.Semigroup
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import mcp4s.client.McpConnection

/** Core abstraction for agent loop strategies.
  *
  * Implementations drive the LLM–tool interaction cycle. The `emit` callback
  * decouples event delivery from queue mechanics — `Agent` wraps it with
  * `queue.offer(Some(_))`.
  *
  * @tparam F the effect type
  */
trait AgentLoop[F[_]]:
  /** Run the loop with the given message history, emitting events via `emit`.
    *
    * @return the final message history (including any appended messages)
    */
  def run(messages: List[Message], emit: AgentEvent => F[Unit]): F[List[Message]]

/** Dependencies shared by loop implementations. */
final case class LoopContext[F[_]](
    llmClient: LlmClient[F],
    connection: McpConnection[F],
    toolSchemas: List[ToolSchema],
    config: LlmConfig
):
  /** Execute tool calls, emit events, and return the updated loop state. */
  private[agent] def handleToolCalls(
      calls: NonEmptyList[ToolCall],
      state: LoopState,
      emit: AgentEvent => F[Unit]
  )(using F: Concurrent[F]): F[LoopState] =
    for
      _ <- calls.traverse_(call => emit(AgentEvent.ToolCalled(call)))
      results <- calls.toList.traverse { call =>
        connection.callTool[io.circe.Json](call.name, call.arguments).map(result => (call, result))
      }
      resultMessages = results.map { case (call, result) =>
        val json = ToolLoop.toolResultToJson(result)
        (Message.ToolResult(call.id, call.name, json), AgentEvent.ToolResultReceived(call.id, call.name, json))
      }
      _ <- resultMessages.traverse_ { case (_, event) => emit(event) }
    yield state
      .appendMessage(Message.ToolUse(calls))
      .appendMessages(resultMessages.map(_._1))

object AgentLoop:
  /** Create an AgentLoop from a function. */
  def apply[F[_]](f: (List[Message], AgentEvent => F[Unit]) => F[List[Message]]): AgentLoop[F] =
    new AgentLoop[F]:
      def run(messages: List[Message], emit: AgentEvent => F[Unit]): F[List[Message]] =
        f(messages, emit)

  /** Chain two loops sequentially: the second receives the first's output messages. */
  def sequence[F[_]: Concurrent](first: AgentLoop[F], second: AgentLoop[F]): AgentLoop[F] =
    new AgentLoop[F]:
      def run(messages: List[Message], emit: AgentEvent => F[Unit]): F[List[Message]] =
        first.run(messages, emit).flatMap(second.run(_, emit))

  /** Semigroup instance — chains loops sequentially via `|+|`. */
  given [F[_]: Concurrent]: Semigroup[AgentLoop[F]] with
    def combine(x: AgentLoop[F], y: AgentLoop[F]): AgentLoop[F] =
      sequence(x, y)

extension [F[_]: Concurrent](loop: AgentLoop[F])

  /** Wrap this loop with a `LoopMiddleware`. */
  def withMiddleware(mw: LoopMiddleware[F]): AgentLoop[F] =
    new AgentLoop[F]:
      def run(messages: List[Message], emit: AgentEvent => F[Unit]): F[List[Message]] =
        mw(messages, emit)(loop.run(messages, emit))

  /** Post-process the result messages. */
  def mapMessages(f: List[Message] => F[List[Message]]): AgentLoop[F] =
    new AgentLoop[F]:
      def run(messages: List[Message], emit: AgentEvent => F[Unit]): F[List[Message]] =
        loop.run(messages, emit).flatMap(f)

  /** Transform emitted events before delivery. */
  def mapEvents(f: AgentEvent => AgentEvent): AgentLoop[F] =
    new AgentLoop[F]:
      def run(messages: List[Message], emit: AgentEvent => F[Unit]): F[List[Message]] =
        loop.run(messages, e => emit(f(e)))
