package mcp4s.agent

import cats.Semigroup
import cats.effect.Concurrent
import cats.syntax.all.*

/** Composable turn-level behavior injection for agent loops.
  *
  * Hooks fire before and after each turn in the LLM–tool interaction cycle.
  * Multiple hooks compose via the `Semigroup` instance, which chains
  * `beforeTurn` and `afterTurn` left-to-right.
  *
  * @tparam F the effect type
  */
trait TurnHook[F[_]]:
  /** Called before each LLM call. May modify the message history.
    *
    * @param view read-only snapshot of current loop state
    * @param emit callback to emit agent events
    * @return possibly modified message history
    */
  def beforeTurn(view: TurnView, emit: AgentEvent => F[Unit]): F[List[Message]]

  /** Called after each tool execution round. May modify the message history.
    *
    * @param view read-only snapshot of current loop state (includes tool results)
    * @param emit callback to emit agent events
    * @return possibly modified message history
    */
  def afterTurn(view: TurnView, emit: AgentEvent => F[Unit]): F[List[Message]]

object TurnHook:

  /** No-op hook that passes messages through unchanged. */
  def identity[F[_]: Concurrent]: TurnHook[F] =
    new TurnHook[F]:
      def beforeTurn(view: TurnView, emit: AgentEvent => F[Unit]): F[List[Message]] =
        Concurrent[F].pure(view.messages)
      def afterTurn(view: TurnView, emit: AgentEvent => F[Unit]): F[List[Message]] =
        Concurrent[F].pure(view.messages)

  /** Create a hook that only fires before each turn. */
  def before[F[_]: Concurrent](
      f: (TurnView, AgentEvent => F[Unit]) => F[List[Message]]
  ): TurnHook[F] =
    new TurnHook[F]:
      def beforeTurn(view: TurnView, emit: AgentEvent => F[Unit]): F[List[Message]] =
        f(view, emit)
      def afterTurn(view: TurnView, emit: AgentEvent => F[Unit]): F[List[Message]] =
        Concurrent[F].pure(view.messages)

  /** Create a hook that only fires after each turn. */
  def after[F[_]: Concurrent](
      f: (TurnView, AgentEvent => F[Unit]) => F[List[Message]]
  ): TurnHook[F] =
    new TurnHook[F]:
      def beforeTurn(view: TurnView, emit: AgentEvent => F[Unit]): F[List[Message]] =
        Concurrent[F].pure(view.messages)
      def afterTurn(view: TurnView, emit: AgentEvent => F[Unit]): F[List[Message]] =
        f(view, emit)

  /** Chain-of-thought hook: injects a "think then act" step before tool turns.
    *
    * Makes a separate LLM call with no tools to force explicit reasoning,
    * then emits `AgentEvent.Thinking`. The thinking response is appended to
    * history as `Message.Assistant`.
    *
    * Fires on turn 0 always. Fires on subsequent turns only when
    * `config.thinkBeforeEveryTurn` is true.
    */
  def thinking[F[_]: Concurrent](ctx: LoopContext[F], config: ChainOfThoughtConfig): TurnHook[F] =
    before[F] { (view, emit) =>
      val messages = view.messages
      if config.thinkBeforeEveryTurn || view.turn == 0 then
        val thinkingRequest = LlmRequest(
          messages :+ Message.User(config.thinkingPrompt),
          tools = Nil,
          config = ctx.config
        )
        ctx.llmClient.complete(thinkingRequest).flatMap {
          case LlmResponse.Text(content, _, _) =>
            emit(AgentEvent.Thinking(content))
              .as(messages :+ Message.Assistant(content))
          case _ =>
            // Unexpected tool use in thinking (no tools were provided); skip
            Concurrent[F].pure(messages)
        }
      else Concurrent[F].pure(messages)
    }

  /** Reflection hook: periodically pauses for LLM self-reflection.
    *
    * After every `config.reflectEveryNTurns` tool turns, makes a separate LLM
    * call with no tools and emits `AgentEvent.Reflection`. The reflection
    * response is appended to history as `Message.Assistant`.
    */
  def reflection[F[_]: Concurrent](ctx: LoopContext[F], config: ReflectionConfig): TurnHook[F] =
    after[F] { (view, emit) =>
      val messages = view.messages
      if (view.turn + 1) % config.reflectEveryNTurns == 0 then
        val reflectionRequest = LlmRequest(
          messages :+ Message.User(config.reflectionPrompt),
          tools = Nil,
          config = ctx.config
        )
        ctx.llmClient.complete(reflectionRequest).flatMap {
          case LlmResponse.Text(content, _, _) =>
            emit(AgentEvent.Reflection(content))
              .as(messages :+ Message.Assistant(content))
          case _ =>
            // Unexpected tool use in reflection (no tools were provided); skip
            Concurrent[F].pure(messages)
        }
      else Concurrent[F].pure(messages)
    }

  /** Context management hook: compresses messages when token count exceeds budget.
    *
    * Fires before each turn. If the current token count is within budget, passes
    * messages through unchanged. Otherwise applies the compression policy and
    * emits `AgentEvent.ContextCompressed`.
    */
  def contextManaged[F[_]: Concurrent](
      estimator: TokenEstimator[F],
      policy: ContextPolicy[F],
      budget: TokenBudget
  ): TurnHook[F] =
    before[F] { (view, emit) =>
      estimator.estimateAll(view.messages).flatMap { currentTokens =>
        if currentTokens > budget.available then
          policy.compact(view.messages, budget, estimator).flatMap { compacted =>
            estimator.estimateAll(compacted).flatMap { afterTokens =>
              emit(AgentEvent.ContextCompressed(currentTokens, afterTokens, view.messages.size, compacted.size))
                .as(compacted)
            }
          }
        else Concurrent[F].pure(view.messages)
      }
    }

  /** Semigroup instance — chains both `beforeTurn` and `afterTurn` left-to-right. */
  given [F[_]: Concurrent]: Semigroup[TurnHook[F]] with
    def combine(x: TurnHook[F], y: TurnHook[F]): TurnHook[F] =
      new TurnHook[F]:
        def beforeTurn(view: TurnView, emit: AgentEvent => F[Unit]): F[List[Message]] =
          x.beforeTurn(view, emit).flatMap(msgs => y.beforeTurn(TurnView(msgs, view.turn), emit))
        def afterTurn(view: TurnView, emit: AgentEvent => F[Unit]): F[List[Message]] =
          x.afterTurn(view, emit).flatMap(msgs => y.afterTurn(TurnView(msgs, view.turn), emit))
