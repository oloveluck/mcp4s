package mcp4s.agent

import cats.effect.Concurrent

/** Configuration for chain-of-thought agent loops.
  *
  * @param thinkingPrompt        the prompt injected to trigger LLM reasoning before tool use
  * @param thinkBeforeEveryTurn  if true, think before every tool turn; if false, only before the first
  */
final case class ChainOfThoughtConfig(
    thinkingPrompt: String =
      "Think step by step about what to do next. What information do you need? Which tool should you use and why? Be concise.",
    thinkBeforeEveryTurn: Boolean = true
)

/** Agent loop that injects a "think then act" step before tool turns.
  *
  * Delegates to `ToolLoop` with a `TurnHook.thinking` hook.
  */
object ChainOfThoughtLoop:

  def apply[F[_]: Concurrent](ctx: LoopContext[F], config: ChainOfThoughtConfig): AgentLoop[F] =
    ToolLoop(ctx, TurnHook.thinking(ctx, config))
