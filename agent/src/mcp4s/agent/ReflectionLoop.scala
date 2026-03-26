package mcp4s.agent

import cats.effect.Concurrent

/** Configuration for reflection-based agent loops.
  *
  * @param reflectEveryNTurns how many tool-calling turns between reflection pauses
  * @param reflectionPrompt   the prompt injected to trigger LLM self-review
  */
final case class ReflectionConfig(
    reflectEveryNTurns: Int = 3,
    reflectionPrompt: String =
      "Review your progress so far. Are you on the right track? What should you do next? Be concise."
)

/** Agent loop that periodically pauses for LLM self-reflection.
  *
  * Delegates to `ToolLoop` with a `TurnHook.reflection` hook.
  */
object ReflectionLoop:

  def apply[F[_]: Concurrent](ctx: LoopContext[F], config: ReflectionConfig): AgentLoop[F] =
    ToolLoop(ctx, TurnHook.reflection(ctx, config))
