package mcp4s.agent

import cats.effect.{Concurrent, Ref}
import fs2.Stream

/** Context passed to agent server-side tool handlers, giving them access to the agent's internals.
  *
  * Server-side tools registered via `withAgentTools` receive an `AgentContext` so they can
  * interact with the agent's LLM, configuration, and conversation state.
  */
trait AgentContext[F[_]]:
  /** The agent's LLM client. */
  def llmClient: LlmClient[F]

  /** The agent's LLM configuration. */
  def config: LlmConfig

  /** Shared conversation messages — read and append. */
  def messages: Ref[F, List[Message]]

  /** Run the agent loop with a prompt, returning events. */
  def run(prompt: String): Stream[F, AgentEvent]

object AgentContext:
  private[agent] def apply[F[_]: Concurrent](
      agent: Agent[F],
      llm: LlmClient[F],
      cfg: LlmConfig,
      msgs: Ref[F, List[Message]]
  ): AgentContext[F] =
    new AgentContext[F]:
      def llmClient: LlmClient[F] = llm
      def config: LlmConfig = cfg
      def messages: Ref[F, List[Message]] = msgs
      def run(prompt: String): Stream[F, AgentEvent] = agent.run(prompt)
