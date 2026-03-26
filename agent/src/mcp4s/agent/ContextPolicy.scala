package mcp4s.agent

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Json

/** Token budget configuration for context window management.
  *
  * @param maxTokens the provider's context window size
  * @param reservedForResponse tokens reserved for the LLM response
  */
final case class TokenBudget(maxTokens: Int, reservedForResponse: Int = 4096):
  def available: Tokens = Tokens(maxTokens.toLong) - Tokens(reservedForResponse.toLong)

/** Algebra for context window compression strategies.
  *
  * Budget and estimator are parameters (not baked in at construction) so a single
  * policy instance is reusable across different estimators/budgets.
  */
trait ContextPolicy[F[_]]:
  def compact(messages: List[Message], budget: TokenBudget, estimator: TokenEstimator[F]): F[List[Message]]

object ContextPolicy:

  /** Keep the first message (system prompt) and as many recent messages as fit in the budget. */
  def keepSystemAndRecent[F[_]: Concurrent]: ContextPolicy[F] =
    new ContextPolicy[F]:
      def compact(messages: List[Message], budget: TokenBudget, estimator: TokenEstimator[F]): F[List[Message]] =
        messages match
          case Nil => Concurrent[F].pure(Nil)
          case first :: rest =>
            estimator.estimate(first).flatMap { firstCost =>
              val remaining = budget.available - firstCost
              fitFromEnd(rest, remaining, estimator).map(first :: _)
            }

  /** Keep the last N tokens worth of messages from the end. Does NOT preserve the first message. */
  def slidingWindow[F[_]: Concurrent]: ContextPolicy[F] =
    new ContextPolicy[F]:
      def compact(messages: List[Message], budget: TokenBudget, estimator: TokenEstimator[F]): F[List[Message]] =
        fitFromEnd(messages, budget.available, estimator)

  /** Replace older `ToolResult` content with `[truncated]`, preserving tool call/result structure.
    *
    * @param keepRecent number of most recent messages to keep intact
    */
  def dropToolResults[F[_]: Concurrent](keepRecent: Int): ContextPolicy[F] =
    new ContextPolicy[F]:
      def compact(messages: List[Message], budget: TokenBudget, estimator: TokenEstimator[F]): F[List[Message]] =
        val (older, recent) = messages.splitAt(messages.size - keepRecent)
        val truncated = older.map {
          case Message.ToolResult(id, name, _) =>
            Message.ToolResult(id, name, Json.fromString("[truncated]"))
          case other => other
        }
        Concurrent[F].pure(truncated ++ recent)

  /** LLM-based summarization of older messages.
    *
    * @param llmClient the LLM client used for summarization
    * @param config LLM configuration for the summarization call
    * @param keepRecent number of most recent messages to keep intact
    * @param summaryPrompt prompt template for the summarization request
    */
  def summarize[F[_]: Concurrent](
      llmClient: LlmClient[F],
      config: LlmConfig,
      keepRecent: Int = 6,
      summaryPrompt: String = "Summarize the following conversation concisely, preserving key facts and decisions:"
  ): ContextPolicy[F] =
    new ContextPolicy[F]:
      def compact(messages: List[Message], budget: TokenBudget, estimator: TokenEstimator[F]): F[List[Message]] =
        val (older, recent) = messages.splitAt(messages.size - keepRecent)
        if older.isEmpty then Concurrent[F].pure(messages)
        else
          val rendered = older.map(renderMessage).mkString("\n")
          val request = LlmRequest(
            List(Message.User(s"$summaryPrompt\n\n$rendered")),
            tools = Nil,
            config = config
          )
          llmClient.complete(request).map {
            case LlmResponse.Text(content, _, _) =>
              Message.Assistant(s"[Summary of earlier conversation]\n$content") :: recent
            case _ =>
              // Unexpected tool use (no tools were provided); keep original
              messages
          }

  /** Apply `first`, then `second` only if still over budget. Sequential fallback chain. */
  def pipeline[F[_]: Concurrent](first: ContextPolicy[F], second: ContextPolicy[F]): ContextPolicy[F] =
    new ContextPolicy[F]:
      def compact(messages: List[Message], budget: TokenBudget, estimator: TokenEstimator[F]): F[List[Message]] =
        first.compact(messages, budget, estimator).flatMap { afterFirst =>
          estimator.estimateAll(afterFirst).flatMap { cost =>
            if cost > budget.available then
              second.compact(afterFirst, budget, estimator)
            else
              Concurrent[F].pure(afterFirst)
          }
        }

  /** Scan messages from the end, keeping as many as fit within the available token budget. */
  private def fitFromEnd[F[_]: Concurrent](
      messages: List[Message],
      available: Tokens,
      estimator: TokenEstimator[F]
  ): F[List[Message]] =
    val reversed = messages.reverse
    reversed.foldLeftM((List.empty[Message], Tokens.zero)) { case ((kept, used), msg) =>
      estimator.estimate(msg).map { cost =>
        val total = used + cost
        if total <= available then (msg :: kept, total)
        else (kept, used)
      }
    }.map(_._1)

  /** Convert a Message to human-readable text for summarization prompts. */
  private def renderMessage(msg: Message): String = msg match
    case Message.User(content)      => s"User: $content"
    case Message.Assistant(content) => s"Assistant: $content"
    case Message.ToolUse(calls)     => s"Tool calls: ${calls.toList.map(c => s"${c.name}(${c.arguments.noSpaces})").mkString(", ")}"
    case Message.ToolResult(_, name, content) => s"Tool result ($name): ${content.noSpaces}"
