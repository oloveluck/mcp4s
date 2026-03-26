package mcp4s.agent

import cats.Applicative
import cats.syntax.all.*

/** Algebra for estimating the token cost of messages.
  *
  * Effectful to support real tokenizer integrations (tiktoken JNI, HTTP APIs, `Ref`-cached).
  */
trait TokenEstimator[F[_]]:
  def estimate(message: Message): F[Tokens]

  def estimateAll(messages: List[Message])(using Applicative[F]): F[Tokens] =
    messages.traverse(estimate).map(_.foldLeft(Tokens.zero)(_ + _))

object TokenEstimator:

  /** Create a `TokenEstimator` from a function. */
  def apply[F[_]](f: Message => F[Tokens]): TokenEstimator[F] =
    new TokenEstimator[F]:
      def estimate(message: Message): F[Tokens] = f(message)

  /** Character-based heuristic estimator (~4 chars per token). */
  def charBased[F[_]: Applicative]: TokenEstimator[F] =
    apply { msg =>
      val chars = msg match
        case Message.User(content)    => content.length.toLong
        case Message.Assistant(content) => content.length.toLong
        case Message.ToolUse(calls) =>
          calls.toList.foldLeft(0L) { (acc, call) =>
            acc + call.name.length.toLong + call.arguments.noSpaces.length.toLong
          }
        case Message.ToolResult(_, name, content) =>
          name.length.toLong + content.noSpaces.length.toLong
      Applicative[F].pure(Tokens(chars / 4))
    }
