package mcp4s.agent

import cats.Semigroup
import cats.effect.{Concurrent, Ref}
import cats.syntax.all.*

/** Middleware for wrapping entire agent loop execution.
  *
  * Mirrors the `server/Middleware[F]` pattern for cross-cutting concerns
  * like logging, error handling, and metrics around loop runs.
  *
  * @tparam F the effect type
  */
trait LoopMiddleware[F[_]]:

  /** Wrap a loop execution.
    *
    * @param messages the input message history
    * @param emit     callback to emit agent events
    * @param next     the wrapped loop execution (by-name for lazy evaluation)
    * @return the final message history
    */
  def apply(messages: List[Message], emit: AgentEvent => F[Unit])(next: => F[List[Message]]): F[List[Message]]

object LoopMiddleware:

  /** Create a LoopMiddleware from a function. */
  def apply[F[_]](
      f: (List[Message], AgentEvent => F[Unit], => F[List[Message]]) => F[List[Message]]
  ): LoopMiddleware[F] =
    new LoopMiddleware[F]:
      def apply(messages: List[Message], emit: AgentEvent => F[Unit])(next: => F[List[Message]]): F[List[Message]] =
        f(messages, emit, next)

  /** Identity middleware — passes through to the wrapped loop unchanged. */
  def identity[F[_]]: LoopMiddleware[F] =
    new LoopMiddleware[F]:
      def apply(messages: List[Message], emit: AgentEvent => F[Unit])(next: => F[List[Message]]): F[List[Message]] =
        next

  /** Combine two middlewares: outer wraps inner. */
  def combine[F[_]](outer: LoopMiddleware[F], inner: LoopMiddleware[F]): LoopMiddleware[F] =
    new LoopMiddleware[F]:
      def apply(messages: List[Message], emit: AgentEvent => F[Unit])(next: => F[List[Message]]): F[List[Message]] =
        outer(messages, emit)(inner(messages, emit)(next))

  /** Logging middleware — logs loop start and end via the provided log function. */
  def logging[F[_]: Concurrent](log: String => F[Unit]): LoopMiddleware[F] =
    new LoopMiddleware[F]:
      def apply(messages: List[Message], emit: AgentEvent => F[Unit])(next: => F[List[Message]]): F[List[Message]] =
        log(s"[Agent] Loop starting with ${messages.size} messages") *>
          next.flatTap(result => log(s"[Agent] Loop finished with ${result.size} messages"))

  /** Error-catching middleware — converts exceptions to a `Finished` event. */
  def catchErrors[F[_]: Concurrent](log: String => F[Unit]): LoopMiddleware[F] =
    new LoopMiddleware[F]:
      def apply(messages: List[Message], emit: AgentEvent => F[Unit])(next: => F[List[Message]]): F[List[Message]] =
        next.handleErrorWith { e =>
          log(s"[Agent] Loop failed: ${e.getMessage}") *>
            emit(AgentEvent.Finished(s"Error: ${e.getMessage}")).as(messages)
        }

  /** Observable middleware — updates a Ref with TurnView snapshots before and after loop execution. */
  def observable[F[_]: Concurrent](ref: Ref[F, Option[TurnView]]): LoopMiddleware[F] =
    new LoopMiddleware[F]:
      def apply(messages: List[Message], emit: AgentEvent => F[Unit])(next: => F[List[Message]]): F[List[Message]] =
        ref.set(Some(TurnView(messages, 0))) *>
          next.flatTap(result => ref.set(Some(TurnView(result, -1))))

  /** Semigroup instance — combines middlewares via nesting (left is outermost). */
  given [F[_]]: Semigroup[LoopMiddleware[F]] with
    def combine(x: LoopMiddleware[F], y: LoopMiddleware[F]): LoopMiddleware[F] =
      LoopMiddleware.combine(x, y)
