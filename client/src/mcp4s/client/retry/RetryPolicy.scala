package mcp4s.client.retry

import cats.MonadError
import cats.effect.Temporal
import cats.syntax.all.*

import scala.concurrent.duration.*
import scala.util.Random

/** Configurable retry policy for MCP operations.
  *
  * A pure (non-effectful) policy that determines whether a failed operation
  * should be retried and how long to wait between retries.
  *
  * Example:
  * {{{
  * val policy = RetryPolicy.exponentialBackoff(
  *   maxRetries = 5,
  *   baseDelay = 100.millis,
  *   maxDelay = 30.seconds
  * )
  *
  * connection.withRetry(policy).callTool("myTool", args)
  * }}}
  */
final case class RetryPolicy(
    /** Maximum number of retry attempts (not including initial attempt) */
    maxRetries: Int,
    /** Calculate delay for a given attempt number (1-indexed) */
    delayFn: Int => FiniteDuration,
    /** Predicate to determine if an error should be retried */
    shouldRetryFn: Throwable => Boolean
):
  /** Determine if an error should trigger a retry */
  def shouldRetry(error: Throwable, attempt: Int): Boolean =
    attempt <= maxRetries && shouldRetryFn(error)

  /** Calculate the delay before the next retry */
  def delay(attempt: Int): FiniteDuration = delayFn(attempt)

object RetryPolicy:

  /** Create a retry policy with exponential backoff.
    *
    * The delay doubles with each attempt up to maxDelay, with optional jitter.
    *
    * @param maxRetries Maximum number of retry attempts (not including initial attempt)
    * @param baseDelay Initial delay between retries
    * @param maxDelay Maximum delay (cap on exponential growth)
    * @param jitterFactor Random jitter factor (0.0 to 1.0) to prevent thundering herd
    * @param retryOn Predicate to determine if a specific error should be retried
    */
  def exponentialBackoff(
      maxRetries: Int = 3,
      baseDelay: FiniteDuration = 100.millis,
      maxDelay: FiniteDuration = 10.seconds,
      jitterFactor: Double = 0.1,
      retryOn: Throwable => Boolean = defaultRetryPredicate
  ): RetryPolicy =
    RetryPolicy(
      maxRetries = maxRetries,
      delayFn = { attempt =>
        val exponentialDelay = baseDelay * math.pow(2, attempt - 1).toLong
        val cappedDelay = exponentialDelay.min(maxDelay)
        if jitterFactor > 0 then
          val jitter = (cappedDelay.toMillis * jitterFactor * Random.nextDouble()).toLong
          (cappedDelay.toMillis + jitter - (cappedDelay.toMillis * jitterFactor / 2).toLong).millis
        else
          cappedDelay
      },
      shouldRetryFn = retryOn
    )

  /** A policy that never retries. */
  val noRetry: RetryPolicy = RetryPolicy(
    maxRetries = 0,
    delayFn = _ => Duration.Zero,
    shouldRetryFn = _ => false
  )

  /** Create a policy with fixed delay between retries. */
  def fixedDelay(
      maxRetries: Int = 3,
      delay: FiniteDuration = 1.second,
      retryOn: Throwable => Boolean = defaultRetryPredicate
  ): RetryPolicy =
    RetryPolicy(
      maxRetries = maxRetries,
      delayFn = _ => delay,
      shouldRetryFn = retryOn
    )

  /** Create a policy with linear backoff (delay increases linearly). */
  def linearBackoff(
      maxRetries: Int = 3,
      initialDelay: FiniteDuration = 100.millis,
      increment: FiniteDuration = 100.millis,
      maxDelay: FiniteDuration = 10.seconds,
      retryOn: Throwable => Boolean = defaultRetryPredicate
  ): RetryPolicy =
    RetryPolicy(
      maxRetries = maxRetries,
      delayFn = attempt => (initialDelay + (increment * (attempt - 1))).min(maxDelay),
      shouldRetryFn = retryOn
    )

  /** Default predicate for retryable errors.
    *
    * Retries on:
    * - Connection errors (java.net.ConnectException, etc.)
    * - Timeout errors
    * - Server errors (5xx-like)
    *
    * Does not retry on:
    * - Client errors (4xx-like, invalid arguments, etc.)
    * - Fatal errors
    */
  val defaultRetryPredicate: Throwable => Boolean = {
    case _: java.net.ConnectException             => true
    case _: java.net.SocketTimeoutException       => true
    case _: java.net.UnknownHostException         => true
    case _: java.io.IOException                   => true
    case _: java.util.concurrent.TimeoutException => true
    case e if e.getMessage != null && e.getMessage.contains("Connection refused") => true
    case e if e.getMessage != null && e.getMessage.contains("Connection reset")   => true
    case _                                        => false
  }

/** Helper to apply retry logic to an effect.
  *
  * This is an internal implementation detail. Users should use the
  * resilience extension methods on McpConnection instead:
  * {{{
  * connection.withRetry(RetryPolicy.exponentialBackoff())
  * }}}
  */
private[client] object Retry:

  /** Execute an effect with retry logic.
    *
    * @param policy The retry policy to use
    * @param fa The effect to execute
    * @return The result of the effect, after retries if needed
    */
  def apply[F[_]: Temporal, A](policy: RetryPolicy)(fa: F[A]): F[A] =
    def loop(attempt: Int): F[A] =
      fa.handleErrorWith { error =>
        if policy.shouldRetry(error, attempt) then
          val delayDuration = policy.delay(attempt)
          Temporal[F].sleep(delayDuration) *> loop(attempt + 1)
        else
          MonadError[F, Throwable].raiseError(error)
      }
    loop(1)
