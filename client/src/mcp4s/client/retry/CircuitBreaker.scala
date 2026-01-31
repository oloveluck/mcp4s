package mcp4s.client.retry

import cats.effect.{Concurrent, Ref, Temporal}
import cats.syntax.all.*

import scala.concurrent.duration.*
import java.time.Instant

/** Circuit breaker states */
enum CircuitState:
  /** Circuit is closed, requests flow through normally */
  case Closed
  /** Circuit is open, requests fail fast */
  case Open
  /** Circuit is half-open, allowing a single test request */
  case HalfOpen

/** Error thrown when the circuit breaker is open */
case class CircuitBreakerOpenException(message: String = "Circuit breaker is open")
    extends RuntimeException(message)

/** Circuit breaker configuration */
final case class CircuitBreakerConfig(
    /** Number of failures before opening the circuit */
    failureThreshold: Int = 5,
    /** Duration to keep the circuit open before allowing a test request */
    resetTimeout: FiniteDuration = 30.seconds,
    /** Number of consecutive successes needed to close a half-open circuit */
    successThreshold: Int = 2,
    /** Predicate to determine if an error should count as a failure */
    isFailure: Throwable => Boolean = _ => true
)

object CircuitBreakerConfig:
  val default: CircuitBreakerConfig = CircuitBreakerConfig()

/** Statistics for circuit breaker observability */
final case class CircuitBreakerStats(
    /** Current circuit state */
    state: CircuitState,
    /** Current consecutive failure count */
    failures: Int,
    /** Current consecutive success count (relevant in HalfOpen state) */
    successes: Int,
    /** Total number of requests processed */
    totalRequests: Long,
    /** Number of requests rejected due to open circuit */
    rejectedRequests: Long
)

/** Circuit breaker for fail-fast behavior when a service is down.
  *
  * The circuit breaker has three states:
  * - Closed: Normal operation, requests flow through
  * - Open: Requests fail fast without being executed
  * - HalfOpen: A single test request is allowed to determine if the service is back
  *
  * Example:
  * {{{
  * for
  *   cb <- CircuitBreaker[IO](config)
  *   result <- cb.protect(riskyOperation)
  * yield result
  * }}}
  */
trait CircuitBreaker[F[_]]:

  /** Execute an effect with circuit breaker protection.
    *
    * - If closed: Execute the effect normally
    * - If open: Fail fast with CircuitBreakerOpenException
    * - If half-open: Allow a test request
    *
    * @param fa The effect to protect
    * @return The result of the effect
    */
  def protect[A](fa: F[A]): F[A]

  /** Get the current circuit state */
  def state: F[CircuitState]

  /** Force the circuit to close (for testing/admin) */
  def forceClose: F[Unit]

  /** Force the circuit to open (for testing/admin) */
  def forceOpen: F[Unit]

  /** Get current failure count */
  def failureCount: F[Int]

  /** Get comprehensive circuit breaker statistics */
  def stats: F[CircuitBreakerStats]

object CircuitBreaker:

  private case class InternalState(
      state: CircuitState,
      failures: Int,
      successes: Int,
      lastFailure: Option[Instant],
      totalRequests: Long,
      rejectedRequests: Long
  )

  /** Create a new circuit breaker with the given configuration. */
  def apply[F[_]: Temporal](
      config: CircuitBreakerConfig = CircuitBreakerConfig.default
  ): F[CircuitBreaker[F]] =
    Ref.of[F, InternalState](
      InternalState(CircuitState.Closed, 0, 0, None, 0L, 0L)
    ).map { stateRef =>
      new CircuitBreakerImpl(config, stateRef)
    }

  private class CircuitBreakerImpl[F[_]: Temporal](
      config: CircuitBreakerConfig,
      stateRef: Ref[F, InternalState]
  ) extends CircuitBreaker[F]:

    def protect[A](fa: F[A]): F[A] =
      for
        currentState <- checkAndTransition
        result <- currentState match
          case CircuitState.Open =>
            recordRejected *> Concurrent[F].raiseError[A](CircuitBreakerOpenException())
          case CircuitState.Closed | CircuitState.HalfOpen =>
            recordRequest *> fa.handleErrorWith { error =>
              if config.isFailure(error) then
                recordFailure *> Concurrent[F].raiseError(error)
              else
                Concurrent[F].raiseError(error)
            }.flatTap(_ => recordSuccess)
      yield result

    def state: F[CircuitState] = stateRef.get.map(_.state)

    def failureCount: F[Int] = stateRef.get.map(_.failures)

    def stats: F[CircuitBreakerStats] =
      stateRef.get.map { internal =>
        CircuitBreakerStats(
          state = internal.state,
          failures = internal.failures,
          successes = internal.successes,
          totalRequests = internal.totalRequests,
          rejectedRequests = internal.rejectedRequests
        )
      }

    def forceClose: F[Unit] =
      stateRef.update(_.copy(state = CircuitState.Closed, failures = 0, successes = 0, lastFailure = None))

    def forceOpen: F[Unit] =
      Temporal[F].realTimeInstant.flatMap { now =>
        stateRef.update(_.copy(state = CircuitState.Open, failures = config.failureThreshold, successes = 0, lastFailure = Some(now)))
      }

    /** Check if we should transition from Open to HalfOpen based on reset timeout */
    private def checkAndTransition: F[CircuitState] =
      Temporal[F].realTimeInstant.flatMap { now =>
        stateRef.modify { internal =>
          internal.state match
            case CircuitState.Open =>
              internal.lastFailure match
                case Some(lastFail) if elapsed(lastFail, now) >= config.resetTimeout =>
                  // Transition to half-open
                  val newState = internal.copy(state = CircuitState.HalfOpen, successes = 0)
                  (newState, CircuitState.HalfOpen)
                case _ =>
                  // Stay open
                  (internal, CircuitState.Open)
            case other =>
              (internal, other)
        }
      }

    private def recordRequest: F[Unit] =
      stateRef.update(s => s.copy(totalRequests = s.totalRequests + 1))

    private def recordRejected: F[Unit] =
      stateRef.update(s => s.copy(rejectedRequests = s.rejectedRequests + 1))

    private def recordFailure: F[Unit] =
      Temporal[F].realTimeInstant.flatMap { now =>
        stateRef.update { internal =>
          val newFailures = internal.failures + 1
          if newFailures >= config.failureThreshold then
            internal.copy(state = CircuitState.Open, failures = newFailures, successes = 0, lastFailure = Some(now))
          else if internal.state == CircuitState.HalfOpen then
            // Failed test request, go back to open
            internal.copy(state = CircuitState.Open, failures = newFailures, successes = 0, lastFailure = Some(now))
          else
            internal.copy(failures = newFailures, successes = 0)
        }
      }

    private def recordSuccess: F[Unit] =
      stateRef.update { internal =>
        internal.state match
          case CircuitState.HalfOpen =>
            val newSuccesses = internal.successes + 1
            if newSuccesses >= config.successThreshold then
              // Enough successes, close the circuit
              internal.copy(state = CircuitState.Closed, failures = 0, successes = 0, lastFailure = None)
            else
              internal.copy(successes = newSuccesses)
          case CircuitState.Closed =>
            // Reset failures on success
            internal.copy(failures = 0, successes = 0)
          case CircuitState.Open =>
            // Shouldn't happen, but stay open
            internal
      }

    private def elapsed(from: Instant, to: Instant): FiniteDuration =
      java.time.Duration.between(from, to).toMillis.millis
