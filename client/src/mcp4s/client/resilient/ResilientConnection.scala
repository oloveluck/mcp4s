package mcp4s.client.resilient

import cats.effect.{Concurrent, Temporal}
import cats.syntax.all.*
import io.circe.Encoder
import mcp4s.client.McpConnection
import mcp4s.client.retry.{CircuitBreaker, CircuitBreakerConfig, CircuitBreakerStats, Retry, RetryPolicy}
import mcp4s.protocol.*

import scala.concurrent.duration.*

/** Configuration for resilient connection behavior.
  *
  * The resilience layers are applied in this order (per-attempt):
  * 1. Timeout - applied to each individual attempt
  * 2. Circuit Breaker - tracks failures and opens when threshold is reached
  * 3. Retry - retries the entire operation (timeout + circuit breaker) on failure
  *
  * This ordering ensures that:
  * - Timeouts are per-attempt, not overall
  * - Circuit breaker sees timeout failures
  * - Retries apply after both timeout and circuit breaker
  *
  * Use [[ResilienceConfig.builder]] for fluent configuration:
  * {{{
  * val config = ResilienceConfig.builder
  *   .withRetry(RetryPolicy.exponentialBackoff(maxRetries = 5))
  *   .withTimeout(30.seconds)
  *   .withCircuitBreaker(CircuitBreakerConfig(failureThreshold = 3))
  *   .build
  * }}}
  */
final case class ResilienceConfig(
    /** Retry policy for failed operations */
    retryPolicy: RetryPolicy = RetryPolicy.exponentialBackoff(),
    /** Circuit breaker configuration (None to disable) */
    circuitBreaker: Option[CircuitBreakerConfig] = Some(CircuitBreakerConfig.default),
    /** Operation timeout per attempt (None to disable) */
    timeout: Option[FiniteDuration] = Some(30.seconds)
)

object ResilienceConfig:
  /** Default configuration with retry, circuit breaker, and timeout */
  val default: ResilienceConfig = ResilienceConfig()

  /** Configuration with only retry enabled */
  def retryOnly(policy: RetryPolicy): ResilienceConfig =
    ResilienceConfig(retryPolicy = policy, circuitBreaker = None, timeout = None)

  /** Configuration with only timeout enabled */
  def timeoutOnly(duration: FiniteDuration): ResilienceConfig =
    ResilienceConfig(
      retryPolicy = RetryPolicy.noRetry,
      circuitBreaker = None,
      timeout = Some(duration)
    )

  /** Create a new builder for fluent configuration */
  def builder: ResilienceConfigBuilder = ResilienceConfigBuilder()

/** Fluent builder for ResilienceConfig.
  *
  * Allows incremental configuration of resilience features:
  * {{{
  * val config = ResilienceConfig.builder
  *   .withRetry(RetryPolicy.exponentialBackoff(maxRetries = 5))
  *   .withTimeout(30.seconds)
  *   .build
  * }}}
  */
final class ResilienceConfigBuilder private[resilient] (
    private val retryPolicy: RetryPolicy = RetryPolicy.exponentialBackoff(),
    private val circuitBreaker: Option[CircuitBreakerConfig] = Some(CircuitBreakerConfig.default),
    private val timeout: Option[FiniteDuration] = Some(30.seconds)
):

  /** Set the retry policy */
  def withRetry(policy: RetryPolicy): ResilienceConfigBuilder =
    new ResilienceConfigBuilder(policy, circuitBreaker, timeout)

  /** Disable retry */
  def withoutRetry: ResilienceConfigBuilder =
    new ResilienceConfigBuilder(RetryPolicy.noRetry, circuitBreaker, timeout)

  /** Set the circuit breaker configuration */
  def withCircuitBreaker(config: CircuitBreakerConfig): ResilienceConfigBuilder =
    new ResilienceConfigBuilder(retryPolicy, Some(config), timeout)

  /** Disable circuit breaker */
  def withoutCircuitBreaker: ResilienceConfigBuilder =
    new ResilienceConfigBuilder(retryPolicy, None, timeout)

  /** Set the per-attempt timeout */
  def withTimeout(duration: FiniteDuration): ResilienceConfigBuilder =
    new ResilienceConfigBuilder(retryPolicy, circuitBreaker, Some(duration))

  /** Disable timeout */
  def withoutTimeout: ResilienceConfigBuilder =
    new ResilienceConfigBuilder(retryPolicy, circuitBreaker, None)

  /** Build the final configuration */
  def build: ResilienceConfig =
    ResilienceConfig(retryPolicy, circuitBreaker, timeout)

object ResilienceConfigBuilder:
  /** Create a builder starting from default configuration */
  def apply(): ResilienceConfigBuilder = new ResilienceConfigBuilder()

  /** Create a builder starting from an existing configuration */
  def from(config: ResilienceConfig): ResilienceConfigBuilder =
    new ResilienceConfigBuilder(config.retryPolicy, config.circuitBreaker, config.timeout)

/** An MCP connection with resilience features and observability.
  *
  * Extends [[McpConnection]] with access to resilience configuration
  * and circuit breaker statistics for monitoring.
  */
trait ResilientMcpConnection[F[_]] extends McpConnection[F]:

  /** Get the current resilience configuration */
  def resilienceConfig: ResilienceConfig

  /** Get circuit breaker statistics if circuit breaker is enabled.
    *
    * Returns None if circuit breaker is disabled, otherwise returns
    * current statistics including state, failure/success counts, and
    * request metrics.
    */
  def circuitBreakerStats: F[Option[CircuitBreakerStats]]

  /** Get the underlying circuit breaker if enabled.
    *
    * Allows direct access for advanced use cases like manual
    * force-open/force-close operations.
    */
  def circuitBreaker: Option[CircuitBreaker[F]]

/** Extension methods to add resilience to McpConnection */
object ResilientConnection:

  /** Wrap a connection with resilience features.
    *
    * @param connection The underlying connection to wrap
    * @param config Resilience configuration
    * @return A wrapped connection with retry, circuit breaker, and timeout support
    */
  def apply[F[_]: Temporal](
      connection: McpConnection[F],
      config: ResilienceConfig = ResilienceConfig.default
  ): F[ResilientMcpConnection[F]] =
    config.circuitBreaker match
      case Some(cbConfig) =>
        CircuitBreaker[F](cbConfig).map { cb =>
          new ResilientConnectionImpl(connection, config, Some(cb))
        }
      case None =>
        Concurrent[F].pure(new ResilientConnectionImpl(connection, config, None))

  private class ResilientConnectionImpl[F[_]: Temporal](
      underlying: McpConnection[F],
      val resilienceConfig: ResilienceConfig,
      val circuitBreaker: Option[CircuitBreaker[F]]
  ) extends ResilientMcpConnection[F]:

    def circuitBreakerStats: F[Option[CircuitBreakerStats]] =
      circuitBreaker.traverse(_.stats)

    /** Apply resilience layers in order: Retry(CircuitBreaker(Timeout(fa)))
      *
      * This means for each retry attempt:
      * 1. Apply timeout to the operation
      * 2. Track success/failure in circuit breaker
      * 3. If failed, retry according to policy
      */
    private def withResilience[A](fa: F[A]): F[A] =
      // Inner effect: timeout per attempt, then circuit breaker
      val perAttempt: F[A] =
        val withTimeout = resilienceConfig.timeout match
          case Some(duration) => Temporal[F].timeout(fa, duration)
          case None           => fa

        circuitBreaker match
          case Some(cb) => cb.protect(withTimeout)
          case None     => withTimeout

      // Outer: retry the entire per-attempt operation
      Retry(resilienceConfig.retryPolicy)(perAttempt)

    def serverInfo: ServerInfo = underlying.serverInfo
    def serverCapabilities: ServerCapabilities = underlying.serverCapabilities

    def listTools: F[List[Tool]] =
      withResilience(underlying.listTools)

    def callTool[A: Encoder](name: ToolName, arguments: A): F[ToolResult] =
      withResilience(underlying.callTool(name, arguments))

    @scala.annotation.targetName("callToolString")
    def callTool[A: Encoder](name: String, arguments: A): F[ToolResult] =
      withResilience(underlying.callTool(name, arguments))

    def callToolIfSupported[A: Encoder](name: ToolName, arguments: A): F[Option[ToolResult]] =
      withResilience(underlying.callToolIfSupported(name, arguments))

    def listResources: F[List[Resource]] =
      withResilience(underlying.listResources)

    def listResourceTemplates: F[List[ResourceTemplate]] =
      withResilience(underlying.listResourceTemplates)

    def readResource(uri: ResourceUri): F[ResourceContent] =
      withResilience(underlying.readResource(uri))

    @scala.annotation.targetName("readResourceString")
    def readResource(uri: String): F[ResourceContent] =
      withResilience(underlying.readResource(uri))

    def readResourceIfSupported(uri: ResourceUri): F[Option[ResourceContent]] =
      withResilience(underlying.readResourceIfSupported(uri))

    def listPrompts: F[List[Prompt]] =
      withResilience(underlying.listPrompts)

    def getPrompt[A: Encoder](name: PromptName, arguments: A): F[GetPromptResult] =
      withResilience(underlying.getPrompt(name, arguments))

    @scala.annotation.targetName("getPromptString")
    def getPrompt[A: Encoder](name: String, arguments: A): F[GetPromptResult] =
      withResilience(underlying.getPrompt(name, arguments))

    def getPromptIfSupported[A: Encoder](name: PromptName, arguments: A): F[Option[GetPromptResult]] =
      withResilience(underlying.getPromptIfSupported(name, arguments))

    def ping: F[Unit] =
      withResilience(underlying.ping)

    def shutdown: F[Unit] =
      // Don't apply resilience to shutdown - we want it to fail fast
      underlying.shutdown

    def cancel(requestId: RequestId, reason: Option[String]): F[Unit] =
      // Don't apply resilience to cancel - best effort
      underlying.cancel(requestId, reason)

/** Extension methods for McpConnection to add resilience */
extension [F[_]: Temporal](connection: McpConnection[F])

  /** Add retry logic to the connection.
    *
    * Example:
    * {{{
    * val resilient = connection.withRetry(
    *   RetryPolicy.exponentialBackoff(maxRetries = 5)
    * )
    * }}}
    */
  def withRetry(policy: RetryPolicy): F[ResilientMcpConnection[F]] =
    ResilientConnection(connection, ResilienceConfig.retryOnly(policy))

  /** Add timeout to the connection.
    *
    * Example:
    * {{{
    * val resilient = connection.withTimeout(30.seconds)
    * }}}
    */
  def withTimeout(duration: FiniteDuration): F[ResilientMcpConnection[F]] =
    ResilientConnection(connection, ResilienceConfig.timeoutOnly(duration))

  /** Add circuit breaker to the connection.
    *
    * Example:
    * {{{
    * val resilient = connection.withCircuitBreaker(
    *   CircuitBreakerConfig(failureThreshold = 3, resetTimeout = 1.minute)
    * )
    * }}}
    */
  def withCircuitBreaker(config: CircuitBreakerConfig): F[ResilientMcpConnection[F]] =
    ResilientConnection(
      connection,
      ResilienceConfig(
        retryPolicy = RetryPolicy.noRetry,
        circuitBreaker = Some(config),
        timeout = None
      )
    )

  /** Add full resilience to the connection (retry + circuit breaker + timeout).
    *
    * Example:
    * {{{
    * val resilient = connection.withResilience(ResilienceConfig.default)
    * }}}
    */
  def withResilience(config: ResilienceConfig = ResilienceConfig.default): F[ResilientMcpConnection[F]] =
    ResilientConnection(connection, config)

/** Extension methods for ResilientMcpConnection to modify resilience settings.
  *
  * These allow chaining to adjust resilience configuration after initial wrapping.
  */
extension [F[_]: Temporal](connection: ResilientMcpConnection[F])

  /** Update retry policy on an already-resilient connection.
    *
    * Example:
    * {{{
    * connection
    *   .withResilience()
    *   .flatMap(_.withUpdatedRetry(RetryPolicy.fixedDelay(maxRetries = 10)))
    * }}}
    */
  def withUpdatedRetry(policy: RetryPolicy): F[ResilientMcpConnection[F]] =
    val newConfig = ResilienceConfigBuilder
      .from(connection.resilienceConfig)
      .withRetry(policy)
      .build
    ResilientConnection(connection, newConfig)

  /** Update timeout on an already-resilient connection. */
  def withUpdatedTimeout(duration: FiniteDuration): F[ResilientMcpConnection[F]] =
    val newConfig = ResilienceConfigBuilder
      .from(connection.resilienceConfig)
      .withTimeout(duration)
      .build
    ResilientConnection(connection, newConfig)

  /** Update circuit breaker on an already-resilient connection. */
  def withUpdatedCircuitBreaker(config: CircuitBreakerConfig): F[ResilientMcpConnection[F]] =
    val newConfig = ResilienceConfigBuilder
      .from(connection.resilienceConfig)
      .withCircuitBreaker(config)
      .build
    ResilientConnection(connection, newConfig)

  /** Update the full resilience configuration. */
  def withUpdatedResilience(config: ResilienceConfig): F[ResilientMcpConnection[F]] =
    ResilientConnection(connection, config)
