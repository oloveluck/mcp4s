# Resilience

Production MCP clients need protection against transient failures — network blips, server restarts, overloaded endpoints. The resilience module wraps an `McpConnection` with retry, timeout, and circuit breaker patterns.

## Configuration

```scala
import mcp4s.client.retry.*
import mcp4s.client.resilient.*
import scala.concurrent.duration.*

val config = ResilienceConfig.builder
  .withRetry(RetryPolicy.exponentialBackoff(maxRetries = 5))
  .withTimeout(30.seconds)
  .withCircuitBreaker(CircuitBreakerConfig(failureThreshold = 3))
  .build

conn.withResilience(config)
```

## Retry Policies

```scala
// Exponential backoff (recommended)
RetryPolicy.exponentialBackoff(
  maxRetries = 5,
  baseDelay = 100.millis,
  maxDelay = 30.seconds,
  jitterFactor = 0.1
)

// Fixed delay
RetryPolicy.fixedDelay(maxRetries = 3, delay = 1.second)

// Linear backoff
RetryPolicy.linearBackoff(maxRetries = 3, initialDelay = 100.millis, increment = 100.millis)

// No retry
RetryPolicy.noRetry
```

## Circuit Breaker

A circuit breaker prevents cascading failures by stopping requests to a failing server. After enough failures, the circuit **opens** and fast-fails all requests. After a timeout, it lets a test request through (**half-open**). If that succeeds, the circuit **closes** again.

```scala
CircuitBreakerConfig(
  failureThreshold = 5,    // Failures before opening
  resetTimeout = 30.seconds,  // Time before testing
  successThreshold = 2     // Successes to close
)

// States: Closed → Open → HalfOpen → Closed
```

## Monitoring

```scala
resilient.circuitBreakerStats  // IO[Option[CircuitBreakerStats]]
// stats.state, stats.failures, stats.successes, stats.totalRequests
```

## Layering

Per-attempt order:
1. **Timeout** — per attempt
2. **Circuit Breaker** — tracks failures
3. **Retry** — retries operation

---
**Next:** [Transports](../transports/)
