# Resilience

Production MCP clients need protection against transient failures — network blips, server restarts, overloaded endpoints. Resilience is applied at transport connect time by wrapping the underlying request function with retry and timeout before `McpConnection` is constructed.

## Configuration

All three transports accept an optional `resilience` parameter:

```scala
import mcp4s.client.*
import mcp4s.client.retry.*
import mcp4s.client.transport.*
import scala.concurrent.duration.*

// With custom resilience
HttpClientTransport.connect[IO](client, config, httpClient,
  resilience = Some(ResilienceConfig(
    retry = RetryPolicy.exponentialBackoff(maxRetries = 5),
    timeout = Some(30.seconds)
  ))
).use { conn =>
  conn.callTool("add", args)  // already resilient
}

// With defaults (exponential backoff, 3 retries, 30s timeout)
StdioClientTransport.connect[IO](client, stdioConfig,
  resilience = Some(ResilienceConfig.default)
).use { conn => ... }

// Without resilience (default)
WebSocketClientTransport.connect[IO](client, wsConfig).use { conn => ... }
```

## Retry Policies

### Exponential backoff (recommended)

```scala
RetryPolicy.exponentialBackoff(
  maxRetries = 3,           // default
  baseDelay = 100.millis,   // default
  maxDelay = 10.seconds,    // default
  jitterFactor = 0.1,       // default; 0.0 to 1.0
  retryOn = defaultRetryPredicate  // default
)
```

### Fixed delay

```scala
RetryPolicy.fixedDelay(
  maxRetries = 3,          // default
  delay = 1.second,        // default
  retryOn = defaultRetryPredicate  // default
)
```

### Linear backoff

```scala
RetryPolicy.linearBackoff(
  maxRetries = 3,            // default
  initialDelay = 100.millis, // default
  increment = 100.millis,    // default
  maxDelay = 10.seconds,     // default
  retryOn = defaultRetryPredicate  // default
)
```

### No retry

```scala
RetryPolicy.noRetry
```

## Custom retry predicates

Every policy accepts a `retryOn` parameter to control which errors trigger a retry. By default, `defaultRetryPredicate` retries on transient network and timeout errors:

- `java.net.ConnectException`
- `java.net.SocketTimeoutException`
- `java.net.UnknownHostException`
- `java.io.IOException`
- `java.util.concurrent.TimeoutException`
- Errors with message containing `"Connection refused"` or `"Connection reset"`

To override:

```scala
// Only retry on IOExceptions
RetryPolicy.exponentialBackoff(
  retryOn = {
    case _: java.io.IOException => true
    case _ => false
  }
)

// Retry on everything
RetryPolicy.fixedDelay(retryOn = _ => true)
```

## Defaults

`ResilienceConfig.default` provides sensible production defaults:

| Parameter | Default |
|-----------|---------|
| Retry policy | `exponentialBackoff(maxRetries = 3, baseDelay = 100ms, maxDelay = 10s, jitter = 0.1)` |
| Timeout | `30.seconds` per attempt |

```scala
// These are equivalent:
ResilienceConfig.default
ResilienceConfig()
ResilienceConfig(
  retry = RetryPolicy.exponentialBackoff(),
  timeout = Some(30.seconds)
)
```

## Layering

Per-attempt order:
1. **Timeout** — applied to each individual attempt
2. **Retry** — retries the operation on failure

The init handshake always uses the raw (unwrapped) function so it fails fast.

---
**Next:** [Transports](../transports/README.md)
