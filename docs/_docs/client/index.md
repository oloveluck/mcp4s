# Client Guide

## Creating a Client

```scala
import cats.effect.*
import mcp4s.client.*
import org.typelevel.otel4s.trace.Tracer

given Tracer[IO] = Tracer.noop[IO]

val client = McpClient.builder[IO]
  .withInfo(ClientInfo("my-client", "1.0.0"))
  .withRoots(List(Root("file:///workspace", Some("Workspace"))))
  .withSamplingHandler { params => myLlm.complete(params) }
  .withElicitationHandler { params => askUser(params) }
  .build
```

## Connecting

```scala
import mcp4s.client.transport.*

// HTTP
HttpClientTransport.connect[IO](client, HttpClientConfig("http://localhost:3000")).use { conn =>
  conn.callTool("add", args)
}

// WebSocket
WebSocketClientTransport.connect[IO](client, WebSocketClientConfig("ws://localhost:3000")).use { conn =>
  conn.callTool("add", args)
}
```

## Resilience

```scala
import mcp4s.client.retry.*
import mcp4s.client.resilient.*

val config = ResilienceConfig.builder
  .withRetry(RetryPolicy.exponentialBackoff(maxRetries = 5))
  .withTimeout(30.seconds)
  .withCircuitBreaker(CircuitBreakerConfig(failureThreshold = 3))
  .build

conn.withResilience(config).flatMap { resilient =>
  resilient.callTool("operation", args)
}
```

## Guide Contents

- [Connection Operations](connection.md) — Full API
- [Resilience Patterns](resilience.md) — Retry, circuit breaker, timeout
