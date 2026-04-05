# Client Guide

An MCP **client** connects to servers, discovers their capabilities, and calls tools, reads resources, and uses prompts. The client handles the protocol handshake (capability negotiation), session management, and optionally provides **sampling** (letting the server request LLM completions) and **elicitation** (letting the server prompt the user for input).

> For an overview of the client role in MCP, see the [MCP architecture](https://modelcontextprotocol.io/introduction#general-architecture) documentation.

In the MCP architecture, clients are the active side — they initiate connections and make requests. A single client can connect to multiple servers through different transports.

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

The `Tracer[IO]` is required for [OpenTelemetry](https://opentelemetry.io/) trace propagation. Use `Tracer.noop` to disable tracing, or provide a real tracer for distributed observability across MCP client-server interactions.

**Roots** tell the server which directories the client has access to. **Sampling** and **elicitation** handlers enable bidirectional features where the server can request help from the client.

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

The connection is a `Resource` — it handles initialization, capability negotiation, and cleanup automatically.

## Resilience

Production clients should use resilience wrappers for retry, timeout, and circuit breaker protection:

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

- [Connection Operations](connection) — Full McpConnection API
- [Resilience Patterns](resilience) — Retry, circuit breaker, timeout

---
**Next:** [Connection Operations](connection)
