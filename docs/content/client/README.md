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

import mcp4s.client.mcp.*

val client = McpClient.from[IO](
  ClientInfo("my-client", "1.0.0"),
  roots = Some(Roots[IO]("file:///workspace", "Workspace")),
  sampling = Some(Sampling[IO](params => myLlm.complete(params))),
  elicitation = Some(Elicitation[IO](params => askUser(params)))
)
```

A `Tracer[IO]` is needed as a type-class instance. Use `Tracer.noop` to disable tracing, or provide a real tracer for distributed observability.

**Roots** tell the server which directories the client has access to. **Sampling** and **elicitation** handlers enable bidirectional features where the server can request help from the client.

## Connecting

```scala
import mcp4s.client.syntax.*

// Stdio — spawn a subprocess
client.connectStdio("node", "server.js").use(conn => conn.callTool("add", args))

// HTTP — JVM one-liner (builds/manages an Ember client for you)
client.connectHttp("http://localhost:3000").use(conn => conn.callTool("add", args))

// HTTP — cross-platform: bring your own http4s Client[F]
client.connectHttp("http://localhost:3000", httpClient).use(conn => conn.callTool("add", args))

// WebSocket (JVM-only)
client.connectWebSocket("ws://localhost:3000").use(conn => conn.callTool("add", args))
```

The connection is a `Resource` — it handles initialization, capability negotiation, and cleanup automatically.

> `connectWebSocket` and the no-`Client` `connectHttp` are JVM-only. On JS/Native, use the
> cross-platform `connectHttp(url, httpClient)` / `connectStdio` and supply a platform
> `Client[F]`. For custom backends or middleware, call `HttpClientTransport` /
> `WebSocketClientTransport` / `StdioClientTransport` directly.

## Retry & Timeout

For HTTP transport, compose standard http4s middleware on your `Client[F]` before passing it to the transport:

```scala
import org.http4s.client.middleware.{Retry, RetryPolicy, Timeout}
import scala.concurrent.duration.*

val retryPolicy = RetryPolicy[IO](RetryPolicy.exponentialBackoff(maxWait = 10.seconds, maxRetry = 3))
val resilientClient = Timeout(30.seconds)(Retry(retryPolicy)(rawHttpClient))

// Pass the wrapped client to the cross-platform connectHttp overload
client.connectHttp("http://localhost:3000", resilientClient).use: conn =>
  conn.callTool("operation", args)
```

For WebSocket/Stdio transports, reconnection (re-establishing the transport) is the appropriate strategy for connection failures rather than per-message retry.

## Guide Contents

- [Connection Operations](connection.md) — Full McpConnection API

---
**Next:** [Connection Operations](connection.md)
