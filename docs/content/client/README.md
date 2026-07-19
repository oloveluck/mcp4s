# Client Guide

An MCP **client** connects to servers, discovers their capabilities, and calls tools, reads resources, and uses prompts. The client handles the protocol handshake (capability negotiation), session management, and optionally provides **sampling** (letting the server request LLM completions) and **elicitation** (letting the server prompt the user for input).

> For an overview of the client role in MCP, see the [MCP architecture](https://modelcontextprotocol.io/introduction#general-architecture) documentation.

In the MCP architecture, clients are the active side — they initiate connections and make requests. A single client can connect to multiple servers through different transports.

## Creating a Client

`McpClientBuilder` is the one entry point — the mirror image of `McpServer` on the server side:

```scala
import cats.effect.*
import mcp4s.client.McpClientBuilder
import mcp4s.client.mcp.*
import mcp4s.protocol.*

def myLlm(params: CreateMessageParams): IO[CreateMessageResult] = ???   // your LLM integration
def askUser(params: ElicitParams): IO[ElicitResult] = ???               // your UI integration

val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))
  .withRoots(Roots[IO]("file:///workspace", "Workspace"))
  .withSampling(Sampling[IO](params => myLlm(params)))
  .withElicitation(Elicitation[IO](params => askUser(params)))
```

Tracing uses otel4s and defaults to `Tracer.noop` — bring a `given Tracer[IO]` into scope to enable distributed observability.

**Roots** tell the server which directories the client has access to. **Sampling** and **elicitation** handlers enable bidirectional features where the server can request help from the client. Advertised client capabilities are **derived** from which handlers you add — no manual capability flags.

`McpClient.from(info, roots, sampling, elicitation)` remains as the compositional alternative when you want a plain `McpClient[F]` value.

## Connecting

Each transport is a verb on the builder (or on any `McpClient[F]`), returning a `Resource[F, McpConnection[F]]`:

```scala
import mcp4s.client.syntax.*   // JVM-only verbs: webSocket, auto-Ember http
import mcp4s.client.transport.*
import io.circe.Json, io.circe.syntax.*

val args = Json.obj("a" -> 1.asJson, "b" -> 2.asJson)

// Stdio — spawn a subprocess
client.stdio("node", "server.js").use(conn => conn.callTool("add", args))

// HTTP — JVM one-liner (builds/manages an Ember client for you)
client.http("http://localhost:3000/mcp").use(conn => conn.callTool("add", args))

// HTTP — cross-platform: bring your own http4s Client[F]
client.http(HttpTransportConfig[IO]("http://localhost:3000/mcp"), httpClient)
  .use(conn => conn.callTool("add", args))

// WebSocket (JVM-only)
client.webSocket("ws://localhost:3000/ws").use(conn => conn.callTool("add", args))
```

Configs take the **full URI including the path** (`/mcp`, `/ws`). The connection is a `Resource` — it handles initialization, capability negotiation, and cleanup automatically.

> `webSocket` and the no-`Client` `http` overload are JVM-only (`import mcp4s.client.syntax.*`).
> On JS/Native, use the cross-platform `http(config, httpClient)` / `stdio` and supply a
> platform `Client[F]`. For custom backends, call `HttpClientTransport` /
> `WebSocketClientTransport` / `StdioClientTransport` directly.

## Authentication & Timeouts

The network transport configs carry auth and timeouts:

```scala
import mcp4s.client.transport.*
import mcp4s.transport.Timeouts
import scala.concurrent.duration.*

val config = HttpTransportConfig[IO](
  uri = "https://api.example.com/mcp",
  auth = Some(McpAuth.Bearer("my-token")),
  timeouts = Timeouts(request = 2.minutes, init = 10.seconds)
)
```

`McpAuth.Bearer` sends a static bearer token; `McpAuth.TokenProvider(io)` resolves a fresh token before each request (use for refresh flows). The same `McpAuth` works for WebSocket, where it's sent on the upgrade request. `Timeouts` applies to every transport, including stdio.

## Retry

For HTTP, compose standard http4s retry middleware on the `Client[F]` you pass to the transport — see [HTTP transport: Retry](../transports/http.md#retry). For WebSocket/Stdio, reconnection (re-acquiring the connection `Resource`) is the appropriate strategy rather than per-message retry.

## Typed Calls

If the server publishes an `McpService`, skip raw JSON entirely:

```scala
import mcp4s.client.TypedClient.*
import mcp4s.schema.{Schema, Tool as ToolDef}

// The endpoint definitions shared with the server (see Services)
case class AddArgs(a: Double, b: Double) derives Schema
case class AddResult(sum: Double) derives Schema

object Calculator:
  val add = ToolDef("add").input[AddArgs].output[AddResult]

conn.call(Calculator.add)(AddArgs(1, 2))   // : IO[AddResult]
```

See [Services](../server/services.md).

## Guide Contents

- [Connection Operations](connection.md) — Full McpConnection API

---
**Next:** [Connection Operations](connection.md)
