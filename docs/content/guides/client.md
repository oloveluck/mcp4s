# Client guide

A client connects to one server at a time, performs the handshake, and then calls tools, reads
resources and gets prompts. It may also answer the server: provide roots, run sampling, prompt
the user for elicitation, and receive notifications. All of that is configured by adding
handlers to a builder; what you add is what the client advertises.

## Building and connecting

```scala
import mcp4s.client.mcp.*
import mcp4s.client.syntax.*      // JVM: http(url) and webSocket(url) without a Client[F]
import mcp4s.client.transport.*
import mcp4s.protocol.*

def myLlm(p: CreateMessageParams): IO[CreateMessageResult] = ???

val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))
  .withRoots(Roots[IO]("file:///workspace", "Workspace"))
  .withSampling(Sampling[IO](myLlm))

// each transport is a verb returning Resource[IO, McpConnection[IO]]
val overStdio = client.stdio("node", "server.js")
val overHttp  = client.http("http://localhost:3000/mcp")
val overWs    = client.webSocket("ws://localhost:3000/ws")

// cross-platform HTTP: bring your own http4s Client (and auth, timeouts) through the config
val configured = client.http(
  HttpTransportConfig[IO]("https://api.example.com/mcp", auth = Some(McpAuth.Bearer("token"))),
  httpClient
)
```

The connection is a `Resource`: acquiring it connects and runs `initialize`, releasing it ends
the session (over HTTP, with a `DELETE`). URLs are the full endpoint, path included. The
no-`Client` `http` and the `webSocket` verbs are JVM-only; on JS and Native pass a platform
`Client[F]`. Tracing is off unless a `given Tracer[IO]` is in scope.

## Operations

```scala
import io.circe.Json, io.circe.syntax.*

overHttp.use: conn =>
  for
    tools   <- conn.listAllTools                     // follows pagination; listTools(cursor) for one page
    result  <- conn.callTool("add", Json.obj("a" -> 1.asJson, "b" -> 2.asJson))
    tracked <- conn.callTool("index", Json.obj(), p => IO.println(s"${p.progress}/${p.total}"))
    readme  <- conn.readResource("file:///readme")
    prompt  <- conn.getPrompt("greet", Map("name" -> "Ada"))
    _       <- conn.subscribeResource("config://app")
    _       <- conn.setLogLevel(LogLevel.Warning)
  yield ()
```

`conn.serverInfo`, `conn.serverCapabilities`, `conn.protocolVersion` and `conn.instructions` hold
the handshake result; `conn.supportsTools` and friends read the capabilities for you. Every call
is cancellable: cancelling the fiber sends `notifications/cancelled` to the server.

A result with `isError` is a *tool* failure the model may react to; it is returned, not raised.
Protocol failures are raised as `McpError`, connection failures as whatever the transport threw.
See [Errors](../reference/errors.md).

## Typed calls

With a shared `McpService` ([Services](server.md#services)), skip JSON entirely:

```scala
import mcp4s.client.TypedClient.*
import mcp4s.schema.{Schema, Tool as Endpoint}

case class AddArgs(a: Double, b: Double) derives Schema
case class AddResult(sum: Double) derives Schema
val add = Endpoint("add").input[AddArgs].output[AddResult]

overHttp.use(conn => conn.call(add)(AddArgs(1, 2)))   // IO[AddResult]
```

## Receiving from the server

Progress is a per-call callback (above). Everything else the server pushes outside a request
arrives through the notification handler as a `ServerNotification`: resource updates, list
changes, log messages, task status. Sampling and elicitation are requests the server makes and
your handlers answer. See [Server-to-client traffic](../concepts/server-to-client.md).

## Tasks

`conn.callToolTask(name, args)` starts a task-augmented call and returns the task; `getTask`,
`taskResult`, `cancelTask` and `listTasks` manage it. `conn.supportsToolTasks` tells you whether
the server accepts them.
