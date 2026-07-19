# Build Your First Client

A client connects to an MCP server, discovers what it offers, and starts making requests. This walkthrough connects to a server and exercises its tools, resources, and prompts.

## Create and Connect

```scala
import cats.effect.*
import mcp4s.client.*
import mcp4s.client.syntax.*
import org.typelevel.otel4s.trace.Tracer

given Tracer[IO] = Tracer.noop[IO]

val client = McpClient.from[IO](ClientInfo("my-client", "1.0.0"))

// JVM one-liner: builds and manages an Ember client for you
client.connectHttp("http://localhost:3000").use: conn =>
  IO.println(s"Connected to ${conn.serverInfo.name}")
```

The `connect` call performs the MCP handshake — both sides exchange capabilities and the server reports its name, version, and supported features.

## Operations

Once connected, you can discover and use everything the server exposes:

```scala
import io.circe.Json, io.circe.syntax.*

client.connectHttp("http://localhost:3000").use: conn =>
  for
    _         <- IO.println(s"Connected to: ${conn.serverInfo.name}")
    tools     <- conn.listAllTools                          // discover tools
    result    <- conn.callTool("add", Json.obj("a" -> 5.asJson, "b" -> 3.asJson))
    resources <- conn.listAllResources                      // discover resources
    content   <- conn.readResource("file:///readme")
    prompts   <- conn.listAllPrompts                        // discover prompts
    prompt    <- conn.getPrompt("greet", Map("name" -> "Alice"))
  yield ()
```

## WebSocket Transport

Same API, different transport. Use WebSocket for lower latency and real-time bidirectional communication (JVM-only):

```scala
client.connectWebSocket("ws://localhost:3000").use: conn =>
  conn.callTool("add", args)
```

## Error Handling

MCP errors carry a numeric code and message. Use `.attempt` to handle them gracefully:

```scala
import mcp4s.protocol.McpError

conn.callTool("unknown", Json.obj()).attempt.flatMap:
  case Right(result)     => IO.println(s"Success: $result")
  case Left(e: McpError) => IO.println(s"MCP error: ${e.message}")
  case Left(e)           => IO.println(s"Error: ${e.getMessage}")
```

## Capability Checks

Not all servers support all features. Check before calling:

```scala
if conn.supportsTools then conn.callTool("add", args)
else IO.println("Tools not supported")

// Or use conditional methods that return Option
conn.callToolIfSupported("add", args)  // Returns F[Option[ToolResult]]
```

---
**Next:** [Server Guide](../server/README.md)
