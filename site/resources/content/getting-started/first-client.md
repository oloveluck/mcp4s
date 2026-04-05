# Build Your First Client

A client connects to an MCP server, discovers what it offers, and starts making requests. This walkthrough connects to a server and exercises its tools, resources, and prompts.

## Create and Connect

```scala
import cats.effect.*
import mcp4s.client.*
import mcp4s.client.transport.*
import org.typelevel.otel4s.trace.Tracer

given Tracer[IO] = Tracer.noop[IO]

val client = McpClient.builder[IO]
  .withInfo(ClientInfo("my-client", "1.0.0"))
  .build

HttpClientTransport.connect[IO](client, HttpClientConfig("http://localhost:3000")).use { conn =>
  // Use connection
}
```

The `connect` call performs the MCP handshake — both sides exchange capabilities and the server reports its name, version, and supported features.

## Operations

Once connected, you can discover and use everything the server exposes:

```scala
import io.circe.Json

HttpClientTransport.connect[IO](client, config).use { conn =>
  for
    // Server info
    _ <- IO.println(s"Connected to: ${conn.serverInfo.name}")

    // Tools — call functions on the server
    tools <- conn.listTools
    result <- conn.callTool("add", Json.obj("a" -> Json.fromDouble(5).get, "b" -> Json.fromDouble(3).get))

    // Resources — read data from the server
    resources <- conn.listResources
    content <- conn.readResource("file:///readme")

    // Prompts — fetch message templates
    prompts <- conn.listPrompts
    prompt <- conn.getPrompt("greet", Map("name" -> "Alice"))
  yield ()
}
```

## WebSocket Transport

Same API, different transport. Use WebSocket for lower latency and real-time bidirectional communication:

```scala
WebSocketClientTransport.connect[IO](client, WebSocketClientConfig("ws://localhost:3000", "ws")).use { conn =>
  conn.callTool("add", args)
}
```

## Error Handling

MCP errors carry a numeric code and message. Use `.attempt` to handle them gracefully:

```scala
import mcp4s.protocol.McpError

conn.callTool("unknown", Map.empty).attempt.flatMap {
  case Right(result) => IO.println(s"Success: $result")
  case Left(e: McpError) => IO.println(s"MCP error ${e.code}: ${e.message}")
  case Left(e) => IO.println(s"Error: ${e.getMessage}")
}
```

## Capability Checks

Not all servers support all features. Check before calling:

```scala
if conn.supportsTools then conn.callTool("add", args)
else IO.println("Tools not supported")

// Or use conditional methods that return Option
conn.callToolIfSupported("add", args)  // Returns Option[ToolResult]
```

---
**Next:** [Server Guide](../server/)
