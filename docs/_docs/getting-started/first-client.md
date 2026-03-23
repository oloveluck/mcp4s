# Build Your First Client

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

## Operations

```scala
import io.circe.Json

HttpClientTransport.connect[IO](client, config).use { conn =>
  for
    // Server info
    _ <- IO.println(s"Connected to: ${conn.serverInfo.name}")

    // Tools
    tools <- conn.listTools
    result <- conn.callTool("add", Json.obj("a" -> Json.fromDouble(5).get, "b" -> Json.fromDouble(3).get))

    // Resources
    resources <- conn.listResources
    content <- conn.readResource("file:///readme")

    // Prompts
    prompts <- conn.listPrompts
    prompt <- conn.getPrompt("greet", Map("name" -> "Alice"))
  yield ()
}
```

## WebSocket Transport

```scala
WebSocketClientTransport.connect[IO](client, WebSocketClientConfig("ws://localhost:3000", "ws")).use { conn =>
  conn.callTool("add", args)
}
```

## Error Handling

```scala
import mcp4s.protocol.McpError

conn.callTool("unknown", Map.empty).attempt.flatMap {
  case Right(result) => IO.println(s"Success: $result")
  case Left(e: McpError) => IO.println(s"MCP error ${e.code}: ${e.message}")
  case Left(e) => IO.println(s"Error: ${e.getMessage}")
}
```

## Capability Checks

```scala
if conn.supportsTools then conn.callTool("add", args)
else IO.println("Tools not supported")

// Or use conditional methods
conn.callToolIfSupported("add", args)  // Returns Option[ToolResult]
```
