# HTTP Transport

## Server

```scala
import mcp4s.server.transport.*

// Simple
server.serveHttp(3000)  // Listens on /mcp

// With config
HttpTransport.serve[IO](server, HttpConfig(
  host = "0.0.0.0",
  port = 3000,
  path = "mcp",
  enableCors = true,
  enableSessions = true
))
```

### Composable Routes

```scala
val mcpRoutes = McpRoutes.of[IO](server)
val combined = mcpRoutes <+> myAppRoutes

EmberServerBuilder.default[IO]
  .withHttpApp(combined.orNotFound)
  .build
```

## Client

```scala
import mcp4s.client.transport.*

HttpClientTransport.connect[IO](client, HttpClientConfig(
  baseUrl = "http://localhost:3000",
  endpoint = "/mcp"
)).use { conn => ... }
```

## Features

- Session management via `Mcp-Session-Id` header
- SSE for server-to-client messages
- OpenTelemetry trace propagation
- CORS and DNS rebinding protection
