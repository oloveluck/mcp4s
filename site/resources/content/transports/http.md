# HTTP Transport

MCP over HTTP uses a request/response model with **Server-Sent Events (SSE)** for server-to-client messages. This is the most common transport for production deployments — it works through load balancers, firewalls, and proxies without special configuration.

> For the full protocol details, see [Streamable HTTP Transport](https://spec.modelcontextprotocol.io/specification/2025-03-26/transport/http/) in the MCP specification.

The client sends JSON-RPC requests via HTTP POST, and the server streams responses and notifications back over an SSE connection.

## Server

```scala
import mcp4s.server.transport.*

// Simple — starts an Ember server on port 3000
server.serveHttp.useForever

// With custom port
server.serveHttp(port"8080").useForever

// With full config
HttpTransport.serve[IO](server, HttpConfig(
  host = host"0.0.0.0",
  port = port"3000",
  path = "mcp",
  enableSessions = true
))
```

### Composable Routes

Use `HttpTransport.routes` to get raw `HttpRoutes[F]` that you can embed in an existing http4s application and wrap with standard middleware:

```scala
import mcp4s.server.transport.*
import org.http4s.server.middleware.CORS
import org.http4s.server.Router
import org.http4s.ember.server.EmberServerBuilder

HttpTransport.routes[IO](server).flatMap { mcpRoutes =>
  // Wrap with CORS
  val withCors = CORS.policy
    .withAllowOriginAll
    .withAllowCredentials(false)
    .apply(mcpRoutes)

  // Combine with your own routes
  val allRoutes = withCors <+> myAppRoutes

  EmberServerBuilder.default[IO]
    .withHttpApp(Router("/" -> allRoutes).orNotFound)
    .build
}
```

See [HTTP Security](../server/auth) for authentication middleware examples.

## Client

```scala
import mcp4s.client.transport.*

HttpClientTransport.connect[IO](client, HttpClientConfig(
  baseUrl = "http://localhost:3000",
  endpoint = "/mcp"
), httpClient).use { conn => ... }

// With resilience
import mcp4s.client.*

HttpClientTransport.connect[IO](client, HttpClientConfig(
  baseUrl = "http://localhost:3000",
  endpoint = "/mcp"
), httpClient,
  resilience = Some(ResilienceConfig.default)
).use { conn => ... }
```

## Features

- Session management via `Mcp-Session-Id` header
- SSE for server-to-client messages (notifications, progress, sampling requests)
- OpenTelemetry trace propagation
- DNS rebinding protection (automatic for localhost bindings)
