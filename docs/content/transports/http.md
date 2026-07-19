# HTTP Transport

MCP over HTTP uses a request/response model with **Server-Sent Events (SSE)** for server-to-client messages. This is the most common transport for production deployments — it works through load balancers, firewalls, and proxies without special configuration.

> For the full protocol details, see [Streamable HTTP Transport](https://spec.modelcontextprotocol.io/specification/2025-03-26/transport/http/) in the MCP specification.

The client sends JSON-RPC requests via HTTP POST, and the server streams responses and notifications back over an SSE connection.

## Server

```scala
import mcp4s.server.syntax.*

// Simple — starts an Ember server on port 3000
server.serveHttp().useForever

// With custom port
server.serveHttp(port"8080").useForever

// With full config
import mcp4s.server.transport.*
server.serveHttp(HttpConfig(
  host = host"0.0.0.0",
  port = port"3000",
  path = "mcp",
  enableSessions = true
)).useForever
```

### Composable Routes

Use `HttpTransport.routes` to get raw `HttpRoutes[F]` that you can embed in an existing http4s application and wrap with standard middleware:

```scala
import mcp4s.server.transport.*
import org.http4s.server.middleware.CORS
import org.http4s.server.Router
import org.http4s.ember.server.EmberServerBuilder

HttpTransport.routes[IO](server).flatMap: mcpRoutes =>
  // Wrap with CORS, combine with your own routes
  val withCors  = CORS.policy.withAllowOriginAll.withAllowCredentials(false).apply(mcpRoutes)
  val allRoutes = withCors <+> myAppRoutes
  EmberServerBuilder.default[IO].withHttpApp(Router("/" -> allRoutes).orNotFound).build
```

See [HTTP Security](../server/auth.md) for authentication middleware examples.

## Client

```scala
import mcp4s.client.syntax.*

// JVM one-liner — builds and manages an Ember client for you
client.connectHttp("http://localhost:3000").use(conn => ...)

// Cross-platform — bring your own http4s Client[F]
client.connectHttp("http://localhost:3000", httpClient).use(conn => ...)

// Custom endpoint via config
import mcp4s.client.transport.*
val config = HttpClientConfig(baseUrl = "http://localhost:3000", endpoint = "/mcp")
client.connectHttp(config, httpClient).use(conn => ...)
```

For retry/timeout, compose standard http4s middleware on the `Client[F]` you pass in (the
cross-platform overload), then connect:

```scala
import org.http4s.client.middleware.{Retry, RetryPolicy, Timeout}

val retryPolicy = RetryPolicy[IO](RetryPolicy.exponentialBackoff(maxWait = 10.seconds, maxRetry = 3))
val resilientClient = Timeout(30.seconds)(Retry(retryPolicy)(httpClient))

client.connectHttp("http://localhost:3000", resilientClient).use(conn => ...)
```

## Features

- Session management via `Mcp-Session-Id` header
- SSE for server-to-client messages (notifications, progress, sampling requests)
- OpenTelemetry trace propagation
- DNS rebinding protection (automatic for localhost bindings)
