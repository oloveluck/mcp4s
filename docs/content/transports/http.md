# HTTP Transport

MCP over HTTP uses a request/response model with **Server-Sent Events (SSE)** for server-to-client messages. This is the most common transport for production deployments — it works through load balancers, firewalls, and proxies without special configuration.

> For the full protocol details, see [Streamable HTTP Transport](https://modelcontextprotocol.io/specification/2025-11-25/basic/transports#streamable-http) in the MCP specification.

The client sends JSON-RPC requests via HTTP POST. When the server responds with an SSE stream, every event on it — progress notifications, server-initiated requests such as sampling and elicitation, and the final response — is surfaced to the client, so **bidirectional flows work exactly as they do over WebSocket**.

## Server

```scala
// Simple — starts an Ember server on port 3000, path /mcp
server.http().resource.useForever

// With custom config
import mcp4s.server.transport.*
import com.comcast.ip4s.*

server.http(HttpConfig(
  host = host"0.0.0.0",
  port = port"3000",
  path = "mcp",
  enableSessions = true
)).resource.useForever
```

`server.http(...)` is available on `McpServer` and on any `Server[F]` — no import needed. `.resource` gives you the bound http4s `Server` as a managed `Resource`; `.run` is shorthand for `.resource.useForever`.

### HttpConfig

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `host` | `Host` | `host"0.0.0.0"` | Bind address (ip4s type-safe) |
| `port` | `Port` | `port"3000"` | Listen port (ip4s type-safe) |
| `path` | `String` | `"mcp"` | MCP endpoint path |
| `enableSessions` | `Boolean` | `true` | Session management via `Mcp-Session-Id` |
| `sessionConfig` | `SessionConfig` | `SessionConfig.default` | Session timeout (30 min), max queue size (1000), request timeout (5 min), max sessions (1000) |

### Composable Routes

Use `.routes` to get raw `HttpRoutes[F]` that you can embed in an existing http4s application and wrap with standard middleware:

```scala
import org.http4s.HttpRoutes
import org.http4s.server.middleware.CORS
import org.http4s.server.Router
import org.http4s.ember.server.EmberServerBuilder

def myAppRoutes: HttpRoutes[IO] = ???   // your existing routes

server.http(HttpConfig[IO]()).routes.flatMap: mcpRoutes =>
  // Wrap with CORS, combine with your own routes
  val withCors  = CORS.policy.withAllowOriginAll.withAllowCredentials(false).apply(mcpRoutes)
  val allRoutes = withCors <+> myAppRoutes
  EmberServerBuilder.default[IO].withHttpApp(Router("/" -> allRoutes).orNotFound).build
```

See [HTTP Security](../server/auth.md) for authentication middleware examples.

## Client

```scala
import mcp4s.client.syntax.*      // JVM-only convenience overloads
import mcp4s.client.transport.*

// JVM one-liner — builds and manages an Ember client for you
client.http("http://localhost:3000/mcp").use(conn => conn.listAllTools)

// Cross-platform — bring your own http4s Client[F]
client.http(HttpTransportConfig[IO]("http://localhost:3000/mcp"), httpClient).use(conn => conn.listAllTools)
```

The config takes the **full URI including the path** — there is no separate base URL/endpoint pair.

### HttpTransportConfig

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `uri` | `String` | — | Full MCP endpoint URI (e.g. `http://host:3000/mcp`) |
| `auth` | `Option[McpAuth[F]]` | `None` | Bearer token / token provider |
| `maxQueueSize` | `Int` | `1024` | Bounded inbox for incoming messages |
| `timeouts` | `Timeouts` | `Timeouts(request = 5.minutes, init = 30.seconds)` | Request and connect+initialize timeouts |

### Authentication

```scala
def fetchToken: IO[String] = ???   // your token refresh flow

val config = HttpTransportConfig[IO](
  uri  = "https://api.example.com/mcp",
  auth = Some(McpAuth.Bearer("my-token"))              // static token
)

// or resolve a fresh token before each request:
val refreshing = config.copy(auth = Some(McpAuth.TokenProvider(fetchToken)))
```

### Retry

For retry, compose standard http4s middleware on the `Client[F]` you pass in (the
cross-platform overload), then connect:

```scala
import org.http4s.client.middleware.{Retry, RetryPolicy}
import scala.concurrent.duration.*

val retryPolicy = RetryPolicy[IO](RetryPolicy.exponentialBackoff(maxWait = 10.seconds, maxRetry = 3))
val resilientClient = Retry(retryPolicy)(httpClient)

client.http(HttpTransportConfig[IO]("http://localhost:3000/mcp"), resilientClient).use(conn => conn.listAllTools)
```

## Features

- Session management via `Mcp-Session-Id` header
- SSE for server-to-client messages — notifications, progress, **and server-initiated sampling/elicitation requests** (answered by the client on the same connection)
- OpenTelemetry trace propagation
- DNS rebinding protection (automatic for localhost bindings)
