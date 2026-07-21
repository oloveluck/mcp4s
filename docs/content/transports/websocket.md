# WebSocket Transport

Full-duplex communication over a single persistent connection.

## Server

```scala
import com.comcast.ip4s.*
import mcp4s.server.transport.*

server.webSocket().resource.useForever          // defaults: port 3000, path /ws

// Full configuration
server.webSocket(WebSocketConfig(
  host = host"0.0.0.0",
  port = port"3000",
  path = "ws"
)).resource.useForever
```

`server.webSocket(...)` is available on `McpServer` and on any `Server[F]` — no import needed. `.run` is shorthand for `.resource.useForever`.

### WebSocketConfig

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `host` | `Host` | `host"0.0.0.0"` | Bind address (ip4s type-safe) |
| `port` | `Port` | `port"3000"` | Listen port (ip4s type-safe) |
| `path` | `String` | `"ws"` | WebSocket endpoint path |
| `maxQueueSize` | `Int` | `1000` | Bounded queue for outgoing messages |
| `requestTimeout` | `FiniteDuration` | `5.minutes` | Timeout for server-to-client requests (e.g. sampling) |

## Client

```scala
import mcp4s.client.syntax.*   // JVM-only

val args = Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))

client.webSocket("ws://localhost:3000/ws").use: conn =>
  conn.callTool("add", args)

// Full configuration
import mcp4s.client.transport.*
import mcp4s.transport.Timeouts

client.webSocket(WebSocketTransportConfig[IO](
  uri  = "wss://api.example.com/ws",
  auth = Some(McpAuth.Bearer("my-token"))   // sent on the upgrade request
)).use: conn =>
  conn.callTool("add", args)
```

The URI is the **full endpoint** including the path (`/ws`).

> The WebSocket client transport is **JVM-only** (it uses http4s `JdkWSClient`, JDK 11+).

### WebSocketTransportConfig

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `uri` | `String` | — | Full server URI (`ws://` or `wss://`, path included) |
| `auth` | `Option[McpAuth[F]]` | `None` | Bearer token / token provider, sent on the upgrade request |
| `maxQueueSize` | `Int` | `1024` | Bounded queue for outgoing messages |
| `timeouts` | `Timeouts` | `Timeouts(request = 5.minutes, init = 30.seconds)` | Request and connect+initialize timeouts |

## Reconnection

The WebSocket client transport handles connection lifecycle within the `Resource`. If the connection drops, the resource is released and must be re-acquired. For automatic reconnection, re-acquire the resource (e.g. with a retry loop around the `.use` block).

## When to Use

**Choose WebSocket when:**
- You need low-latency, real-time communication
- The server pushes frequent notifications (progress, resource changes)
- You want a single persistent connection per client
- Your deployment supports long-lived connections

**Choose HTTP when:**
- You need simpler load balancing and horizontal scaling
- Firewall or proxy restrictions block WebSocket upgrades
- Connections are short-lived or infrequent
- You prefer stateless request/response semantics

Note that bidirectional features (sampling, elicitation) work over **both** — HTTP delivers server-initiated requests via SSE. WebSocket's advantage is latency and connection management, not capability.

**Choose Stdio when:**
- The client spawns the server as a subprocess
- You need the simplest possible transport (no networking)
