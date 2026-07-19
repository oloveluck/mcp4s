# WebSocket Transport

Full-duplex communication over a single persistent connection.

## Server

```scala
import mcp4s.server.syntax.*
import com.comcast.ip4s.*

server.serveWebSocket().useForever          // defaults: port 3000, path /ws
server.serveWebSocket(port"3001").useForever

// Full configuration
import mcp4s.server.transport.*
server.serveWebSocket(WebSocketConfig(
  host = host"0.0.0.0",
  port = port"3000",
  path = "ws"
)).useForever
```

### WebSocketConfig

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `host` | `Host` | `host"0.0.0.0"` | Bind address (ip4s type-safe) |
| `port` | `Port` | `port"3000"` | Listen port (ip4s type-safe) |
| `path` | `String` | `"ws"` | WebSocket endpoint path |

## Client

```scala
import mcp4s.client.syntax.*

client.connectWebSocket("ws://localhost:3000").use: conn =>
  conn.callTool("add", args)

// Full configuration
import mcp4s.client.transport.*
val config = WebSocketClientConfig(url = "ws://localhost:3000", path = "ws")
client.connectWebSocket(config).use: conn =>
  conn.callTool("add", args)
```

> The WebSocket client transport is **JVM-only** (it uses http4s `JdkWSClient`).

### WebSocketClientConfig

| Parameter | Default | Description |
|-----------|---------|-------------|
| `url` | — | Server URL (`ws://` or `wss://`) |
| `path` | `"ws"` | WebSocket endpoint path |

## Reconnection

The WebSocket client transport handles connection lifecycle within the `Resource`. If the connection drops, the resource is released and must be re-acquired. For automatic reconnection, re-acquire the resource (e.g. with a retry loop around the `.use` block).

## When to Use

**Choose WebSocket when:**
- You need low-latency, real-time bidirectional communication
- The server pushes frequent notifications (progress, resource changes)
- You want a single persistent connection per client
- Your deployment supports long-lived connections

**Choose HTTP when:**
- You need simpler load balancing and horizontal scaling
- Firewall or proxy restrictions block WebSocket upgrades
- Connections are short-lived or infrequent
- You prefer stateless request/response semantics

**Choose Stdio when:**
- The client spawns the server as a subprocess
- You need the simplest possible transport (no networking)
