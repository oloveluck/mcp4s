# WebSocket Transport

Full-duplex communication over a single persistent connection.

## Server

```scala
import mcp4s.server.transport.*

WebSocketTransport.serve[IO](server, WebSocketConfig(
  host = "0.0.0.0",
  port = 3000,
  path = "ws"
))
```

### WebSocketConfig

| Parameter | Default | Description |
|-----------|---------|-------------|
| `host` | `"0.0.0.0"` | Bind address |
| `port` | `3000` | Listen port |
| `path` | `"ws"` | WebSocket endpoint path |

## Client

```scala
import mcp4s.client.transport.*

WebSocketClientTransport.connect[IO](client, WebSocketClientConfig(
  url = "ws://localhost:3000",
  path = "ws"
)).use { conn =>
  conn.callTool("add", args)
}
```

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
