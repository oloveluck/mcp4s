# WebSocket Transport

## Server

```scala
import mcp4s.server.transport.*

WebSocketTransport.serve[IO](server, WebSocketConfig(
  host = "0.0.0.0",
  port = 3000,
  path = "ws"
))
```

## Client

```scala
import mcp4s.client.transport.*

WebSocketClientTransport.connect[IO](client, WebSocketClientConfig(
  url = "ws://localhost:3000",
  path = "ws"
)).use { conn => ... }
```

## When to Use

**WebSocket** — Lower latency, native bidirectional, real-time updates

**HTTP** — Simpler load balancing, better firewall support, stateless
