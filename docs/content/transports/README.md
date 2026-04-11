# Transports

MCP supports three transport mechanisms. Choose based on your deployment model:

| Transport | Best For | Bidirectional |
|-----------|----------|---------------|
| [HTTP](http.md) | Production services, load-balanced deployments | Yes (via SSE) |
| [WebSocket](websocket.md) | Low-latency real-time communication | Yes (native) |
| [Stdio](stdio.md) | Claude Desktop, local subprocess servers | No |

All transports use the same JSON-RPC protocol — your server and client code stays the same regardless of transport.
