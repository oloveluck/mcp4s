# Transports

MCP supports three transport mechanisms. Choose based on your deployment model:

| Transport | Best For | Bidirectional |
|-----------|----------|---------------|
| [HTTP](http) | Production services, load-balanced deployments | Yes (via SSE) |
| [WebSocket](websocket) | Low-latency real-time communication | Yes (native) |
| [Stdio](stdio) | Claude Desktop, local subprocess servers | No |

All transports use the same JSON-RPC protocol — your server and client code stays the same regardless of transport.
