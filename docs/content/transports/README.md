# Transports

MCP supports three transport mechanisms. Choose based on your deployment model:

| Transport | Best For | Bidirectional |
|-----------|----------|---------------|
| [HTTP](http.md) | Production services, load-balanced deployments | Yes (via SSE) |
| [WebSocket](websocket.md) | Low-latency real-time communication | Yes (native) |
| [Stdio](stdio.md) | Claude Desktop, local subprocess servers | No |

All transports use the same JSON-RPC protocol — your server and client code stays the same regardless of transport. Binding one is a single verb on the value you already hold: `server.stdio.run` / `server.http().resource` / `server.webSocket().resource` on the server, and `client.stdio(...)` / `client.http(...)` / `client.webSocket(...)` on the client.

Server-initiated requests (sampling, elicitation) work over **both** network transports — Streamable HTTP delivers them on the SSE response stream, WebSocket on the same duplex connection. Stdio remains plain request/response.
