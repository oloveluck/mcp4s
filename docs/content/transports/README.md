# Transports

MCP supports three transport mechanisms. Choose based on your deployment model:

| Transport | Best For | Bidirectional |
|-----------|----------|---------------|
| [HTTP](http.md) | Production services, load-balanced deployments | Yes (via SSE) |
| [WebSocket](websocket.md) | Low-latency real-time communication | Yes (native) |
| [Stdio](stdio.md) | Claude Desktop, local subprocess servers | Yes (same pipe) |

All transports use the same JSON-RPC protocol — your server and client code stays the same regardless of transport. Binding one is a single verb on the value you already hold: `server.stdio.run` / `server.http().resource` / `server.webSocket().resource` on the server, and `client.stdio(...)` / `client.http(...)` / `client.webSocket(...)` on the client.

Server-initiated requests (sampling, elicitation) and server-pushed notifications (progress, logging, subscribed resource updates, task status) work over **all three** transports — Streamable HTTP delivers them on the SSE response stream of the originating request (or the standalone GET stream), WebSocket and stdio on their single duplex connection. One shared session engine drives every server transport, and one shared connection runner drives every client transport.
