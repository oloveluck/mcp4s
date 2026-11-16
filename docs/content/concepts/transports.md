# Transports

A transport carries JSON-RPC messages between a client and a server. mcp4s ships the three the
spec defines, and each one is a thin framing layer over the same session engine, so your server
and client code does not change when the transport does.

| Transport | Carries messages as | Typical use | Server-initiated traffic |
|---|---|---|---|
| **stdio** | newline-delimited JSON on stdin/stdout | Claude Desktop and other hosts that spawn the server as a subprocess | same pipe, both directions |
| **Streamable HTTP** | one POST per client message; SSE streams for server messages | services behind load balancers and proxies | the request's SSE stream, or a standalone GET stream |
| **WebSocket** | text frames on one duplex socket | low-latency, chatty clients | same socket |

Choose stdio when a host launches you, HTTP when you deploy a service, and WebSocket when you
control both ends and want the lowest latency. Functionally they are equivalent: sampling,
elicitation, progress, subscriptions and tasks work on all three.

Binding is a verb on the server (`server.stdio.run`, `server.http().resource`,
`server.webSocket().resource`) and on the client (`client.stdio(...)`, `client.http(...)`,
`client.webSocket(...)`). Configuration objects live in `mcp4s.server.transport` and
`mcp4s.client.transport`; their fields are documented in the Scaladoc.

## stdio

The simplest mapping: the client writes one JSON-RPC message per line to the server's stdin and
reads one per line from its stdout. Everything else the server prints must go to stderr.

Requests are dispatched concurrently and responses are written in completion order (JSON-RPC
correlates by id, so order does not matter). This is what makes `notifications/cancelled`
useful: it can interrupt a running call because it is read while that call runs. Server-to-client
requests travel the same pipe and the client's answers are correlated back to the waiting tool.

`StdioTransport.runWith(server, input, output)` runs the same protocol over any byte streams,
which is how the transport is tested without a subprocess.

## Streamable HTTP

The transport most of the spec's text is about. The server exposes one endpoint (`/mcp` by
default) and three methods on it.

**POST** carries one client message. A *request* is answered either with a single JSON object or,
when the client accepts `text/event-stream`, with an SSE stream that carries everything the
handler emits for that request and ends with the response. *Notifications* and *responses* from
the client get `202 Accepted` with no body.

**GET** opens the session's *standalone* stream, for server messages that relate to no request:
a subscribed resource changing, or a task finishing after its original call returned. One per
session; the mcp4s client opens it automatically after the handshake.

**DELETE** ends the session.

Sessions are the unit of state. The server issues an `Mcp-Session-Id` on `initialize`, the client
sends it back on every later request, and requests with an unknown id get `404` so the client
re-initializes. The negotiated protocol version travels in `MCP-Protocol-Version`; an unsupported
one is refused with `400`. Set `enableSessions = false` for a stateless deployment: one shared
session, no header, `initialize` accepted repeatedly.

Two details are worth knowing because they shape behaviour you can observe:

- *Every streamed request has its own queue.* Progress, log lines and sampling requests emitted
  while handling a request appear on that request's stream and nowhere else; the spec forbids
  broadcasting. A client that drops the connection cancels the handler.
- *Streams are primed and kept alive.* The first SSE event carries an id and empty data so clients
  learn the id scheme; idle streams receive a comment every `keepAliveInterval` so proxies do not
  close them. Resumption with `Last-Event-ID` is not implemented.

Security: a server bound to localhost rejects requests whose `Host` or `Origin` is not local
(`403`), which defeats DNS rebinding. Deployed servers add authentication and CORS as ordinary
http4s middleware on `server.http(config).routes`; see [Security](../guides/security.md).

## WebSocket

One socket, text frames in both directions, no sessions to manage. Each connection gets its own
dispatcher, so state is per connection. Incoming frames are handled under a connection-scoped
supervisor: a handler still running when the socket closes is cancelled rather than leaked, and a
handler that awaits a sampling answer does not block the frames that carry it.

The WebSocket *client* transport is JVM-only; it is built on the JDK's HTTP client.

## Timeouts and resilience

Every client transport takes the same `Timeouts`: how long to wait for a response, and how long
the connect-plus-handshake may take. Server-initiated requests have their own timeout on the
server side, generous by default because a human or a model may be on the other end.

Retry is deliberately not built in. For HTTP, wrap the http4s `Client` you pass to the transport
in `Retry` middleware; for WebSocket and stdio, re-acquire the connection `Resource`.
