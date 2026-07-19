# Architecture

## Design Philosophy

**AI infrastructure must be predictable.**

MCP4S is built on http4s and Cats Effect because protocol implementations must be reliable:

- **Resource safety** — Connections, files, processes clean up deterministically via `Resource[F, A]`
- **Backpressure** — fs2 streams handle flow control naturally
- **Typed errors** — Failures are values in `F[_]`, not runtime surprises that crash your system
- **Scalability** — Fiber-based concurrency handles thousands of connections per node

The entire library is generic over `F[_]` — you choose your effect type, and the compiler guarantees resource safety.

## One Schema, Many Interpreters

The design centerpiece (borrowed from smithy4s) is a single source of truth: `Schema[A]` in
`mcp4s.schema` is a reified description of a Scala type, and everything the protocol needs is an
interpreter over it —

```
Schema[A] ─┬─→ JSON Schema (advertised in tools/list)
           ├─→ circe Encoder/Decoder (the wire values)
           └─→ prompt arguments (names, descriptions, required flags)
```

Because all three derive from the same value, the advertised schema can never disagree with the
codec that validates arguments. `ToolEndpoint[I, O]` bundles a name with input/output schemas and
is the shared currency between server (attach a handler) and client (typed calls).

## Module Structure

```
mcp4s/
├── core/     # Protocol types, JSON-RPC framing, Schema + endpoints, McpChannel
├── server/   # DSL, Server, McpServer builder, ServiceRoutes, transports
├── client/   # McpClientBuilder, McpConnection, TypedClient, transports
└── testkit/  # Compliance + performance suites, deterministic test clients
```

All modules cross-build for JVM, Scala.js, and Scala Native (the WebSocket client is JVM-only).

## Server Flow

```
Tools[F] ─┐
Resources[F] ├─→ Server[F] ─→ Dispatcher ─→ transport binding (stdio / HTTP / WebSocket)
Prompts[F] ─┘
```

Incoming JSON-RPC requests are dispatched to the appropriate handler based on the method name.
Capabilities are derived from what is registered — empty routes advertise nothing. The transport
layer handles framing, sessions, and protocol details; the duplex transports share one
`ServerSession` for server-initiated requests (sampling, elicitation).

## Client Flow

```
McpClientBuilder[F] ─→ ClientTransport ─→ McpChannel (send + incoming stream)
                                               │
                                        ConnectionRunner
                          (handshake, correlation + timeouts, progress routing,
                           dispatch of server-initiated requests)
                                               │
                                          McpConnection[F]
```

Every transport implements the same message-level `McpChannel`; the shared `ConnectionRunner`
layers the MCP protocol on top. That is why sampling and elicitation work identically over HTTP
and WebSocket, and why every transport gets the same request timeouts. For resilience, compose
standard http4s middleware (`Retry`, `Timeout`) on the `Client[F]` you pass to the HTTP transport.

## Capability Negotiation

Client and server exchange capabilities during `initialize`:

```
Client: { sampling: {}, roots: {} }
Server: { tools: {}, resources: {}, prompts: {} }
```

Both sides derive their declarations from what you actually registered: adding `withSampling`
advertises sampling; registering a subscribable resource sets `resources.subscribe`. Features
only activate when both sides support them — a server tool calling `ctx.sampling` against a
client that never declared sampling fails with `McpError.SamplingNotSupported`.

## Composition

A key design principle is that everything composes with standard typeclasses:

- **Tools, Resources, Prompts** combine with `|+|` (Semigroup) — first match wins
- **Servers** combine with `|+|` (Semigroup)

You build small pieces and snap them together — no inheritance hierarchies, no plugin systems,
just composition.
