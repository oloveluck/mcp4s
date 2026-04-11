# Architecture

## Design Philosophy

**AI infrastructure must be predictable.**

MCP4S is built on http4s and Cats Effect because protocol implementations must be reliable:

- **Resource safety** — Connections, files, processes clean up deterministically via `Resource[F, A]`
- **Backpressure** — fs2 streams handle flow control naturally
- **Typed errors** — Failures are values in `F[_]`, not runtime surprises that crash your system
- **Scalability** — Fiber-based concurrency handles thousands of connections per node

The entire library is generic over `F[_]` — you choose your effect type, and the compiler guarantees resource safety.

## Module Structure

```
mcp4s/
├── core/     # Protocol types, codecs, JSON-RPC
├── server/   # Server, tools, resources, prompts, transports
└── client/   # McpClient, McpConnection, resilience, transports
```

- **core** contains the MCP protocol types that both server and client share — `Tool`, `Resource`, `Prompt`, `ToolResult`, JSON-RPC framing, and type derivation macros.
- **server** provides the DSL for building MCP servers and all transport implementations.
- **client** provides `McpClient` (handles capability negotiation) and `McpConnection` (the active session).

## Server Flow

```
Server[F]
├── Tools[F]      ─┐
├── Resources[F]   ├─→ Dispatcher ─→ Transport (HTTP/WS/Stdio)
└── Prompts[F]    ─┘
```

Incoming JSON-RPC requests are dispatched to the appropriate handler based on the method name. The transport layer handles framing, sessions, and protocol details.

## Client Flow

```
McpClient[F] ─→ Transport.connect() ─→ McpConnection[F]
                                            │
                                    ┌───────┴───────┐
                                    │ Resilience    │
                                    │ (retry/cb/to) │
                                    └───────────────┘
```

The client performs capability negotiation during `connect()`. The resulting `McpConnection` only enables operations the server supports.

## Capability Negotiation

Client and server exchange capabilities during `initialize`:

```
Client: { sampling: {}, roots: {} }
Server: { tools: {}, resources: {}, prompts: {} }
```

Features only activate when both sides support them. For example, sampling only works if the client declares sampling support and the server actually uses it.

## Composition

A key design principle is that everything composes with standard typeclasses:

- **Tools, Resources, Prompts** combine with `|+|` (Semigroup)
- **Servers** combine with `<+>` (SemigroupK)
- **Hooks** combine with `|+|` (Semigroup)

This means you build small pieces and snap them together — no inheritance hierarchies, no plugin systems, just composition.
