# Architecture

## Design Philosophy

**Non-deterministic agents need deterministic guardrails.**

MCP4S is built on http4s and Cats Effect because AI infrastructure must be predictable even when agents aren't:

- **Resource safety** — Connections, files, processes clean up deterministically
- **Backpressure** — Agents can't overwhelm services; fs2 streams handle flow control
- **Typed errors** — Failures are values in `F[_]`, not runtime surprises
- **Scalability** — Fiber-based concurrency handles thousands of connections per node

## Module Structure

```
mcp4s/
├── core/     # Protocol types, codecs
├── server/   # McpServer, tools, resources, prompts, transports
└── client/   # McpClient, McpConnection, resilience, transports
```

## Server Flow

```
McpServer[F]
├── McpTools[F]      ─┐
├── McpResources[F]   ├─→ Dispatcher ─→ Transport (HTTP/WS/Stdio)
└── McpPrompts[F]    ─┘
```

## Client Flow

```
McpClient[F] ─→ Transport.connect() ─→ McpConnection[F]
                                            │
                                    ┌───────┴───────┐
                                    │ Resilience    │
                                    │ (retry/cb/to) │
                                    └───────────────┘
```

## Capability Negotiation

Client and server exchange capabilities during `initialize`:

```
Client: { sampling: {}, roots: {} }
Server: { tools: {}, resources: {}, prompts: {} }
```

Features only activate when both sides support them.
