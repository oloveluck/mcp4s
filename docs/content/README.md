# MCP4S

A pure functional Scala 3 implementation of the [Model Context Protocol](https://spec.modelcontextprotocol.io/).

## What is MCP?

The **Model Context Protocol** (MCP) is an open standard for connecting AI models to external tools and data sources. A server exposes capabilities, and any MCP-compatible client (Claude Desktop, IDE agents, custom apps) can discover and use them without custom integration work.

MCP defines three core primitives:

- **Tools** — Functions the AI can call (search a database, send an email, run a calculation)
- **Resources** — Data the AI can read (files, database records, API responses)
- **Prompts** — Reusable message templates (code review templates, analysis workflows)

> Learn more: [MCP introduction](https://modelcontextprotocol.io/introduction) | [Full specification](https://spec.modelcontextprotocol.io/specification/2025-03-26/)

## Quick Example

```scala
import cats.effect.*
import mcp4s.server.*
import mcp4s.server.mcp.*
import mcp4s.protocol.*

@description("Add two numbers")
case class AddArgs(a: Double, b: Double) derives ToolInput

val tools = Tool[IO, AddArgs] { args =>
  IO.pure(ok(s"${args.a + args.b}"))
}

val server = Server.from[IO](
  info = ServerInfo("calculator", "1.0.0"),
  tools = tools
)
```

## What mcp4s provides

- **Type-safe tool derivation** — `derives ToolInput` generates JSON schemas from case classes at compile time
- **Cats Effect resource safety** — connections, processes, and transports clean up deterministically via `Resource`
- **Multiple transports** — HTTP, stdio, and WebSocket out of the box
- **Client resilience** — retry policies and timeouts built in
- **Bidirectional communication** — servers can request LLM completions (sampling) and user input (elicitation)

## Installation

```scala
// Mill
mvn"io.github.oloveluck::mcp4s-server::0.1.8"
mvn"io.github.oloveluck::mcp4s-client::0.1.8"

// SBT
"io.github.oloveluck" %% "mcp4s-server" % "0.1.8"
"io.github.oloveluck" %% "mcp4s-client" % "0.1.8"
```

## Modules

| Module | Purpose | Guide |
|--------|---------|-------|
| `mcp4s-core` | Protocol types and codecs | [Protocol Reference](reference/protocol.md) |
| `mcp4s-server` | Server with DSL for tools, resources, prompts | [Server Guide](server/README.md) |
| `mcp4s-client` | Client with resilience patterns | [Client Guide](client/README.md) |
