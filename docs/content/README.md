# MCP4S

A pure functional Scala 3 implementation of the [Model Context Protocol](https://spec.modelcontextprotocol.io/).

## What is MCP?

The **Model Context Protocol** (MCP) is an open standard for connecting AI models to external tools and data sources. A server exposes capabilities, and any MCP-compatible client (Claude Desktop, IDE agents, custom apps) can discover and use them without custom integration work.

MCP defines three core primitives:

- **Tools** — Functions the AI can call (search a database, send an email, run a calculation)
- **Resources** — Data the AI can read (files, database records, API responses)
- **Prompts** — Reusable message templates (code review templates, analysis workflows)

> Learn more: [MCP introduction](https://modelcontextprotocol.io/introduction) | [Full specification](https://modelcontextprotocol.io/specification/2025-11-25/)

## Quick Example

```scala
import cats.effect.*
import mcp4s.protocol.ServerInfo
import mcp4s.server.*
import mcp4s.server.dsl.*

@description("Add two numbers")
case class AddArgs(a: Double, b: Double) derives Schema

val tools = Tool.from[AddArgs].handle[IO](args => IO.pure(ok(s"${args.a + args.b}")))

val server = McpServer[IO](ServerInfo("calculator", "1.0.0")).withTools(tools)
```

## What mcp4s Provides

- **One unified `Schema`** — `derives Schema` describes a type once; the JSON Schema, encoder, decoder, and prompt-argument metadata are all derived from it and can never disagree
- **Endpoint definitions shared by both sides** — define a `Tool`/`Prompt` endpoint once, attach a handler on the server, call it type-safely from the client (`McpService` + `TypedClient`)
- **Cats Effect resource safety** — connections, processes, and transports clean up deterministically via `Resource`
- **Multiple transports** — Streamable HTTP, stdio, and WebSocket out of the box, with one verb per transport (`server.stdio.run`, `server.http().resource`, `client.http(...)`)
- **Bidirectional communication** — servers can request LLM completions (sampling) and user input (elicitation) over both HTTP (SSE) and WebSocket

## Installation

<!-- doc-snippet: skip -->
```scala
libraryDependencies ++= Seq(
  "io.github.oloveluck" %%% "mcp4s-core" % "0.2.0",
  "io.github.oloveluck" %%% "mcp4s-server" % "0.2.0",
  "io.github.oloveluck" %%% "mcp4s-client" % "0.2.0"
)
```

`%%%` cross-resolves for the JVM, Scala.js, and Scala Native; use `%%` for a JVM-only project.

## Modules

| Module | Purpose | Guide |
|--------|---------|-------|
| `mcp4s-core` | Protocol types, codecs, and the `Schema`/endpoint layer | [Protocol Reference](reference/protocol.md) |
| `mcp4s-server` | Server with DSL for tools, resources, prompts | [Server Guide](server/README.md) |
| `mcp4s-client` | Client with typed endpoint calls | [Client Guide](client/README.md) |
| `mcp4s-testkit` | Compliance and performance suites for any MCP server | [Testing](testing/README.md) |

## Where to Start

1. [Getting Started](getting-started/README.md) — a working server and client in minutes
2. [Your First Server](getting-started/first-server.md) / [Your First Client](getting-started/first-client.md)
3. [Services](server/services.md) — define endpoints once, share them between server and client
4. [Transports](transports/README.md) — stdio for Claude Desktop, HTTP or WebSocket for the network
