# MCP4S

A pure functional Scala 3 implementation of the [Model Context Protocol](https://spec.modelcontextprotocol.io/).

## Why MCP4S?

**Non-deterministic agents need deterministic guardrails.**

AI agents are inherently unpredictable. The infrastructure connecting them to tools and data must be rock-solid. MCP4S is built on http4s and Cats Effect because production AI systems demand:

- **Predictable resource management** — No leaked connections, no orphaned processes
- **Backpressure by default** — Agents can't overwhelm your services
- **Composable error handling** — Failures are values, not surprises
- **Practical scalability** — Thousands of concurrent connections on a single node

## Quick Example

```scala
import cats.effect.*
import mcp4s.server.*
import mcp4s.server.mcp.*
import mcp4s.protocol.ToolInput

case class AddArgs(a: Double, b: Double) derives ToolInput

val server = McpServer.builder[IO]
  .withInfo(ServerInfo("calculator", "1.0.0"))
  .tool[AddArgs]("add", "Add two numbers") { args =>
    IO.pure(ok(s"${args.a + args.b}"))
  }
  .build

// Serve on HTTP
server.serveHttp(3000)
```

## Installation

```scala
// Mill
mvn"io.github.mcp4s::mcp4s-server::0.1.3"
mvn"io.github.mcp4s::mcp4s-client::0.1.3"

// SBT
"io.github.mcp4s" %% "mcp4s-server" % "0.1.3"
"io.github.mcp4s" %% "mcp4s-client" % "0.1.3"
```

## Modules

| Module | Purpose |
|--------|---------|
| `mcp4s-core` | Protocol types and codecs |
| `mcp4s-server` | Server with DSL for tools, resources, prompts |
| `mcp4s-client` | Client with resilience patterns |
