# MCP4S

A Scala implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) for the Typelevel ecosystem.

[Documentation](https://oloveluck.github.io/mcp4s/) | [API Reference](https://oloveluck.github.io/mcp4s/api/)

## Overview

MCP4S provides a type-safe, functional implementation of MCP for Scala 3 using cats-effect, fs2, http4s, and circe. It enables Scala applications to act as MCP servers (providing tools, resources, and prompts) or clients (consuming MCP servers).

## Installation

Add to your `build.mill`:

```scala
def ivyDeps = Agg(
  ivy"io.github.mcp4s::mcp4s-core::0.1.5",
  ivy"io.github.mcp4s::mcp4s-server::0.1.5",  // for servers
  ivy"io.github.mcp4s::mcp4s-client::0.1.5"   // for clients
)
```

Or in sbt:

```scala
libraryDependencies ++= Seq(
  "io.github.mcp4s" %% "mcp4s-core" % "0.1.5",
  "io.github.mcp4s" %% "mcp4s-server" % "0.1.5",
  "io.github.mcp4s" %% "mcp4s-client" % "0.1.5"
)
```

## Quick Start

See the [documentation site](https://oloveluck.github.io/mcp4s/getting-started/) for server and client examples.

## Modules

| Module | Description |
|--------|-------------|
| **core** | Protocol types, JSON-RPC messages, codec definitions |
| **server** | MCP server with Streamable HTTP, WebSocket, and stdio transports |
| **client** | MCP client for connecting to servers |

## Development

```bash
mill __.compile          # Compile all modules
mill __.test             # Run all tests
mill conformance         # Run MCP conformance tests (requires Node.js 18+)
mill __.publishLocal     # Publish locally
```

See the [documentation site](https://oloveluck.github.io/mcp4s/) for guides on tools, resources, prompts, transports, and testing.

## License

Apache 2.0 - see [LICENSE](LICENSE) for details.

## Links

- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [MCP Documentation](https://modelcontextprotocol.io/)
