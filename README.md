# MCP4S

A Scala implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) for the Typelevel ecosystem.

[![Docs](https://img.shields.io/badge/docs-oloveluck.github.io%2Fmcp4s-blue)](https://oloveluck.github.io/mcp4s/) [![Maven Central](https://img.shields.io/maven-central/v/io.github.oloveluck/mcp4s-core_3)](https://central.sonatype.com/search?q=io.github.oloveluck)

## Overview

MCP4S provides a type-safe, functional implementation of MCP for Scala 3 using cats-effect, fs2, http4s, and circe. It enables Scala applications to act as MCP servers (providing tools, resources, and prompts) or clients (consuming MCP servers).

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies ++= Seq(
  "io.github.oloveluck" %%% "mcp4s-core" % "0.2.0",
  "io.github.oloveluck" %%% "mcp4s-server" % "0.2.0", // for servers
  "io.github.oloveluck" %%% "mcp4s-client" % "0.2.0"  // for clients
)
```

`%%%` cross-resolves the artifact for your platform — mcp4s is published for
the JVM, Scala.js (Node), and Scala Native. (Use `%%` for a JVM-only project.)

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
sbt compile              # Compile all modules (JVM + JS + Native)
sbt test                 # Run all tests
sbt conformance          # Run MCP conformance tests (requires Node.js 18+)
sbt publishLocal         # Publish locally

# Target a single platform with the rootJVM / rootJS / rootNative aggregates:
sbt rootJVM/test
```

See the [documentation site](https://oloveluck.github.io/mcp4s/) for guides on tools, resources, prompts, transports, and testing.

## License

Apache 2.0 - see [LICENSE](LICENSE) for details.

## Links

- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [MCP Documentation](https://modelcontextprotocol.io/)
