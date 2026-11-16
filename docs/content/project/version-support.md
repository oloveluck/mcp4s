# Version Support

Scala versions, JVM requirements, and dependency versions for mcp4s.

## Scala and Platforms

- **Scala 3.9.0** (Scala 3 only — no Scala 2 support)
- Cross-built for **JVM**, **Scala.js** (Node), and **Scala Native**

## JVM Requirements

- **Java 17+** (the build targets JDK 17 via `tlJdkRelease`)
- Tested with Eclipse Temurin

## Dependencies

| Dependency | Version | Purpose |
|-----------|---------|---------|
| cats-effect | 3.7.1 | Async effect system |
| http4s | 0.23.34 | HTTP client and server |
| fs2 | 3.14.0 | Streaming |
| circe | 0.14.16 | JSON encoding/decoding |
| otel4s | 1.1.0 | OpenTelemetry observability |

## MCP Protocol Version

mcp4s implements the **MCP 2025-11-25** protocol specification and negotiates down to 2024-11-05 with older peers (`McpVersion.Supported`). The in-progress draft revision (`2026-07-28`, stateless architecture) is not implemented.

## Build Tool

mcp4s builds with **sbt** and [sbt-typelevel](https://typelevel.org/sbt-typelevel/). See [Contributing](contributing.md) for setup.
