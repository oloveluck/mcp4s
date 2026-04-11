# Version Support

Scala versions, JVM requirements, and dependency versions for mcp4s.

## Scala Versions

| Version | Status | Notes |
|---------|--------|-------|
| Scala 3.3.4 | Supported | LTS — widest ecosystem compatibility |
| Scala 3.6.4 | Supported | Latest — newer language features |

All modules are cross-compiled for both versions. CI tests run against both.

## JVM Requirements

- **Java 21+** is required
- Tested with GraalVM and Eclipse Temurin

## Dependencies

| Dependency | Version | Purpose |
|-----------|---------|---------|
| cats-effect | 3.6.0 | Async effect system |
| http4s | 0.23.33 | HTTP client and server |
| fs2 | 3.12.2 | Streaming |
| circe | 0.14.15 | JSON encoding/decoding |
| otel4s | 0.11.2 | OpenTelemetry observability |

## MCP Protocol Version

mcp4s implements the **MCP 2025-11-25** protocol specification.

## Build Tool

mcp4s uses [Mill](https://mill-build.org/) as its build tool. The repository includes a bootstrap script, so you don't need to install Mill globally — just run `./mill` from the project root.
