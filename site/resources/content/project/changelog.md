# Changelog

## [0.1.6] - 2026-04-05

### Added
- Fluent builder methods on `ResilienceConfig` (`withRetry`, `withTimeout`, `withoutTimeout`)
- WebSocket transport resilience integration tests
- Resilient client example (`ResilientClient`)
- Docs site badge in README

### Fixed
- Stale docstrings in `RetryPolicy` referencing removed `connection.withRetry()` API

## [0.1.5] - 2026-04-05

### Removed
- Agent module
- Auth module (use standard http4s middleware for authentication)
- Server middleware (use standard error handling patterns)
- CircuitBreaker and ResilientConnection (resilience simplified to RetryPolicy + timeout at transport connect time)

### Changed
- Resilience applied at transport connect time via `ResilienceConfig` instead of post-connection wrapping
- Cross-build for Scala 3.3.4 and 3.6.4
- Publish to Maven Central
- Simplified type names (dropped `Mcp` prefix from most types)
- Unified `StreamingTools` into `Tools`
- Unified `McpSubscribableResource` into `McpResources`

## [0.1.4]

### Added
- Production auth with JWKS validation and token expiration
- Client-side progress notification routing
- Stdio transport (contributed by @pityka)

### Changed
- MCP protocol version 2025-03-26 → 2025-11-25
- Improved auth API design

### Fixed
- Progress notification via `_meta.progressToken`
- SSE event formatting

## [0.1.3]

### Added
- Server middleware (logging, timing, error handling)
- Streaming tools and `McpStreamingConnection`
- Resource subscriptions
- Server testing utilities
- RetryPolicy, ResilienceConfig (transport-level retry + timeout)
- MCP conformance testing in CI
- Dependabot and Scala Steward configuration

### Removed
- PostgreSQL module

### Fixed
- HttpSession shutdown handling
- SessionManager pruning

## [0.1.2] - 2025-01-29

### Added
- Unified DSL for MCP server construction (`mcp.Tool`, `mcp.Resource`, `mcp.Prompt`)
- `McpServer.from()` for compositional server building
- Monoid-based combining of tools, resources, and prompts with `|+|`

### Changed
- Client handler types renamed for consistency with server API:
  - `McpSampling[F]` → `McpSamplings[F]`
  - `McpElicitation[F]` → `McpElicitations[F]`

### Fixed
- HTTP client now requests JSON only instead of SSE for non-streaming endpoints

## [0.1.1] - 2025-01-XX

### Added
- Conformance testing infrastructure
- Property-based tests for protocol types
- HTTP session management for stateful transport

## [0.1.0] - Initial Release

- MCP protocol implementation for Scala 3
- Server and client modules
- HTTP and stdio transports
- PostgreSQL server module
