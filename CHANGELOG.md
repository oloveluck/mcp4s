# Changelog

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
