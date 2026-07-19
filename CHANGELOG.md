# Changelog

## [Unreleased]

### Changed (breaking) — smithy4s-style redesign

- **Unified `Schema` typeclass** (`mcp4s.schema.Schema`, exported by `mcp4s.server.dsl`) — a single `derives Schema` replaces `derives ToolInput`, `derives PromptInput`, and `derives ToolOutput, Encoder.AsObject`; the JSON Schema, circe codecs, and prompt-argument metadata are all derived from the same value. Supports nested case classes, Scala 3 enums (string enums), sealed traits with payloads (`oneOf` + `"type"` discriminator), `Map[String, V]`, `Option`, constructor defaults, `@description` on classes and fields, and recursion via `Schema.defer`.
- **Endpoint DSL** — `import mcp4s.server.dsl.*` replaces `import mcp4s.server.mcp.*`. The 16-overload `Tool` object is gone: build `Tool("name").withDescription(...).input[Args].output[Out].withAnnotations(...)`, then attach exactly one of `.handle` / `.handleWith` / `.stream` / `.streamWith`. `Tool.from[Args]` / `Prompt.from[Args]` derive name + description from the input type; prompts gain `.messages` / `.static` / `.input[Args].handle`. `.output[B]` advertises `outputSchema` and encodes results as `structuredContent` (primitives wrap as `{"result": ...}`).
- **Server assembly** — `McpServer[F](info).withTools(...).withResources(...).withPrompts(...)` then `.stdio.run` / `.http(config).resource` (or `.routes` for embedding) / `.webSocket(config).resource`; the same verbs work on any `Server[F]`. `mcp4s.server.syntax` (`serveHttp` / `serveWebSocket` / `runStdio`) is **removed**. Server capabilities are now **derived** from what is registered (tools-only servers advertise only tools; `resources.subscribe` only with a subscribable resource).
- **Client assembly** — `McpClientBuilder[F](info).withRoots(...).withSampling(...).withElicitation(...)` then `.stdio(config)` / `.http(config, httpClient)`; the JVM adds `.webSocket(...)` and auto-Ember `.http(uri)` via `import mcp4s.client.syntax.*`. `connectStdio` / `connectHttp` / `connectWebSocket` are **removed**.
- **Transport config unification** — configs take the full URI including path (`.../mcp`, `.../ws`); all three carry `timeouts: Timeouts(request = 5.minutes, init = 30.seconds)` (stdio included), and HTTP/WebSocket carry `auth: Option[McpAuth[F]]` (`Bearer` / `TokenProvider`) — `McpAuth` replaces `HttpAuth` and also authenticates the WebSocket upgrade.

### Added

- **Service algebra + typed client** — declare endpoints once in an `object MyService extends McpService(name, version)`: `ServiceRoutes(MyService)(handlers*)` fails fast at construction on missing/duplicate/undeclared handlers, and `import mcp4s.client.TypedClient.*` gives `conn.call(MyService.add)(AddArgs(1, 2)): F[AddResult]` (with `isError` raised as `McpError.ToolExecutionError`) and `conn.getPrompt(endpoint)(input)`.
- Prompt inputs now support `Int` / `Long` / `Double` / `Boolean` and Scala 3 enum fields (parsed from their string form), plus defaults — not just `String` / `Option[String]`.

### Fixed

- **Bidirectional flows now work over Streamable HTTP, not just WebSocket** — server-initiated sampling/elicitation requests ride the SSE response stream and the client answers them on every network transport (one shared `ConnectionRunner` drives stdio, HTTP, and WebSocket). Stdio remains plain request/response.

## [0.1.5] - 2026-04-05

### Removed
- Agent module
- Auth module (use standard http4s middleware for authentication)
- Server middleware (use standard error handling patterns)

### Changed
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
- CircuitBreaker, RetryPolicy, ResilientConnection builder
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
