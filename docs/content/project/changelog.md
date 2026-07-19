# Changelog

## Unreleased

### Docs
- Refreshed all guide examples to modern braceless Scala 3 syntax and the current fluent APIs (`server.serveHttp`/`client.connectHttp`, `listAll*`, `.asJson`), and fixed several stale snippets (e.g. `serveHttp()` needs `.useForever`, `McpError` is an `enum`, the non-existent `conn.callToolStreaming`).

### Performance
- **List responses are no longer re-encoded on every call.** The dispatcher caches the encoded `tools/list` / `resources/list` / `resources/templates/list` / `prompts/list` JSON, keyed by the source list (re-encoded only when it changes, e.g. on `list_changed`). Measured **−80%** allocations on `tools/list` (28.9 KB → 5.7 KB per call).
- **URI-template resource matching compiles its regex once** per template instead of on every `resources/read` — **−28%** allocations on a templated read (10.8 KB → 7.8 KB). Guarded by `benchmarks/results/baseline.json`.

### Fixed
- **WebSocket client no longer stalls under sustained load.** Reimplemented the JVM WebSocket client transport on http4s `JdkWSClient` (high-level `WSConnectionHighLevel`) instead of sttp's `HttpClientFs2Backend`. A 4,000-call / concurrency-8 run that previously hung indefinitely now completes with 0 failures at ~4,100 calls/sec (p50 1.5 ms, p99 7.6 ms) — faster than HTTP. The `sttp-client4` dependency is dropped; WebSocket remains JVM-only.

### Changed (breaking)
- Renamed the streaming tool constructors to align with `fs2.Stream` and the `withContext` modifier: `Tool.streaming` → `Tool.stream` and `Tool.streamingWithContext` → `Tool.streamWithContext` (no deprecated aliases)

### Added
- Exposed `Tool.streamWithContext` in the public DSL (typed and no-argument overloads); previously a streaming + context tool could only be built via the internal `McpTool`
- Symmetric, fluent transport selection — pick a transport with one extension method on the value you already hold:
  - **Server** (`import mcp4s.server.syntax.*`): added `server.serveWebSocket(...)` (previously only `WebSocketTransport.serve` was available) plus bare-`Port` convenience overloads `serveHttp(port)` / `serveWebSocket(port)`
  - **Client** (new `import mcp4s.client.syntax.*`): `client.connectStdio(...)` / `connectHttp(...)` / `connectWebSocket(...)`, each returning `Resource[F, McpConnection[F]]`, with `command`/`args` and bare-URL convenience overloads. On the JVM, `connectHttp(url)` builds and manages an Ember client for you, and `connectWebSocket` is available (JVM-only, as before)
  - Purely additive — the `*Transport` objects remain for custom http4s routes/middleware and non-Ember client backends
- **`mcp4s-testkit`** — a new cross-platform (JVM/JS/Native), published module with reusable test fixtures (`TestServers` configurable/chaotic/counting servers, `DeterministicClients`) for testing MCP servers and clients. Extracted from the examples' internal fixtures so downstream users can depend on it too.
- **`benchmarks`** module (JVM-only, not published) — JMH microbenchmarks for the in-memory request hot path (`DispatcherBench`: dispatch, decode, encode), tool lookup vs N tools (`ToolLookupBench`), and the resource-template regex hotspot (`ResourceTemplateBench`); plus an end-to-end throughput/latency driver (`ThroughputDriver`, HdrHistogram). A committed `benchmarks/results/baseline.json` and a documented **allocations-per-op** (`gc.alloc.rate.norm`) comparison workflow make this a reliable cross-version regression measure. See `BENCHMARKS.md`.
- **Compliance + performance harness in `mcp4s-testkit`** (JVM, built on [weaver](https://github.com/typelevel/weaver-test)) — point it at any `Server[IO]` to get a profile:
  - `McpComplianceSuite` — a capability-parameterized suite (via `ComplianceProfile` + `ToolProbe`/`ResourceProbe`/`PromptProbe`) that checks protocol correctness over live HTTP **and** WebSocket, skipping checks for capabilities a server doesn't declare
  - `McpBenchmark.run` (returns a `PerfReport`) and `McpPerformanceSuite` — a concurrent load run that reports throughput/latency (HdrHistogram) and asserts `PerfProfile` SLOs (failure rate / throughput / p99); a one-liner against any `Server[IO]`
  - The weaver code is **JVM-only** (`testkit/.jvm`); the cross-platform fixtures are unchanged. `weaver-cats` becomes a JVM compile-scope dependency of the published `mcp4s-testkit`. See the [Testing guide](../testing/README.md).

## 0.2.0 - 2026-06-20

### Build
- Migrated from Mill to **sbt + sbt-typelevel**; now cross-built for **JVM, Scala.js (Node), and Scala Native**
- Upgraded to Scala **3.8.4** and the latest cats-effect 3.7 / fs2 3.13 / otel4s 1.0 / http4s 0.23.34 stack

### Changed (breaking)
- Removed `Server.builder` and `McpClient.builder`; the composable DSL (`Server.from` / `McpClient.from` + `import mcp4s.{server,client}.mcp.*`) is now canonical
- Removed the `<+>` operator in favor of `|+|` (cats `Semigroup`)
- `McpError` is now an `enum`; no-argument cases (`NotInitialized`, etc.) are singletons (drop the `()`)
- `runStdio` / `serveHttp` now default `Tracer` to noop (removed the `*NoTrace` variants); `serveHttp` takes an `HttpConfig`

### Improved
- Stricter lint (`-Wnonunit-statement`, `-Wshadow:all`, `-Wimplausible-patterns`) and full scalafmt + license-header enforcement
- Extracted a shared `RequestCorrelator` for request/response correlation across all duplex transports
- The stdio client uses a bounded queue with resource-managed reader fibers; reduced Codecs boilerplate

## 0.1.8 - 2026-05-10

### Improved
- Centralized JSON-RPC error code constants (`JsonRpcErrorCode.RequestCancelled`, `JsonRpcErrorCode.ServerError`)
- Fixed O(n²) list accumulation in `drainQueue` and client `paginate`
- Extracted shared HTTP header builder in `HttpClientTransport`
- Deduplicated initialization request construction across all client transports
- Pre-compiled URI template regexes in `BuiltServer` (avoids per-call recompilation)
- Extracted shared session infrastructure (`SessionSupport`) between HTTP and WebSocket transports
- Simplified `ServerBuilder` composition with `combineOpt` helper
- Deduplicated response parsing in HTTP client transport
- Lazy allocation in `BuiltServer.listTools`/`listResources`/`listPrompts`

### Fixed
- Removed non-existent API documentation (Tasks, `readResourceStreaming`)
- Fixed WebSocket config documentation to show correct type-safe types
- Clarified `Tracer` is optional (use `Tracer.noop` to disable)

## 0.1.7 - 2026-04-11

### Fixed
- `ToolOutput` schema derivation: optional fields now emit correct JSON Schema instead of wrapping in an extra `Option` layer
- `Dispatcher` initialization: server capabilities are now sent in `initialize` response before any tool/resource requests
- `PromptInput` error messages: validation failures now include the prompt name and argument details
- Landing page "Get Started" button rendering (was unstyled text link)

### Added
- WebSocket client: configurable connect/request timeouts and bounded message queues
- WebSocket server: graceful disconnect cleanup on client departure
- `ResourceSubscription`: bounded update queue to prevent unbounded memory growth
- `SessionManager`: configurable maximum session limit
- Client pagination support for `listTools`, `listResources`, and `listPrompts`

## 0.1.6 - 2026-04-05

### Added
- Fluent builder methods on `ResilienceConfig` (`withRetry`, `withTimeout`, `withoutTimeout`)
- WebSocket transport resilience integration tests
- Resilient client example (`ResilientClient`)
- Docs site badge in README

### Fixed
- Stale docstrings in `RetryPolicy` referencing removed `connection.withRetry()` API

## 0.1.5 - 2026-04-05

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

## 0.1.4

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

## 0.1.3

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

## 0.1.2 - 2025-01-29

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

## 0.1.1

### Added
- Conformance testing infrastructure
- Property-based tests for protocol types
- HTTP session management for stateful transport

## 0.1.0 - Initial Release

- MCP protocol implementation for Scala 3
- Server and client modules
- HTTP and stdio transports
- PostgreSQL server module
