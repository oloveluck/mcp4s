# Changelog

## Unreleased

### Fixed

- **Flaky CI: HTTP progress notifications.** The Streamable HTTP server shared one outbound queue across every in-flight request on a session and juggled "foreign" progress events between SSE pollers with re-offers and 1 ms sleeps; under concurrency a notification could surface on the wrong stream and be dropped, failing `[http] progress notifications are delivered to the callback` on `main`. Every request answered with an SSE stream now owns its own queue: the messages a tool emits while handling a request (progress, logging, sampling, elicitation) go to that request's stream and nowhere else, as the spec requires. No polling, no re-offers.
- Server-initiated requests made from a request that has no SSE stream (JSON-mode POST without a GET stream) now fail immediately instead of waiting out the 5-minute request timeout.
- **Composed servers kept their tools' context.** `Server#combine` / `|+|` did not forward `callToolWithContext`, so a tool behind a composed server lost sampling, elicitation, progress and logging; `ComposedServer` now delegates with the context, left first.
- `SamplingRequester.supportsSampling` / `ElicitationRequester.supportsElicitation` now reflect the client's advertised capabilities instead of always returning `true`.
- `initialize` performs version negotiation: a supported requested version (`2025-11-25`, `2025-06-18`, `2025-03-26`, `2024-11-05`) is echoed back; anything else gets the latest. The client disconnects (`McpError.ProtocolVersionMismatch`) if the server picks a version it cannot speak.

### Added

- **Streamable HTTP, complete** (spec 2025-11-25): standalone `GET` SSE stream per session for unsolicited server messages (`409` for a second one, `406` without `Accept: text/event-stream`); `DELETE` terminates a session; `404` for unknown/expired session ids, `400` for a missing id or an unsupported `MCP-Protocol-Version`, `202 Accepted` (no body) for notifications and client responses, `403` for a foreign `Origin` as well as `Host` on localhost bindings (`HttpConfig.allowedOrigins`); SSE priming events with ids and periodic keep-alive comments (`HttpConfig.keepAliveInterval`); invalid JSON/JSON-RPC bodies are `400`. The stateless mode (`enableSessions = false`) is now the same code path with a single shared session that accepts `initialize` repeatedly.
- **Client:** sends the negotiated `MCP-Protocol-Version` on every request, opens the standalone `GET` stream after initialization (`HttpTransportConfig.listen`, reconnecting when it drops), tolerates priming events and comments, forgets the session on `404`, and `DELETE`s the session when the connection is released.
- **Tasks (SEP-1686, experimental in 2025-11-25).** `ToolEndpoint.withTaskSupport(TaskSupport.Optional | Required)` marks a tool task-capable and the server derives the `tasks` capability. The dispatcher runs task-augmented `tools/call`s in the background and implements `tasks/get`, `tasks/result`, `tasks/cancel`, `tasks/list` with TTL-based retention (`Dispatcher.Config.tasks`); `required` tools refuse plain calls and `forbidden` tools refuse task calls with `-32601`. Client: `callToolTask`, `getTask`, `taskResult`, `cancelTask`, `listTasks`, `supportsToolTasks`. New protocol types `Task`, `TaskStatus`, `TaskMetadata`, `CreateTaskResult`, `ListTasksResult`.
- **Resource subscriptions delivered.** `resources/subscribe` / `unsubscribe` are tracked per session and `Server.resourceChanges` (the merged change stream of subscribable resources) is forwarded as `notifications/resources/updated` to subscribed clients over HTTP (GET stream), WebSocket and stdio. Client: `conn.subscribeResource` / `unsubscribeResource`.
- **Server notification handler on the client.** `McpClientBuilder.withNotifications(Notifications[F] { ... })` receives a typed `ServerNotification` (`ResourceUpdated`, `ResourcesListChanged`, `ToolsListChanged`, `PromptsListChanged`, `Log`, `TaskStatusChanged`, `Other`); `McpClient.onNotification` is the underlying hook.
- **Logging level.** `logging/setLevel` is honoured: `ctx.log` messages below the session's level are not sent. Client: `conn.setLogLevel`.
- **Completions.** `completion/complete` is implemented: prompts built from a typed input complete enum (and boolean) arguments automatically (`Prompts.complete`, `Server.complete`, `PromptCodec.candidates`); client: `conn.complete`. New types `CompleteParams`, `CompletionRef`, `CompletionArgument`, `Completion`, `CompleteResult`.
- **Stdio is bidirectional.** The stdio server runs on the same session engine as HTTP and WebSocket: tools get a full `ToolContext` (progress, logging, sampling, elicitation) and client responses are correlated back. `StdioTransport.runWith(server, input, output)` runs the protocol over arbitrary byte streams (sockets, in-memory pipes in tests); `StdioConfig` sets the request timeout and concurrency.
- `instructions` at initialize: `McpServer.withInstructions(...)` / `Server.from(..., instructions = ...)`; `conn.instructions` and `conn.protocolVersion` on the client. `ToolEndpoint.withTitle`.
- `McpVersion.Supported`, `McpVersion.negotiate`, `Tool.taskSupport`, `ServerTasksCapability.toolCalls`.
- **JSON Schema 2020-12 keywords (SEP-1613).** `JsonSchema` gains `$defs`, `JsonSchemaProperty` gains `$ref` (`JsonSchema.ref("name")`), and `additionalProperties` is now raw `Json` on both so `additionalProperties: false` can be expressed; helpers `.closed`, `.withDialect` (`JsonSchema.Dialect2020_12`) and `.withDefs(...)`. The conformance server's `json_schema_2020_12_tool` now passes the upstream keyword-preservation scenario.

### Changed

- `Dispatcher` is now explicitly the per-session protocol state machine: it exposes `protocolVersion`, `clientCapabilities`, `logLevel`, `subscriptions` and `close`, and takes a `Dispatcher.Config` (re-initialization policy for stateless HTTP, task settings). `Dispatcher.withContext`'s factory is effectful (`(RequestId, Option[RequestId]) => F[ToolContext[F]]`) and `Dispatcher` requires `Temporal` rather than `Concurrent`.
- `McpChannel` gains `negotiated(protocolVersion)`, called by the connection runner after the handshake.
- `McpConnection.progressHandlers` is no longer part of the public trait (the runner owns the registry); `McpConnection.apply` takes the `InitializeResult`.
- `HttpSession` no longer exposes `outQueue` / `activeProgressTokens` / `queueSize`; it owns per-request streams and the standalone stream (`hasStandaloneStream`).
- **Removed redundant API surfaces** (pre-1.0 cleanup): `McpResult`, `PromptResult` and the `String#asResource` / `asToolResult` extensions (use `ToolResult.text`, `ResourceContent.text` and the `mcp4s.server.dsl` builders `ok`/`messages`/`user`/`assistant`); `McpLifecycle`, `Server#asResource` and `ServerLifecycle` (a `Resource[F, Tools[F]]` lifts with `.map(Server.fromTools(info, _))`; `McpLifecycleTool` stays); and `ResourceSubscriptionManager`, superseded by the per-session subscription delivery built into every transport.
- `JsonSchema.additionalProperties` / `JsonSchemaProperty.additionalProperties` are `Option[Json]" instead of `Option[JsonSchemaProperty]` (JSON Schema allows a boolean there too); derived `Map[String, V]` schemas render the same JSON as before.

### Build

- **Dependency updates** — Scala 3.8.4 → 3.9.0, cats-effect 3.7.0 → 3.7.1, fs2 3.13.0 → 3.14.0, circe 0.14.15 → 0.14.16, otel4s 1.0.1 → 1.1.0, munit-scalacheck 1.3.0 → 1.3.1, weaver 0.10.1 → 0.13.0; sbt-typelevel 0.8.6 → 0.8.7, sbt-jmh 0.4.7 → 0.4.8. All three platforms (JVM/JS/Native) compile and the full test + MCP conformance suites pass.
- **http4s held at 0.23.34.** 0.23.35 regressed ember's parallel request throughput (concurrent calls serialize — the parallel-vs-sequential perf test flips), so http4s is pinned to 0.23.34 in both `build.sbt` and `.scala-steward.conf` until a fixed 0.23.x ships. (circe and http4s also remain on their pre-1.0/0.15 stable lines, whose milestone successors have no Scala Native build.)
- **CI now builds and tests on JDK 25** (temurin 25, up from 17) across the main matrix and the conformance / validate-steward / deploy-site jobs. `tlJdkRelease` stays at **17**, so the published artifacts remain runnable on **JVM 17+** — only the build toolchain moved.
- **CI / conformance** — MCP conformance suite bumped to v0.1.16 (server passes all 40 scenarios); generated-workflow actions `setup-node` → v7 and `coursier/setup-action` → v3. Dropped the `github-actions` ecosystem from Dependabot: the workflows are sbt-typelevel–generated and verified by `githubWorkflowCheck`, so Dependabot's YAML edits could never merge — action versions are managed via `build.sbt` and the sbt-typelevel plugin instead.

## 0.3.0 - 2026-07-21

### Changed (breaking) — smithy4s-style redesign

- **Unified `Schema` typeclass** (`mcp4s.schema.Schema`, exported by `mcp4s.server.dsl`) — a single `derives Schema` replaces `derives ToolInput`, `derives PromptInput`, and `derives ToolOutput, Encoder.AsObject`. The JSON Schema, circe encoder/decoder, and prompt-argument metadata are all interpreters over the same value, so they can never disagree. Supports nested case classes (full nested schemas), Scala 3 enums (string enums), sealed traits with payloads (`oneOf` + `"type"` discriminator), `Map[String, V]` (`additionalProperties`), `Option` and constructor defaults (not required; defaults rendered in the schema and applied on decode), `@description` on classes and fields, and recursive types via `Schema.defer`.
- **Endpoint DSL** — `import mcp4s.server.dsl.*` replaces `import mcp4s.server.mcp.*`. The 16-overload `Tool` object is gone: build an endpoint fluently (`Tool("name").withDescription(...).input[Args].output[Out].withAnnotations(...)`), then attach exactly one of four handlers: `.handle[F]`, `.handleWith[F]` (with `ToolContext`), `.stream[F]`, `.streamWith[F]`. `Tool.from[Args]` / `Prompt.from[Args]` derive name (snake_case, `Args`/`Input`/`Params`/`Request` stripped) and description from the input type. Prompts gain `.messages`, `.static`, and `.input[Args].handle`. Result/message builders (`ok`/`error`/`content`/`messages`/`user`/`assistant`/…) and the `Resource` constructors are unchanged.
- **Typed tool output** — `.output[B]` advertises `outputSchema` and encodes handler results as `structuredContent`; primitive outputs wrap as `{"result": ...}` on the wire.
- **Server assembly** — `McpServer[F](info).withTools(...).withResources(...).withPrompts(...)` then bind a transport: `.stdio.run`, `.http(config).resource` (or `.routes` for embedding in an existing http4s app), `.webSocket(config).resource`. The same verbs work on any `Server[F]` with no import. `import mcp4s.server.syntax.*` and `serveHttp` / `serveWebSocket` / `runStdio` are **removed**. `Server.from` remains as the low-level constructor.
- **Derived capabilities** — server capabilities now reflect what is actually registered: a tools-only server advertises only tools (plus logging/completions), and `resources.subscribe` is `true` only when a subscribable resource is registered. Client capabilities likewise derive from which handlers are present.
- **Client assembly** — `McpClientBuilder[F](info).withRoots(...).withSampling(...).withElicitation(...)` then `.stdio(config)` / `.http(config, httpClient)`; the JVM adds `.webSocket(...)` and an auto-Ember `.http(uri)` via `import mcp4s.client.syntax.*`. `connectStdio` / `connectHttp` / `connectWebSocket` are **removed**. `McpClient.from` remains.
- **Transport config unification** — configs take the **full URI including path** (`.../mcp`, `.../ws`); the separate baseUrl/endpoint and url/path fields are gone. All three client configs carry `timeouts: Timeouts(request = 5.minutes, init = 30.seconds)` (`mcp4s.transport.Timeouts`), so stdio now has request/init timeouts too. HTTP and WebSocket carry `auth: Option[McpAuth[F]]` — `McpAuth` (`Bearer` / `TokenProvider`) replaces `HttpAuth` and now also authenticates the WebSocket upgrade request.
- **`SessionId` opaque type** — `SessionManager.get`/`remove` and `HttpSession.id` use `mcp4s.server.transport.SessionId` (an opaque wrapper over the `Mcp-Session-Id` header value) instead of raw `String`; construct with `SessionId(...)`, unwrap with `.value`.
- **`Server#withInfo` now preserves context-aware tool dispatch** — it previously routed `callToolWithContext` through the plain `callTool`, so a wrapped server silently lost sampling/elicitation context on tool calls.

### Added

- **Service algebra + typed client** — define endpoints once in an `object MyService extends McpService(name, version)` and share them: on the server, `ServiceRoutes(MyService)(handlers*)` verifies at construction time that handlers cover the declared endpoints exactly (missing/duplicate/undeclared handlers throw `IllegalArgumentException`); on the client, `import mcp4s.client.TypedClient.*` gives `conn.call(MyService.add)(AddArgs(1, 2)): F[AddResult]` — typed both ways, with `isError` results raised as `McpError.ToolExecutionError` — and `conn.getPrompt(endpoint)(input)` for prompts.
- Prompt inputs now support `Int` / `Long` / `Double` / `Boolean` and Scala 3 enum fields (parsed from their string form), plus defaults — not just `String` / `Option[String]`.
- **`mcp4s-testkit`** — a new cross-platform (JVM/JS/Native), published module with reusable test fixtures (`TestServers` configurable/chaotic/counting servers, `DeterministicClients`) for testing MCP servers and clients. Extracted from the examples' internal fixtures so downstream users can depend on it too.
- **`benchmarks`** module (JVM-only, not published) — JMH microbenchmarks for the in-memory request hot path (`DispatcherBench`: dispatch, decode, encode), tool lookup vs N tools (`ToolLookupBench`), and the resource-template regex hotspot (`ResourceTemplateBench`); plus an end-to-end throughput/latency driver (`ThroughputDriver`, a thin CLI over the testkit's `McpBenchmark`). A committed `benchmarks/results/baseline.json` and a documented **allocations-per-op** (`gc.alloc.rate.norm`) comparison workflow make this a reliable cross-version regression measure. See `benchmarks/README.md`.
- **Compliance + performance harness in `mcp4s-testkit`** (JVM, built on [weaver](https://github.com/typelevel/weaver-test)) — point it at any `Server[IO]` to get a profile:
  - `McpComplianceSuite` — a capability-parameterized suite (via `ComplianceProfile` + `ToolProbe`/`ResourceProbe`/`PromptProbe`) that checks protocol correctness over live HTTP **and** WebSocket, skipping checks for capabilities a server doesn't declare
  - `McpBenchmark.run` (returns a `PerfReport`) and `McpPerformanceSuite` — a concurrent load run that reports throughput/latency (HdrHistogram) and asserts `PerfProfile` SLOs (failure rate / throughput / p99); a one-liner against any `Server[IO]`
  - The weaver code is **JVM-only** (`testkit/.jvm`); the cross-platform fixtures are unchanged. `weaver-cats` becomes a JVM compile-scope dependency of the published `mcp4s-testkit`. See the Testing guide in the docs (`docs/content/testing/README.md`).

### Fixed

- **Bidirectional flows now work over Streamable HTTP, not just WebSocket** — server-initiated sampling/elicitation requests ride the SSE response stream and the client answers them on every network transport (a single shared `ConnectionRunner` drives stdio, HTTP, and WebSocket). Stdio remains plain request/response.
- **WebSocket client no longer stalls under sustained load.** Reimplemented the JVM WebSocket client transport on http4s `JdkWSClient` (high-level `WSConnectionHighLevel`) instead of sttp's `HttpClientFs2Backend`. A 4,000-call / concurrency-8 run that previously hung indefinitely now completes with 0 failures at ~4,100 calls/sec (p50 1.5 ms, p99 7.6 ms) — faster than HTTP. The `sttp-client4` dependency is dropped; WebSocket remains JVM-only.

### Performance

- **List responses are no longer re-encoded on every call.** The dispatcher caches the encoded `tools/list` / `resources/list` / `resources/templates/list` / `prompts/list` JSON, keyed by the source list (re-encoded only when it changes, e.g. on `list_changed`). Measured **−80%** allocations on `tools/list` (28.9 KB → 5.7 KB per call).
- **URI-template resource matching compiles its regex once** per template instead of on every `resources/read` — **−28%** allocations on a templated read (10.8 KB → 7.8 KB). Guarded by `benchmarks/results/baseline.json`.

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
