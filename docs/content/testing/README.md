# Testing Your MCP Server

`mcp4s-testkit` is a published, reusable toolkit for testing MCP servers and clients — both
mcp4s itself and any server you build with the library. It has three layers:

| Layer | What it gives you | Platform |
|-------|-------------------|----------|
| **Fixtures** | `TestServers` (configurable / counting / chaotic / jittered servers) and `DeterministicClients` (canned sampling/elicitation/roots) | JVM / JS / Native |
| **Compliance** | `McpComplianceSuite` — a capability-parameterized [weaver](https://github.com/typelevel/weaver-test) suite that checks protocol correctness over live HTTP + WebSocket | JVM |
| **Performance** | `McpBenchmark.run` (returns a `PerfReport`), and `McpPerformanceSuite` — a weaver suite that drives load and asserts SLOs | JVM |

<!-- doc-snippet: skip -->
```scala
libraryDependencies += "io.github.oloveluck" %% "mcp4s-testkit" % "<version>" % Test
// weaver test framework (transitively available via testkit on the JVM):
testFrameworks += new TestFramework("weaver.framework.CatsEffect")
```

## Compliance Profile

Extend `McpComplianceSuite`, point it at your `Server[IO]`, and describe how to exercise it with
a `ComplianceProfile`. The suite reads your server's declared `ServerCapabilities` to decide
*which* groups (tools / resources / prompts) to check; the profile supplies the concrete valid
inputs (the "toolkit"). Checks for absent capabilities or unset probes are reported as **ignored**,
so the same suite adapts to whatever your server supports. Every applicable check runs over both
HTTP and WebSocket.

```scala
import cats.effect.IO
import io.circe.Json
import io.circe.syntax.*
import mcp4s.testkit.*

object MyServer:
  def build[F[_]]: mcp4s.server.Server[F] = ???   // your server under test

object MyServerComplianceSpec extends McpComplianceSuite:
  def serverUnderTest: mcp4s.server.Server[IO] = MyServer.build[IO]

  def profile = ComplianceProfile(
    sampleTool       = Some(ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson),
                              _.textContent == "5.0")),
    cancellationTool = Some(ToolProbe("slow_add", Json.obj("a" -> 1.asJson, "b" -> 2.asJson))),
    sampleResource   = Some(ResourceProbe("file:///readme", _.text.exists(_.nonEmpty))),
    samplePrompt     = Some(PromptProbe("greet", Map("name" -> "Ada")))
  )
```

What it checks (per transport): server identity/capabilities, `ping`, capability-flag
consistency, `tools/list` + `tools/call` (valid, unknown-tool error, concurrent), optional
progress callbacks, `resources/list` + `read` (valid, unknown-uri error) + templates,
`prompts/list` + `get` (valid, unknown-prompt error), auto-pagination, and connection survival
after a cancelled call.

> Override suite members with `def` (not `val`) — `transports` is read during construction.
> The suite covers **post-initialize** behavior; pre-initialize state-gating and wire-level
> SSE/DNS-rebinding remain the domain of mcp4s's unit tests and the official
> [`conformance`](https://github.com/modelcontextprotocol/conformance) suite (`sbt conformance`).

## Performance

Everything is under one import: `import mcp4s.testkit.*`.

**Quickest — get numbers ad-hoc.** `McpBenchmark.run` starts your server, drives a concurrent
load, and returns a `PerfReport`:

```scala
import cats.effect.{IO, IOApp}
import io.circe.Json, io.circe.syntax.*
import mcp4s.testkit.*

object Bench extends IOApp.Simple:
  def run =
    McpBenchmark
      .run(MyServer.build[IO], PerfProfile(ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson))))
      .flatMap(r => IO.println(r.render))
```

Run it with `sbt "runMain Bench"` and you'll see throughput plus p50/p95/p99 latency. Pass
`transport = McpTransport.WebSocket` as a third argument to benchmark WebSocket instead of HTTP.

**As a CI gate — assert SLOs.** Extend `McpPerformanceSuite` with a `PerfProfile`; the same load
run asserts the SLOs you set (the report is always printed):

```scala
object MyServerPerfSpec extends McpPerformanceSuite:
  def serverUnderTest = MyServer.build[IO]
  def perfProfile = PerfProfile(
    toolProbe      = ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson)),
    calls = 5000, concurrency = 8,
    maxFailureRate = Some(0.0),          // SLO: no failed/stalled calls
    minThroughput  = None,               // leave perf SLOs off on shared CI to avoid flakiness
    maxP99         = None
  )
```

Each call is bounded by `callTimeout`, so a stalled connection is counted as a failure rather
than hanging the run — making this a resilience probe as well as a throughput meter. For
lower-level, allocation-aware microbenchmarks of the server core, see the JMH benchmarks in
`BENCHMARKS.md`.
