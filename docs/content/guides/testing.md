# Testing

Three levels, from fastest to most realistic.

## In process, no transport

`mcp4s.server.testing` drives a `Server[F]` or a `Tools[F]` directly. Use it for the logic of
individual tools.

```scala
import io.circe.Encoder
import mcp4s.server.*
import mcp4s.server.dsl.*
import mcp4s.server.testing.*
import mcp4s.server.testing.ToolsTest.*

case class AddArgs(a: Double, b: Double) derives Schema, Encoder.AsObject

val tools    = Tool("add").withDescription("Add").input[AddArgs].handle[IO](a => IO.pure(ok(s"${a.a + a.b}")))
val calcServer = Server.fromTools[IO](ServerInfo("test", "1.0.0"), tools)

val viaTools  = tools.testCall("add", AddArgs(2, 3))                         // IO[ToolResult]
val viaServer = ServerTest(calcServer).use(_.callTool("add", AddArgs(2, 3)))   // the whole server
```

`args("a" -> 2.0, "b" -> 3.0)` builds JSON arguments when you have no case class; `hasTool`,
`getTool` and `assertTool` inspect the registered definitions.

## Over a real transport

`StdioTransport.runWith` runs the stdio server over in-memory streams, and the HTTP and
WebSocket servers bind to port 0 for ephemeral ports. The `examples` module's integration specs
show the pattern; the testkit's `TestServers` and `DeterministicClients` provide servers with
configurable failures, call counting and jitter, and clients with canned sampling and
elicitation answers.

## Compliance and performance

`mcp4s-testkit` (JVM) publishes two [weaver](https://github.com/typelevel/weaver-test) suites
that run *your* server over live HTTP and WebSocket connections.

```scala
import io.circe.Json, io.circe.syntax.*
import mcp4s.testkit.*

object MyServer:
  def build: Server[IO] = ???

object MyServerComplianceSpec extends McpComplianceSuite:
  def serverUnderTest = MyServer.build
  def profile = ComplianceProfile(
    sampleTool       = Some(ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson), _.textContent == "5.0")),
    cancellationTool = Some(ToolProbe("slow_add", Json.obj("a" -> 1.asJson, "b" -> 2.asJson))),
    sampleResource   = Some(ResourceProbe("file:///readme")),
    samplePrompt     = Some(PromptProbe("greet", Map("name" -> "Ada")))
  )

object MyServerPerfSpec extends McpPerformanceSuite:
  def serverUnderTest = MyServer.build
  def perfProfile = PerfProfile(
    ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson)),
    calls = 5000, concurrency = 8,
    maxFailureRate = Some(0.0)          // leave throughput and latency SLOs off on shared CI
  )
```

The compliance suite reads the server's capabilities and checks only what it declares, so one
profile adapts to any server; the probes supply valid inputs. `McpBenchmark.run` returns the same
`PerfReport` (throughput, p50/p95/p99) without a test framework.

The official MCP conformance suite runs with `sbt conformance` against the example conformance
server; it is the authority on wire-level behaviour and runs in CI.
