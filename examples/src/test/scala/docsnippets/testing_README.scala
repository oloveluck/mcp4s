// GENERATED from testing/README.md — do not edit; regenerate with snippet_harness.py
package docsnippets.testing_README

import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import io.circe.Json
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.server.{McpServer, Prompts, Resources, Server, ServiceRoutes, ToolContext, Tools}
import mcp4s.server.transport.{HttpConfig, SessionConfig, WebSocketConfig}
import mcp4s.client.{McpClient, McpClientBuilder, McpConnection}
import mcp4s.client.transport.{HttpTransportConfig, McpAuth, StdioTransportConfig, WebSocketTransportConfig}
import mcp4s.transport.Timeouts

object stubs:
  def conn: McpConnection[IO]                  = ???
  def connection: McpConnection[IO]            = ???
  def httpClient: org.http4s.client.Client[IO] = ???
  def server: Server[IO]                       = ???
  def client: McpClient[IO]                    = ???

object scope_1:
  import stubs.{*, given}
  // ---- snippet at line 29
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

  // ---- snippet at line 68
  import cats.effect.{IO, IOApp}
  import io.circe.Json, io.circe.syntax.*
  import mcp4s.testkit.*

  object Bench extends IOApp.Simple:
    def run =
      McpBenchmark
        .run(MyServer.build[IO], PerfProfile(ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson))))
        .flatMap(r => IO.println(r.render))

  // ---- snippet at line 86
  object MyServerPerfSpec extends McpPerformanceSuite:
    def serverUnderTest = MyServer.build[IO]
    def perfProfile = PerfProfile(
      toolProbe      = ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson)),
      calls = 5000, concurrency = 8,
      maxFailureRate = Some(0.0),          // SLO: no failed/stalled calls
      minThroughput  = None,               // leave perf SLOs off on shared CI to avoid flakiness
      maxP99         = None
    )

