// GENERATED from getting-started/README.md — do not edit; regenerate with snippet_harness.py
package docsnippets.getting_started_README

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
  // ---- snippet at line 27
  import cats.effect.*
  import mcp4s.protocol.ServerInfo
  import mcp4s.server.*
  import mcp4s.server.dsl.*

  @description("Add two numbers")
  case class AddArgs(a: Double, b: Double) derives Schema

  object MyServer extends IOApp.Simple:
    val add = Tool.from[AddArgs].handle[IO](args => IO.pure(ok(s"${args.a + args.b}")))

    val server = McpServer[IO](ServerInfo("calculator", "1.0.0")).withTools(add)

    def run = server.http().run

  // ---- snippet at line 52
  import cats.effect.*
  import io.circe.Json, io.circe.syntax.*
  import mcp4s.client.*
  import mcp4s.client.syntax.*
  import mcp4s.protocol.*
  import org.typelevel.otel4s.trace.Tracer

  object MyClient extends IOApp.Simple:
    given Tracer[IO] = Tracer.noop[IO]

    val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))

    def run = client.http("http://localhost:3000/mcp").use: conn =>
      conn
        .callTool("add", Json.obj("a" -> 5.asJson, "b" -> 3.asJson))
        .flatMap(r => IO.println(s"Result: $r"))

  // ---- snippet at line 76
  case class Args(query: String, limit: Option[Int]) derives Schema

  // ---- snippet at line 81
  val addTool, multiplyTool, divideTool = Tools.empty[IO]   // stand-ins for your Tool definitions
  val tools = addTool |+| multiplyTool |+| divideTool

  // ---- snippet at line 87
  client.http("http://localhost:3000/mcp").use: conn =>
    conn.callTool("add", Json.obj("a" -> 5.asJson, "b" -> 3.asJson))

