// GENERATED from README.md — do not edit; regenerate with snippet_harness.py
package docsnippets.README

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
  // ---- snippet at line 20
  import cats.effect.*
  import mcp4s.protocol.ServerInfo
  import mcp4s.server.*
  import mcp4s.server.dsl.*

  @description("Add two numbers")
  case class AddArgs(a: Double, b: Double) derives Schema

  val tools = Tool.from[AddArgs].handle[IO](args => IO.pure(ok(s"${args.a + args.b}")))

  val server = McpServer[IO](ServerInfo("calculator", "1.0.0")).withTools(tools)

