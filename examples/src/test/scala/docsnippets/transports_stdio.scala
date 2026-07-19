// GENERATED from transports/stdio.md — do not edit; regenerate with snippet_harness.py
package docsnippets.transports_stdio

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
  // ---- snippet at line 12
  import cats.effect.*
  import mcp4s.protocol.ServerInfo
  import mcp4s.server.*
  import mcp4s.server.dsl.*

  case class Args(query: String) derives Schema

  object MyServer extends IOApp.Simple:
    val tools = Tool("search").withDescription("Search files").input[Args]
      .handle[IO](args => IO.pure(ok(s"Results for: ${args.query}")))

    val server = McpServer[IO](ServerInfo("my-server", "1.0.0")).withTools(tools)

    def run = server.stdio.run

  // ---- snippet at line 35
  import io.circe.Json
  import mcp4s.client.McpClientBuilder
  import mcp4s.protocol.ClientInfo

  val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))

  client.stdio("java", "-jar", "/path/to/server.jar").use: conn =>
    conn.callTool("search", Json.obj("query" -> Json.fromString("readme")))

  // ---- snippet at line 48
  import mcp4s.client.transport.StdioTransportConfig
  import mcp4s.transport.Timeouts
  import scala.concurrent.duration.*

  client.stdio(StdioTransportConfig(
    command          = "node",
    args             = List("server.js"),
    workingDirectory = Some("/srv/mcp"),
    env              = Map("LOG_LEVEL" -> "debug"),
    timeouts         = Timeouts(request = 1.minute, init = 15.seconds)
  )).use(conn => conn.listAllTools)

