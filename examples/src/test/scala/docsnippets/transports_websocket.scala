// GENERATED from transports/websocket.md — do not edit; regenerate with snippet_harness.py
package docsnippets.transports_websocket

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
  // ---- snippet at line 8
  import com.comcast.ip4s.*
  import mcp4s.server.transport.*

  server.webSocket().resource.useForever          // defaults: port 3000, path /ws

  // Full configuration
  server.webSocket(WebSocketConfig(
    host = host"0.0.0.0",
    port = port"3000",
    path = "ws"
  )).resource.useForever

  // ---- snippet at line 36
  import mcp4s.client.syntax.*   // JVM-only

  val args = Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))

  client.webSocket("ws://localhost:3000/ws").use: conn =>
    conn.callTool("add", args)

  // Full configuration
  import mcp4s.client.transport.*
  import mcp4s.transport.Timeouts

  client.webSocket(WebSocketTransportConfig[IO](
    uri  = "wss://api.example.com/ws",
    auth = Some(McpAuth.Bearer("my-token"))   // sent on the upgrade request
  )).use: conn =>
    conn.callTool("add", args)

