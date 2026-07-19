// GENERATED from getting-started/first-client.md — do not edit; regenerate with snippet_harness.py
package docsnippets.getting_started_first_client

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
  import cats.effect.*
  import mcp4s.client.*
  import mcp4s.client.syntax.*
  import mcp4s.protocol.*
  import org.typelevel.otel4s.trace.Tracer

  given Tracer[IO] = Tracer.noop[IO]

  val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))

  // JVM one-liner: builds and manages an Ember client for you
  client.http("http://localhost:3000/mcp").use: conn =>
    IO.println(s"Connected to ${conn.serverInfo.name}")

  // ---- snippet at line 30
  import io.circe.Json, io.circe.syntax.*

  client.http("http://localhost:3000/mcp").use: conn =>
    for
      _         <- IO.println(s"Connected to: ${conn.serverInfo.name}")
      tools     <- conn.listAllTools                          // discover tools
      result    <- conn.callTool("add", Json.obj("a" -> 5.asJson, "b" -> 3.asJson))
      resources <- conn.listAllResources                      // discover resources
      content   <- conn.readResource("file:///readme")
      prompts   <- conn.listAllPrompts                        // discover prompts
      prompt    <- conn.getPrompt("greet", Map("name" -> "Alice"))
    yield ()

  // ---- snippet at line 51
  client.webSocket("ws://localhost:3000/ws").use: conn =>
    conn.callTool("add", Json.obj("a" -> 5.asJson, "b" -> 3.asJson))

  // ---- snippet at line 60
  import mcp4s.protocol.McpError

  conn.callTool("unknown", Json.obj()).attempt.flatMap:
    case Right(result)     => IO.println(s"Success: $result")
    case Left(e: McpError) => IO.println(s"MCP error: ${e.message}")
    case Left(e)           => IO.println(s"Error: ${e.getMessage}")

  // ---- snippet at line 73
  val args = Json.obj("a" -> 5.asJson, "b" -> 3.asJson)

  if conn.supportsTools then conn.callTool("add", args)
  else IO.println("Tools not supported")

  // Or use conditional methods that return Option
  conn.callToolIfSupported(ToolName("add"), args)  // Returns F[Option[ToolResult]]

