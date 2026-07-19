// GENERATED from client/README.md — do not edit; regenerate with snippet_harness.py
package docsnippets.client_README

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
  // ---- snippet at line 14
  import cats.effect.*
  import mcp4s.client.McpClientBuilder
  import mcp4s.client.mcp.*
  import mcp4s.protocol.*

  def myLlm(params: CreateMessageParams): IO[CreateMessageResult] = ???   // your LLM integration
  def askUser(params: ElicitParams): IO[ElicitResult] = ???               // your UI integration

  val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))
    .withRoots(Roots[IO]("file:///workspace", "Workspace"))
    .withSampling(Sampling[IO](params => myLlm(params)))
    .withElicitation(Elicitation[IO](params => askUser(params)))

  // ---- snippet at line 39
  import mcp4s.client.syntax.*   // JVM-only verbs: webSocket, auto-Ember http
  import mcp4s.client.transport.*
  import io.circe.Json, io.circe.syntax.*

  val args = Json.obj("a" -> 1.asJson, "b" -> 2.asJson)

  // Stdio — spawn a subprocess
  client.stdio("node", "server.js").use(conn => conn.callTool("add", args))

  // HTTP — JVM one-liner (builds/manages an Ember client for you)
  client.http("http://localhost:3000/mcp").use(conn => conn.callTool("add", args))

  // HTTP — cross-platform: bring your own http4s Client[F]
  client.http(HttpTransportConfig[IO]("http://localhost:3000/mcp"), httpClient)
    .use(conn => conn.callTool("add", args))

  // WebSocket (JVM-only)
  client.webSocket("ws://localhost:3000/ws").use(conn => conn.callTool("add", args))

  // ---- snippet at line 71
  import mcp4s.client.transport.*
  import mcp4s.transport.Timeouts
  import scala.concurrent.duration.*

  val config = HttpTransportConfig[IO](
    uri = "https://api.example.com/mcp",
    auth = Some(McpAuth.Bearer("my-token")),
    timeouts = Timeouts(request = 2.minutes, init = 10.seconds)
  )

  // ---- snippet at line 93
  import mcp4s.client.TypedClient.*
  import mcp4s.schema.{Schema, Tool as ToolDef}

  // The endpoint definitions shared with the server (see Services)
  case class AddArgs(a: Double, b: Double) derives Schema
  case class AddResult(sum: Double) derives Schema

  object Calculator:
    val add = ToolDef("add").input[AddArgs].output[AddResult]

  conn.call(Calculator.add)(AddArgs(1, 2))   // : IO[AddResult]

