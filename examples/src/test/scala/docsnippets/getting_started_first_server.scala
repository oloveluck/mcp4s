// GENERATED from getting-started/first-server.md — do not edit; regenerate with snippet_harness.py
package docsnippets.getting_started_first_server

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
  // ---- snippet at line 10
  import cats.effect.IO
  import mcp4s.server.dsl.*

  @description("Add two numbers")
  case class AddArgs(a: Double, b: Double) derives Schema

  @description("Multiply two numbers")
  case class MultiplyArgs(a: Double, b: Double) derives Schema

  val tools =
    Tool.from[AddArgs].handle[IO](args => IO.pure(ok(s"${args.a + args.b}"))) |+|
      Tool.from[MultiplyArgs].handle[IO](args => IO.pure(ok(s"${args.a * args.b}")))

  // ---- snippet at line 27
  Tool("add").withDescription("Add two numbers").input[AddArgs].handle[IO] { args =>
    IO.pure(ok(s"${args.a + args.b}"))
  }

  // ---- snippet at line 37
  import mcp4s.server.dsl.Resource   // the DSL's resource constructors, not cats.effect.Resource

  val resources =
    Resource.text[IO]("file:///readme", "README")("Calculator Server v1.0") |+|
      Resource.template[IO]("api://users/{id}", "User", "Get user by ID")(uri =>
        IO.pure(text(uri, s"""{"id":"${uri.split("/").last}"}"""))
      )

  // ---- snippet at line 51
  case class GreetArgs(name: String) derives Schema

  val prompts =
    Prompt("help").withDescription("Get help").messages[IO](user("How do I use this?")) |+|
      Prompt("greet").withDescription("Greet someone").input[GreetArgs].handle[IO](args =>
        IO.pure(messages(user(s"Hello, ${args.name}!")))
      )

  // ---- snippet at line 63
  import mcp4s.protocol.ServerInfo
  import mcp4s.server.*

  val server = McpServer[IO](ServerInfo("calculator", "1.0.0"))
    .withTools(tools)
    .withResources(resources)
    .withPrompts(prompts)

  // HTTP (production) — defaults to port 3000, path /mcp
  server.http().resource.useForever

  // HTTP on a custom port
  import mcp4s.server.transport.HttpConfig
  import com.comcast.ip4s.*
  server.http(HttpConfig(port = port"8080")).resource.useForever

  // Stdio (Claude Desktop)
  server.stdio.run

  // WebSocket — defaults to port 3000, path /ws
  server.webSocket().resource.useForever

