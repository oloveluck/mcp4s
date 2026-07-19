// GENERATED from server/README.md — do not edit; regenerate with snippet_harness.py
package docsnippets.server_README

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
  // ---- snippet at line 18
  import cats.effect.IO
  import mcp4s.server.*
  import mcp4s.server.dsl.{Resource, *}
  import mcp4s.protocol.ServerInfo

  case class EchoArgs(text: String) derives Schema

  val myTools = Tool("echo").withDescription("Echo back").input[EchoArgs]
    .handle[IO](args => IO.pure(ok(args.text)))
  val myResources = Resource.text[IO]("file:///readme", "README")("Hello")
  val myPrompts   = Prompt("help").withDescription("Get help").messages[IO](user("How can I help?"))

  val server = McpServer[IO](ServerInfo("my-server", "1.0.0"))
    .withTools(myTools)
    .withResources(myResources)
    .withPrompts(myPrompts)

  // ---- snippet at line 50
  val calculatorServer = McpServer[IO](ServerInfo("calc", "1.0.0")).withTools(myTools).toServer
  val utilityServer    = McpServer[IO](ServerInfo("util", "1.0.0")).withPrompts(myPrompts).toServer

  val combined = calculatorServer |+| utilityServer

  // ---- snippet at line 61
  val config = HttpConfig[IO](port = port"8080")

  server.stdio.run                          // Stdio for Claude Desktop
  server.http().resource.useForever         // HTTP on /mcp, port 3000
  server.http(config).resource              // HTTP on a custom port
  server.http(config).routes                // embed in an existing http4s app
  server.webSocket().resource.useForever    // WebSocket on /ws, port 3000

  // ---- snippet at line 77
  import mcp4s.server.dsl.*

  @description("Summarize text")
  case class SummarizeArgs(text: String) derives Schema

  // Tools — an endpoint definition plus exactly one handler
  Tool.from[SummarizeArgs]
    .handle[IO](args => IO.pure(ok("result")))                    // derived name + desc
  Tool("name").withDescription("desc").input[SummarizeArgs]
    .handle[IO](args => IO.pure(ok("result")))                    // explicit
  Tool("name").input[SummarizeArgs]
    .handleWith[IO]((args, ctx) => IO.pure(ok("result")))         // with context
  Tool("name").input[SummarizeArgs]
    .stream[IO](args => fs2.Stream(ok("chunk")))                  // streaming

  // Resources — data the AI can read
  Resource.text[IO]("uri", "name")("content")
  Resource.template[IO]("uri/{id}", "name", "desc")(uri => IO.pure(text(uri, "content")))

  // Prompts — reusable message templates
  Prompt("name").withDescription("desc").messages[IO](user("Hello"))
  Prompt.from[SummarizeArgs]
    .handle[IO](args => IO.pure(messages(user("Hello"))))         // derived name + desc

  // Results
  ok("success")
  error("failed")
  user("text")
  assistant("text")

