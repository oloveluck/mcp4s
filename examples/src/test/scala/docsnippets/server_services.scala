// GENERATED from server/services.md — do not edit; regenerate with snippet_harness.py
package docsnippets.server_services

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
  import mcp4s.schema.{McpService, Prompt, Schema, Tool}
  import mcp4s.protocol.description

  case class AddArgs(a: Double, b: Double) derives Schema
  case class AddResult(sum: Double) derives Schema

  @description("Greet someone by name")
  case class GreetArgs(name: String) derives Schema

  object Calculator extends McpService("calculator", "1.0.0"):
    val add   = Tool("add").withDescription("Add two numbers").input[AddArgs].output[AddResult]
    val greet = Tool.from[GreetArgs]   // name + description derived from the type

    def endpoints = List(add, greet)

  // ---- snippet at line 33
  import cats.effect.IO
  import mcp4s.server.*
  import mcp4s.server.dsl.*

  val routes: Tools[IO] = ServiceRoutes(Calculator)(
    Calculator.add.handle[IO](args => IO.pure(AddResult(args.a + args.b))),
    Calculator.greet.handle[IO](args => IO.pure(ok(s"Hello, ${args.name}!")))
  )

  val server = McpServer[IO](Calculator.info).withTools(routes)

  // ---- snippet at line 50
  ServiceRoutes(Calculator)(
    Calculator.add.handle[IO](args => IO.pure(AddResult(args.a + args.b)))
  )
  // IllegalArgumentException: Service 'calculator': no handler for endpoints: greet

  // ---- snippet at line 61
  import mcp4s.client.TypedClient.*

  conn.call(Calculator.add)(AddArgs(19, 23))      // : IO[AddResult]
  conn.call(Calculator.greet)(GreetArgs("Ada"))   // : IO[ToolResult]  (no .output declared)

  // ---- snippet at line 74
  val greeting = Prompt("greeting").input[GreetArgs]   // a PromptEndpoint, shareable like a val

  conn.getPrompt(greeting)(GreetArgs("Ada"))  // : IO[GetPromptResult]

  // ---- snippet at line 82
  import com.comcast.ip4s.port
  import mcp4s.client.McpClientBuilder
  import mcp4s.client.syntax.*
  import mcp4s.client.TypedClient.*
  import mcp4s.server.transport.HttpConfig

  McpServer[IO](Calculator.info)
    .withTools(routes)
    .http(HttpConfig(port = port"3000"))
    .resource
    .use: _ =>
      McpClientBuilder[IO](ClientInfo("svc-client", "1.0.0"))
        .http("http://localhost:3000/mcp")
        .use: conn =>
          for
            sum   <- conn.call(Calculator.add)(AddArgs(19, 23))
            greet <- conn.call(Calculator.greet)(GreetArgs("Ada"))
          yield (sum.sum, greet.textContent)  // (42.0, "Hello, Ada!")

