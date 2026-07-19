// GENERATED from reference/testing.md — do not edit; regenerate with snippet_harness.py
package docsnippets.reference_testing

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
  import cats.effect.IO
  import io.circe.Encoder
  import munit.CatsEffectSuite
  import mcp4s.server.*
  import mcp4s.server.dsl.*
  import mcp4s.server.testing.*

  class MyServerSuite extends CatsEffectSuite:

    // Schema drives the tool; Encoder.AsObject lets tests pass typed arguments
    case class AddArgs(a: Double, b: Double) derives Schema, Encoder.AsObject

    val tools = Tool("add").withDescription("Add").input[AddArgs].handle[IO] { args =>
      IO.pure(ok(s"${args.a + args.b}"))
    }
    val server = Server.fromTools[IO](ServerInfo("test", "1.0.0"), tools)

    test("add tool returns correct result"):
      ServerTest(server).use: client =>
        for result <- client.callTool("add", AddArgs(2.0, 3.0))
        yield assertEquals(result.textContent, "5.0")

  // ---- snippet at line 70
  import mcp4s.server.testing.*
  import mcp4s.server.testing.ToolsTest.*

  class MyToolsSuite extends CatsEffectSuite:
    import mcp4s.server.dsl.*

    @description("Add two numbers")
    case class MyAddArgs(a: Double, b: Double) derives Schema

    val tools = Tool.from[MyAddArgs].withName("add").handle[IO] { args =>
      IO.pure(ok(s"${args.a + args.b}"))
    }

    test("call tool directly"):
      for result <- tools.testCall("add", args("a" -> 3.0, "b" -> 2.0))
      yield assertEquals(result.textContent, "5.0")

    test("tool exists"):
      for exists <- tools.hasTool("add")
      yield assert(exists)

    test("get tool definition"):
      for tool <- tools.assertTool("add")
      yield assertEquals(tool.name, "add")

  // ---- snippet at line 112
  import mcp4s.server.testing.args

  args("a" -> 2.0, "b" -> 3.0)              // Json.obj("a" -> 2.0, "b" -> 3.0)
  args("query" -> "hello", "limit" -> 10)    // Json.obj("query" -> "hello", "limit" -> 10)
  args("name" -> "Alice")                    // Json.obj("name" -> "Alice")
  args.empty                                 // Json.obj()

