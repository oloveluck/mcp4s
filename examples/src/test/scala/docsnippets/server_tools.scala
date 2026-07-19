// GENERATED from server/tools.md — do not edit; regenerate with snippet_harness.py
package docsnippets.server_tools

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
  import mcp4s.server.dsl.*

  @description("Search documents")
  case class SearchArgs(
    query: String,
    @description("Max results") limit: Option[Int]
  ) derives Schema

  // ---- snippet at line 30
  import mcp4s.server.dsl.*

  val search = Tool("search")
    .withDescription("Search the document index")
    .input[SearchArgs]
    .handle[IO] { args =>
      IO.pure(ok(s"Searching for ${args.query}"))
    }

  // ---- snippet at line 45
  // name = "search", description = "Search documents"
  Tool.from[SearchArgs].handle[IO](args => IO.pure(ok("result")))

  // ---- snippet at line 54
  Tool("version").withDescription("Get server version").handle[IO](_ => IO.pure(ok("1.0.0")))

  // ---- snippet at line 62
  val endpoint = Tool("search_docs").withDescription("Search the docs").input[SearchArgs]

  def params: CreateMessageParams = ???
  object database:
    def search(query: String): Stream[IO, String] = ???

  // 1. Effectful
  endpoint.handle[IO](args => IO.pure(ok("done")))

  // 2. Effectful + ToolContext (sampling, elicitation, progress, logging)
  endpoint.handleWith[IO] { (args, ctx) =>
    for
      _        <- ctx.log(LogLevel.Info, "Processing")
      _        <- ctx.progress(0.5, Some(100))
      response <- ctx.sampling.createMessage(params)
    yield ok(response.content.toString)
  }

  // 3. Streaming — on the plain request/response path the last emitted value is the result
  endpoint.stream[IO](args => database.search(args.query).map(r => ok(r.toString)))

  // 4. Streaming + ToolContext
  endpoint.streamWith[IO] { (args, ctx) =>
    database.search(args.query).evalTap(_ => ctx.progress(0.5, None)).map(r => ok(r.toString))
  }

  // ---- snippet at line 98
  case class CalcArgs(a: Double, b: Double) derives Schema

  case class CalcResult(
    @description("The computed value") result: Double,
    @description("Operation performed") operation: String
  ) derives Schema

  val calculate = Tool("calculate")
    .withDescription("Calculate")
    .input[CalcArgs]
    .output[CalcResult]
    .handle[IO](args => IO.pure(CalcResult(args.a + args.b, "add")))

  // ---- snippet at line 121
  case class DeleteArgs(path: String) derives Schema

  Tool("delete_file")
    .withDescription("Delete a file")
    .input[DeleteArgs]
    .withAnnotations(ToolAnnotations(destructiveHint = Some(true)))
    .handle[IO](args => IO.pure(ok(s"Deleted ${args.path}")))

  // ---- snippet at line 133
  ok("success")                    // Text result
  error("failed")                  // Error result
  content(textContent("a"), textContent("b"))  // Multiple items

  // ---- snippet at line 143
  val tools = search |+| calculate

  val server = McpServer[IO](ServerInfo("calc", "1.0.0")).withTools(tools)

