// GENERATED from advanced/tasks.md — do not edit; regenerate with snippet_harness.py
package docsnippets.advanced_tasks

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
  // ---- snippet at line 13
  import mcp4s.server.dsl.*

  case class ProcessArgs(items: List[String]) derives Schema
  def processItem(item: String): IO[String] = ???

  val process = Tool("process").withDescription("Process data").input[ProcessArgs].handleWith[IO] {
    (args, ctx) =>
      for results <- args.items.zipWithIndex.traverse { case (item, idx) =>
          ctx.progress(idx.toDouble / args.items.size, Some(args.items.size.toDouble)) *>
            processItem(item)
        }
      yield ok(results.mkString(", "))
  }

  // ---- snippet at line 35
  conn.callTool(
    "process",
    Json.obj("items" -> Json.arr(Json.fromString("a"), Json.fromString("b"))),
    p => IO.println(s"${p.progress}/${p.total.getOrElse("?")}")
  )

  // ---- snippet at line 48
  val ticker = Tool("count").withDescription("Count up").stream[IO] { _ =>
    Stream.range(1, 6).map(n => ok(s"count: $n"))
  }

