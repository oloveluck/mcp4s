// GENERATED from reference/errors.md — do not edit; regenerate with snippet_harness.py
package docsnippets.reference_errors

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
  // ---- snippet at line 41
  import mcp4s.server.dsl.*

  case class RiskyArgs(input: String) derives Schema
  def doWork(args: RiskyArgs): IO[String] = ???

  val risky = Tool("risky").withDescription("May fail").input[RiskyArgs].handle[IO] { args =>
    doWork(args).attempt.map:
      case Right(r) => ok(r)
      case Left(e)  => error(e.getMessage)
  }

  // ---- snippet at line 56
  conn.callTool("tool", Json.obj()).attempt.flatMap:
    case Right(result) if result.isError.getOrElse(false) => IO.println("Tool error")
    case Right(result)     => IO.println(s"Success: $result")
    case Left(e: McpError) => IO.println(s"Protocol error: ${e.message}")
    case Left(e)           => IO.println(s"Connection error: ${e.getMessage}")

  // ---- snippet at line 70
  import org.http4s.client.middleware.{Retry, RetryPolicy}
  import mcp4s.client.transport.HttpTransportConfig
  import scala.concurrent.duration.*

  val retryPolicy = RetryPolicy[IO](RetryPolicy.exponentialBackoff(maxWait = 10.seconds, maxRetry = 3))
  val resilientClient = Retry(retryPolicy)(httpClient)

  client.http(HttpTransportConfig[IO]("http://localhost:3000/mcp"), resilientClient).use: conn =>
    conn.callTool("tool", Json.obj())

