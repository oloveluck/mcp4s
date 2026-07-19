// GENERATED from server/resources.md — do not edit; regenerate with snippet_harness.py
package docsnippets.server_resources

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
  // ---- snippet at line 12
  import mcp4s.server.dsl.{Resource, *}

  def getStatus: IO[String] = IO.pure("all good")

  // Static text
  Resource.text[IO]("file:///readme", "README")("Hello world")

  // Dynamic (effectful)
  Resource[IO]("file:///status", "Status")(getStatus.map(s => text("file:///status", s)))

  // Template (pattern matching)
  Resource.template[IO]("api://users/{id}", "User", "Get user by ID"): uri =>
    val id = uri.split("/").last
    IO.pure(text(uri, s"""{"id": "$id"}"""))

  // ---- snippet at line 33
  val base64Data = "iVBORw0KGgoAAAANSUhEUg=="

  // Plain text
  text("uri", "text content")

  // Binary (base64-encoded)
  blob("uri", base64Data, "image/png")

  // ---- snippet at line 47
  val readme = Resource.text[IO]("file:///readme", "README")("Hello")
  val config = Resource.text[IO]("file:///config", "Config")("debug = false")
  val userTemplate = Resource.template[IO]("api://users/{id}", "User"): uri =>
    IO.pure(text(uri, "{}"))

  val resources = readme |+| config |+| userTemplate

object scope_2:
  import stubs.{*, given}
  // ---- snippet at line 59
  import mcp4s.server.dsl.{Resource, *}

  def getStatus: IO[String] = IO.pure("ok")

  val resources =
    Resource.text[IO]("file:///readme", "README")("Hello") |+|
      Resource.handler[IO]("file:///status", "Status")(_ =>
        getStatus.map(s => ResourceContent.text("file:///status", s))
      )

  val server = McpServer[IO](ServerInfo("my-server", "1.0.0")).withResources(resources)

  // ---- snippet at line 79
  import scala.concurrent.duration.*

  def dbChangeStream: Stream[IO, Unit] = ???
  def getDbStatus: IO[String]          = ???
  def configChanged: IO[Boolean]       = ???
  def readConfig: IO[String]           = ???

  // Change-stream driven — notifies when the stream emits
  Resource.subscribable[IO]("db://status", "DB Status", dbChangeStream)(uri =>
    getDbStatus.map(s => text(uri, s))
  )

  // Polling — checks a condition on an interval
  Resource.polling[IO]("file:///config", "Config", 10.seconds, configChanged)(uri =>
    readConfig.map(c => text(uri, c))
  )

  // ---- snippet at line 106
  case class Order(id: String):
    def toJson: String = s"""{"id": "$id"}"""

  object orderRepo:
    def findById(id: String): IO[Option[Order]] = ???

  Resource.template[IO]("db://orders/{orderId}", "Order", "Fetch order by ID"): uri =>
    orderRepo.findById(uri.split("/").last).flatMap:
      case Some(order) => IO.pure(text(uri, order.toJson))
      case None        => IO.raiseError(McpError.ResourceNotFound(uri))

