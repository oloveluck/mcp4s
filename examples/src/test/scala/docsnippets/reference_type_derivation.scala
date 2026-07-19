// GENERATED from reference/type-derivation.md — do not edit; regenerate with snippet_harness.py
package docsnippets.reference_type_derivation

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
  import mcp4s.server.dsl.*

  @description("Search documents")
  case class SearchArgs(
    @description("Search query") query: String,
    @description("Max results to return") limit: Option[Int]
  ) derives Schema

  // ---- snippet at line 28
  def search(query: String, limit: Int): IO[String] = ???   // your search logic

  Tool.from[SearchArgs].handle[IO](args => search(args.query, args.limit.getOrElse(10)).map(ok(_)))

  // ---- snippet at line 40
  enum Unit derives Schema:
    case Celsius, Fahrenheit

  case class Coordinates(lat: Double, lon: Double)   // nested: derived automatically

  @description("Get a weather forecast")
  case class Forecast(
    @description("City to look up") city: String,
    where: Coordinates,
    days: Int = 3,
    unit: Unit = Unit.Celsius,
    tags: Map[String, String] = Map.empty
  ) derives Schema

  // ---- snippet at line 75
  sealed trait Shape derives Schema
  case class Circle(radius: Double) extends Shape
  case class Rect(w: Double, h: Double) extends Shape

  // on the wire: {"type": "Circle", "radius": 2.0}

  // ---- snippet at line 87
  case class Tree(value: Int, children: List[Tree])

  given Schema[Tree] = Schema.defer(Schema.derived[Tree])

  // ---- snippet at line 99
  case class UserId(value: String)
  given Schema[UserId] = Schema.bijection[String, UserId](UserId(_), _.value)

  // ---- snippet at line 108
  case class CalcArgs(a: Double, b: Double) derives Schema

  case class CalcResult(
    @description("The computed value") result: Double,
    @description("The operation") operation: String
  ) derives Schema

  Tool("calculate").input[CalcArgs].output[CalcResult]
    .handle[IO](args => IO.pure(CalcResult(args.a + args.b, "add")))

  // ---- snippet at line 132
  @description("Greet someone")
  case class GreetArgs(
    @description("Name to greet") name: String,
    @description("How excited") excitement: Int = 1
  ) derives Schema

  Prompt.from[GreetArgs].handle[IO] { args =>
    IO.pure(messages(user(s"Hello, ${args.name}${"!" * args.excitement}")))
  }

  // ---- snippet at line 148
  // Class-level — becomes the tool/prompt description
  @description("Add two numbers")
  case class AddArgs(
    @description("First number") a: Double,
    @description("Second number") b: Double
  ) derives Schema

