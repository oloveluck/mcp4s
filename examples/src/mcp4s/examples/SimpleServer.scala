package mcp4s.examples

import cats.effect.{IO, IOApp}
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.transport.*
import org.typelevel.otel4s.trace.Tracer

// Simple args for the calculator tool
case class CalcArgs(a: Double, b: Double)
object CalcArgs:
  given Decoder[CalcArgs] = deriveDecoder
  given ToolInput[CalcArgs] = ToolInput.derived

/** Simple MCP server without auth for conformance testing.
  *
  * Run with: mill examples.runMain mcp4s.examples.SimpleServer
  */
object SimpleServer extends IOApp.Simple:

  val server: McpServer[IO] = McpServer
    .builder[IO]
    .withInfo(ServerInfo("simple-server", "1.0.0"))
    .tool[CalcArgs]("add", "Add two numbers") { args =>
      IO.pure(ToolResult.text(s"Result: ${args.a + args.b}"))
    }
    .resource("test://readme", "Test readme")(
      "This is a simple test server for conformance testing."
    )
    .prompt("test-prompt", "A test prompt") { _ =>
      IO.pure(
        GetPromptResult(
          description = Some("A simple test prompt"),
          messages = List(PromptMessage(Role.User, TextContent("Hello from test prompt")))
        )
      )
    }
    .build

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    // No auth configuration - simple HTTP server
    val httpConfig = HttpConfig[IO]()
    IO.println("Starting Simple MCP Server on http://localhost:3000") *>
      HttpTransport.serve[IO](server, httpConfig).useForever
