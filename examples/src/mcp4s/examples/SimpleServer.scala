package mcp4s.examples

import cats.effect.{IO, IOApp}
import cats.syntax.semigroup.*
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.mcp
import mcp4s.server.mcp.{ok, user, pure}
import mcp4s.server.transport.*
import org.typelevel.otel4s.trace.Tracer

case class CalcArgs(a: Double, b: Double) derives ToolInput

/** Simple MCP server without auth for conformance testing.
  *
  * Run with: mill examples.runMain mcp4s.examples.SimpleServer
  */
object SimpleServer extends IOApp.Simple:

  val tools: McpTools[IO] =
    mcp.Tool[IO, CalcArgs]("add", "Add two numbers") { args =>
      ok(s"Result: ${args.a + args.b}").pure[IO]
    }

  val resources: McpResources[IO] =
    mcp.Resource.text[IO]("test://readme", "Test readme") {
      "This is a simple test server for conformance testing."
    }

  val prompts: McpPrompts[IO] =
    mcp.Prompt.withDesc[IO]("test-prompt", "A test prompt", "A simple test prompt")(
      user("Hello from test prompt")
    )

  val server: McpServer[IO] = McpServer.from[IO](
    info = ServerInfo("simple-server", "1.0.0"),
    tools = tools,
    resources = resources,
    prompts = prompts
  )

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    val httpConfig = HttpConfig[IO]()
    IO.println("Starting Simple MCP Server on http://localhost:3000") *>
      HttpTransport.serve[IO](server, httpConfig).useForever
