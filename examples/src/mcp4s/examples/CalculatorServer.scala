package mcp4s.examples

import cats.effect.{IO, IOApp}
import cats.syntax.semigroup.*
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.mcp
import mcp4s.server.mcp.{ok, error, user, messages, pure}
import mcp4s.server.auth.*
import mcp4s.server.transport.*
import org.typelevel.otel4s.trace.Tracer

case class AddArgs(
    @description("First number") a: Double,
    @description("Second number") b: Double
) derives ToolInput

case class SubtractArgs(
    @description("Number to subtract from") a: Double,
    @description("Number to subtract") b: Double
) derives ToolInput

case class MultiplyArgs(
    @description("First number") a: Double,
    @description("Second number") b: Double
) derives ToolInput

case class DivideArgs(
    @description("Dividend") a: Double,
    @description("Divisor") b: Double
) derives ToolInput

case class CalculatePromptArgs(
    @description("The operation: add, subtract, multiply, or divide") operation: String,
    @description("First number") a: String,
    @description("Second number") b: String
) derives PromptInput

/** Example MCP server that provides calculator tools.
  *
  * Run with: mill examples.runMain mcp4s.examples.CalculatorServer
  * Test with: curl -X POST http://localhost:3000/mcp \
  *   -H "Content-Type: application/json" \
  *   -H "Accept: application/json, text/event-stream" \
  *   -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}'
  */
object CalculatorServer extends IOApp.Simple:

  val authConfig: AuthConfig[IO] = AuthConfig[IO](
    metadata = ProtectedResourceMetadata(
      resource = "http://localhost:3000",
      authorizationServers = List("https://auth.example.com"),
      scopesSupported = Some(List("mcp:read", "mcp:write"))
    ),
    validator = TokenValidator.allowAll[IO],
    requiredScopes = Set.empty
  )

  val mathTools: McpTools[IO] =
    mcp.Tool[IO, AddArgs]("add", "Add two numbers") { args =>
      ok(s"Result: ${args.a + args.b}").pure[IO]
    } |+|
    mcp.Tool[IO, SubtractArgs]("subtract", "Subtract two numbers") { args =>
      ok(s"Result: ${args.a - args.b}").pure[IO]
    } |+|
    mcp.Tool[IO, MultiplyArgs]("multiply", "Multiply two numbers") { args =>
      ok(s"Result: ${args.a * args.b}").pure[IO]
    } |+|
    mcp.Tool[IO, DivideArgs]("divide", "Divide two numbers") { args =>
      if args.b == 0 then error("Cannot divide by zero").pure[IO]
      else ok(s"Result: ${args.a / args.b}").pure[IO]
    }

  val resources: McpResources[IO] =
    mcp.Resource.text[IO]("calc://help", "Calculator Help") {
      """Calculator MCP Server
        |
        |Available tools: add, subtract, multiply, divide
        |Each tool takes 'a' and 'b' as numbers.""".stripMargin
    }

  val prompts: McpPrompts[IO] =
    mcp.Prompt[IO, CalculatePromptArgs]("calculate", "Perform a calculation") { args =>
      messages(s"Calculate ${args.a} ${args.operation} ${args.b}")(
        user(s"Please calculate: ${args.a} ${args.operation} ${args.b}")
      ).pure[IO]
    }

  val server: McpServer[IO] = McpServer
    .builder[IO]
    .withInfo(ServerInfo("calculator-server", "1.0.0"))
    .withTools(mathTools)
    .withResources(resources)
    .withPrompts(prompts)
    .build

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    val httpConfig = HttpConfig[IO](auth = Some(authConfig))
    IO.println("Starting Calculator MCP Server on http://localhost:3000") *>
      HttpTransport.serve[IO](server, httpConfig).useForever
