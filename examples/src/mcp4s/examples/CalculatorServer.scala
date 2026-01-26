package mcp4s.examples

import cats.effect.{IO, IOApp}
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.auth.*
import mcp4s.server.transport.*
import org.typelevel.otel4s.trace.Tracer

// Type-safe tool inputs with descriptions
case class AddArgs(
    @description("First number") a: Double,
    @description("Second number") b: Double
)
object AddArgs:
  given Decoder[AddArgs] = deriveDecoder
  given ToolInput[AddArgs] = ToolInput.derived

case class SubtractArgs(
    @description("Number to subtract from") a: Double,
    @description("Number to subtract") b: Double
)
object SubtractArgs:
  given Decoder[SubtractArgs] = deriveDecoder
  given ToolInput[SubtractArgs] = ToolInput.derived

case class MultiplyArgs(
    @description("First number") a: Double,
    @description("Second number") b: Double
)
object MultiplyArgs:
  given Decoder[MultiplyArgs] = deriveDecoder
  given ToolInput[MultiplyArgs] = ToolInput.derived

case class DivideArgs(
    @description("Dividend") a: Double,
    @description("Divisor") b: Double
)
object DivideArgs:
  given Decoder[DivideArgs] = deriveDecoder
  given ToolInput[DivideArgs] = ToolInput.derived

// Type-safe prompt input
case class CalculatePromptArgs(
    @description("The operation: add, subtract, multiply, or divide") operation: String,
    @description("First number") a: String,
    @description("Second number") b: String
)
object CalculatePromptArgs:
  given PromptInput[CalculatePromptArgs] = PromptInput.derived

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
    validator = TokenValidator.allowAll[IO],  // Accepts any token for dev
    requiredScopes = Set.empty
  )

  val server: McpServer[IO] = McpServer
    .builder[IO]
    .withInfo(ServerInfo("calculator-server", "1.0.0"))
    // Type-safe tools - schemas and descriptions auto-derived
    .tool[AddArgs]("add", "Add two numbers") { args =>
      IO.pure(ToolResult.text(s"Result: ${args.a + args.b}"))
    }
    .tool[SubtractArgs]("subtract", "Subtract two numbers") { args =>
      IO.pure(ToolResult.text(s"Result: ${args.a - args.b}"))
    }
    .tool[MultiplyArgs]("multiply", "Multiply two numbers") { args =>
      IO.pure(ToolResult.text(s"Result: ${args.a * args.b}"))
    }
    .tool[DivideArgs]("divide", "Divide two numbers") { args =>
      if args.b == 0 then IO.pure(ToolResult.error("Cannot divide by zero"))
      else IO.pure(ToolResult.text(s"Result: ${args.a / args.b}"))
    }
    // Simple static resource
    .resource("calc://help", "Calculator Help")(
      """Calculator MCP Server
        |
        |Available tools: add, subtract, multiply, divide
        |Each tool takes 'a' and 'b' as numbers.""".stripMargin
    )
    // Type-safe prompt - arguments auto-derived
    .prompt[CalculatePromptArgs]("calculate", "Perform a calculation") { args =>
      IO.pure(
        GetPromptResult(
          description = Some(s"Calculate ${args.a} ${args.operation} ${args.b}"),
          messages = List(
            PromptMessage(Role.User, TextContent(s"Please calculate: ${args.a} ${args.operation} ${args.b}"))
          )
        )
      )
    }
    .build

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    val httpConfig = HttpConfig[IO](auth = Some(authConfig))
    IO.println("Starting Calculator MCP Server on http://localhost:3000") *>
      HttpTransport.serve[IO](server, httpConfig).useForever
