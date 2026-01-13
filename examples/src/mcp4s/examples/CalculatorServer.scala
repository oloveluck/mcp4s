package mcp4s.examples

import cats.effect.{IO, IOApp}
import io.circe.*
import mcp4s.protocol.{
  GetPromptResult,
  JsonSchema,
  Prompt,
  PromptArgument,
  PromptMessage,
  Resource as McpResource,
  ResourceContent,
  Role,
  ServerInfo,
  TextContent,
  Tool,
  ToolResult
}
import org.typelevel.otel4s.trace.Tracer
import mcp4s.server.*
import mcp4s.server.transport.*

/** Example MCP server that provides calculator tools.
  *
  * Run with: sbt "examples/run"
  * Test with: curl -X POST http://localhost:3000/message \
  *   -H "Content-Type: application/json" \
  *   -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}'
  */
object CalculatorServer extends IOApp.Simple:

  val server: McpServer[IO] = McpServer.builder[IO]
    .withInfo(ServerInfo("calculator-server", "1.0.0"))
    .withTool(
      Tool(
        name = "add",
        description = Some("Add two numbers"),
        inputSchema = JsonSchema.obj(
          properties = Map(
            "a" -> JsonSchema.number(Some("First number")),
            "b" -> JsonSchema.number(Some("Second number"))
          ),
          required = List("a", "b")
        )
      ),
      args => {
        val cursor = args.hcursor
        val result = for
          a <- cursor.get[Double]("a")
          b <- cursor.get[Double]("b")
        yield a + b

        result.fold(
          err => IO.pure(ToolResult.error(s"Invalid arguments: ${err.getMessage}")),
          sum => IO.pure(ToolResult.text(s"Result: $sum"))
        )
      }
    )
    .withTool(
      Tool(
        name = "subtract",
        description = Some("Subtract two numbers"),
        inputSchema = JsonSchema.obj(
          properties = Map(
            "a" -> JsonSchema.number(Some("Number to subtract from")),
            "b" -> JsonSchema.number(Some("Number to subtract"))
          ),
          required = List("a", "b")
        )
      ),
      args => {
        val cursor = args.hcursor
        val result = for
          a <- cursor.get[Double]("a")
          b <- cursor.get[Double]("b")
        yield a - b

        result.fold(
          err => IO.pure(ToolResult.error(s"Invalid arguments: ${err.getMessage}")),
          diff => IO.pure(ToolResult.text(s"Result: $diff"))
        )
      }
    )
    .withTool(
      Tool(
        name = "multiply",
        description = Some("Multiply two numbers"),
        inputSchema = JsonSchema.obj(
          properties = Map(
            "a" -> JsonSchema.number(Some("First number")),
            "b" -> JsonSchema.number(Some("Second number"))
          ),
          required = List("a", "b")
        )
      ),
      args => {
        val cursor = args.hcursor
        val result = for
          a <- cursor.get[Double]("a")
          b <- cursor.get[Double]("b")
        yield a * b

        result.fold(
          err => IO.pure(ToolResult.error(s"Invalid arguments: ${err.getMessage}")),
          product => IO.pure(ToolResult.text(s"Result: $product"))
        )
      }
    )
    .withTool(
      Tool(
        name = "divide",
        description = Some("Divide two numbers"),
        inputSchema = JsonSchema.obj(
          properties = Map(
            "a" -> JsonSchema.number(Some("Dividend")),
            "b" -> JsonSchema.number(Some("Divisor"))
          ),
          required = List("a", "b")
        )
      ),
      args => {
        val cursor = args.hcursor
        val result = for
          a <- cursor.get[Double]("a")
          b <- cursor.get[Double]("b")
        yield (a, b)

        result.fold(
          err => IO.pure(ToolResult.error(s"Invalid arguments: ${err.getMessage}")),
          { case (a, b) =>
            if b == 0 then IO.pure(ToolResult.error("Cannot divide by zero"))
            else IO.pure(ToolResult.text(s"Result: ${a / b}"))
          }
        )
      }
    )
    .withResource(
      McpResource(
        uri = "calc://help",
        name = "Calculator Help",
        description = Some("Help information for the calculator"),
        mimeType = Some("text/plain")
      ),
      _ => IO.pure(ResourceContent.text(
        "calc://help",
        """Calculator MCP Server
          |
          |Available tools:
          |- add: Add two numbers (a + b)
          |- subtract: Subtract two numbers (a - b)
          |- multiply: Multiply two numbers (a * b)
          |- divide: Divide two numbers (a / b)
          |
          |Each tool takes parameters 'a' and 'b' as numbers.""".stripMargin,
        Some("text/plain")
      ))
    )
    .withPrompt(
      Prompt(
        name = "calculate",
        description = Some("Prompt for performing a calculation"),
        arguments = List(
          PromptArgument("operation", Some("The operation: add, subtract, multiply, or divide"), required = true),
          PromptArgument("a", Some("First number"), required = true),
          PromptArgument("b", Some("Second number"), required = true)
        )
      ),
      args => {
        val op = args.getOrElse("operation", "add")
        val a = args.getOrElse("a", "0")
        val b = args.getOrElse("b", "0")

        IO.pure(GetPromptResult(
          description = Some(s"Calculate $a $op $b"),
          messages = List(
            PromptMessage(
              role = Role.User,
              content = TextContent(s"Please calculate: $a $op $b")
            )
          )
        ))
      }
    )
    .build

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    IO.println("Starting Calculator MCP Server on http://localhost:3000") *>
      HttpTransport.serve[IO](server, HttpConfig.default)
        .useForever
