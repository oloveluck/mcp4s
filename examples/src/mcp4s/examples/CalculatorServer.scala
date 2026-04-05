package mcp4s.examples

import cats.effect.{IO, IOApp}
import cats.syntax.semigroup.*
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.mcp
import mcp4s.server.mcp.{ok, error, user, messages, pure}
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

case class BatchAddArgs(
    @description("List of numbers to sum") numbers: List[Double]
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
  *   -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}'
  */
object CalculatorServer extends IOApp.Simple:

  val mathTools: Tools[IO] =
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
    } |+|
    mcp.Tool.withContext[IO, BatchAddArgs]("batch_add", "Sum a list of numbers with progress reporting") { (args, ctx) =>
      import scala.concurrent.duration.*
      val total = args.numbers.length.toDouble
      args.numbers.zipWithIndex
        .foldLeft(IO.pure(0.0)) { case (accF, (n, i)) =>
          for
            acc <- accF
            _ <- ctx.progress(i.toDouble, Some(total))
            _ <- IO.sleep(50.millis)
          yield acc + n
        }
        .flatMap { sum =>
          ctx.progress(total, Some(total)) *>
            IO.pure(ok(s"Result: $sum"))
        }
    }

  val resources: Resources[IO] =
    mcp.Resource.text[IO]("calc://help", "Calculator Help") {
      """Calculator MCP Server
        |
        |Available tools: add, subtract, multiply, divide
        |Each tool takes 'a' and 'b' as numbers.""".stripMargin
    }

  val prompts: Prompts[IO] =
    mcp.Prompt[IO, CalculatePromptArgs]("calculate", "Perform a calculation") { args =>
      messages(s"Calculate ${args.a} ${args.operation} ${args.b}")(
        user(s"Please calculate: ${args.a} ${args.operation} ${args.b}")
      ).pure[IO]
    }

  val server: Server[IO] = Server
    .builder[IO]
    .withInfo(ServerInfo("calculator-server", "1.0.0"))
    .withTools(mathTools)
    .withResources(resources)
    .withPrompts(prompts)
    .build

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    IO.println("Starting Calculator MCP Server on http://localhost:3000") *>
      HttpTransport.serve[IO](server, HttpConfig[IO]()).useForever
