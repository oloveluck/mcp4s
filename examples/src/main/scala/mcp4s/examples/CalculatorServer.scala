/*
 * Copyright 2025 MCP4S Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mcp4s.examples

import cats.effect.{IO, IOApp}
import cats.syntax.semigroup.*
import mcp4s.protocol.*
import mcp4s.schema.Schema
import mcp4s.server.*
import org.typelevel.otel4s.trace.Tracer

@description("Add two numbers")
case class AddArgs(
    @description("First number") a: Double,
    @description("Second number") b: Double
) derives Schema

@description("Subtract two numbers")
case class SubtractArgs(
    @description("Number to subtract from") a: Double,
    @description("Number to subtract") b: Double
) derives Schema

@description("Multiply two numbers")
case class MultiplyArgs(
    @description("First number") a: Double,
    @description("Second number") b: Double
) derives Schema

@description("Divide two numbers")
case class DivideArgs(
    @description("Dividend") a: Double,
    @description("Divisor") b: Double
) derives Schema

@description("Sum a list of numbers with progress reporting")
case class BatchAddArgs(
    @description("List of numbers to sum") numbers: List[Double]
) derives Schema

@description("Perform a calculation")
case class CalculateArgs(
    @description("The operation: add, subtract, multiply, or divide") operation: String,
    @description("First number") a: String,
    @description("Second number") b: String
) derives Schema

/** Example MCP server that provides calculator tools.
  *
  * Run with: mill examples.runMain mcp4s.examples.CalculatorServer Test with: curl -X POST
  * http://localhost:3000/mcp \ -H "Content-Type: application/json" \ -H "Accept: application/json,
  * text/event-stream" \ -d
  * '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}'
  */
object CalculatorServer extends IOApp.Simple:

  import mcp4s.server.dsl.*

  val mathTools: Tools[IO] =
    Tool.from[AddArgs].handle[IO] { args =>
      IO.pure(ok(s"Result: ${args.a + args.b}"))
    } |+|
      Tool.from[SubtractArgs].handle[IO] { args =>
        IO.pure(ok(s"Result: ${args.a - args.b}"))
      } |+|
      Tool.from[MultiplyArgs].handle[IO] { args =>
        IO.pure(ok(s"Result: ${args.a * args.b}"))
      } |+|
      Tool.from[DivideArgs].handle[IO] { args =>
        if args.b == 0 then IO.pure(error("Cannot divide by zero"))
        else IO.pure(ok(s"Result: ${args.a / args.b}"))
      } |+|
      Tool.from[BatchAddArgs].handleWith[IO] { (args, ctx) =>
        import scala.concurrent.duration.*
        val total = args.numbers.length.toDouble
        args.numbers.zipWithIndex
          .foldLeft(IO.pure(0.0)) { case (accF, (n, i)) =>
            for
              acc <- accF
              _   <- ctx.progress(i.toDouble, Some(total))
              _   <- IO.sleep(50.millis)
            yield acc + n
          }
          .flatMap: sum =>
            ctx.progress(total, Some(total)) *>
              IO.pure(ok(s"Result: $sum"))
      }

  val resources: Resources[IO] =
    Resource.text[IO]("calc://help", "Calculator Help") {
      """Calculator MCP Server
        |
        |Available tools: add, subtract, multiply, divide
        |Each tool takes 'a' and 'b' as numbers.""".stripMargin
    }

  val prompts: Prompts[IO] =
    Prompt.from[CalculateArgs].handle[IO] { args =>
      IO.pure(
        messages(s"Calculate ${args.a} ${args.operation} ${args.b}")(
          user(s"Please calculate: ${args.a} ${args.operation} ${args.b}")
        )
      )
    }

  val server: Server[IO] =
    Server.from[IO](ServerInfo("calculator-server", "1.0.0"), mathTools, resources, prompts)

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    IO.println("Starting Calculator MCP Server on http://localhost:3000") *>
      server.http().resource.useForever
