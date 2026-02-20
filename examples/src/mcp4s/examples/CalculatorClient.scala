package mcp4s.examples

import cats.effect.{IO, IOApp}
import io.circe.Json
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.*
import mcp4s.protocol.*
import mcp4s.client.transport.*
import org.http4s.ember.client.EmberClientBuilder

/** Example MCP client that connects to the calculator server.
  *
  * Usage:
  *   1. Start the calculator server: sbt "examples/runMain mcp4s.examples.CalculatorServer"
  *   2. In another terminal: sbt "examples/runMain mcp4s.examples.CalculatorClient"
  */
object CalculatorClient extends IOApp.Simple:

  val client: McpClient[IO] = McpClient.builder[IO]
    .withInfo(ClientInfo("calculator-client", "1.0.0"))
    .withRoots(List(
      Root("file:///tmp/workspace", Some("Workspace"))
    ))
    .build

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    IO.println("Connecting to Calculator MCP Server...") *>
      EmberClientBuilder.default[IO].build.use{ httpClient =>
         HttpClientTransport.connect[IO](
          client,
          HttpClientConfig("http://localhost:3000"),
          httpClient
        ).use { conn =>
          for
            _ <- IO.println(s"Connected to: ${conn.serverInfo.name} v${conn.serverInfo.version}")
            _ <- IO.println("")

            // List available tools
            tools <- conn.listTools
            _ <- IO.println(s"Available tools: ${tools.map(_.name).mkString(", ")}")
            _ <- IO.println("")

            // Test addition
            _ <- IO.println("Testing 5 + 3:")
            addResult <- conn.callTool("add", Json.obj(
              "a" -> Json.fromDouble(5.0).get,
              "b" -> Json.fromDouble(3.0).get
            ))
            _ <- IO.println(s"  ${formatResult(addResult)}")

            // Test subtraction
            _ <- IO.println("Testing 10 - 4:")
            subResult <- conn.callTool("subtract", Json.obj(
              "a" -> Json.fromDouble(10.0).get,
              "b" -> Json.fromDouble(4.0).get
            ))
            _ <- IO.println(s"  ${formatResult(subResult)}")

            // Test multiplication
            _ <- IO.println("Testing 7 * 8:")
            mulResult <- conn.callTool("multiply", Json.obj(
              "a" -> Json.fromDouble(7.0).get,
              "b" -> Json.fromDouble(8.0).get
            ))
            _ <- IO.println(s"  ${formatResult(mulResult)}")

            // Test division
            _ <- IO.println("Testing 100 / 4:")
            divResult <- conn.callTool("divide", Json.obj(
              "a" -> Json.fromDouble(100.0).get,
              "b" -> Json.fromDouble(4.0).get
            ))
            _ <- IO.println(s"  ${formatResult(divResult)}")

            // Test division by zero
            _ <- IO.println("Testing 5 / 0 (should error):")
            divZeroResult <- conn.callTool("divide", Json.obj(
              "a" -> Json.fromDouble(5.0).get,
              "b" -> Json.fromDouble(0.0).get
            ))
            _ <- IO.println(s"  ${formatResult(divZeroResult)}")

            _ <- IO.println("")
            _ <- IO.println("All tests completed!")

            _ <- conn.shutdown
          yield ()
        }
      }

  private def formatResult(result: ToolResult): String =
    result.content.headOption match
      case Some(tc: TextContent) => tc.text
      case _ => result.toString
