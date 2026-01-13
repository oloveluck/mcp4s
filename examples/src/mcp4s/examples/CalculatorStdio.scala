package mcp4s.examples

import cats.effect.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.server.transport.*

/** Stdio version of the calculator server.
  *
  * This version is suitable for use with Claude Desktop or other MCP clients
  * that communicate via stdio.
  *
  * To use with Claude Desktop, add to your config:
  * {
  *   "mcpServers": {
  *     "calculator": {
  *       "command": "path/to/java",
  *       "args": ["-jar", "path/to/mcp4s-examples.jar", "stdio"]
  *     }
  *   }
  * }
  */
object CalculatorStdio extends IOApp.Simple:

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    StdioTransport.run[IO](CalculatorServer.server)
