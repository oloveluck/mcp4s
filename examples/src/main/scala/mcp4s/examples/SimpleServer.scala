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
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.mcp
import mcp4s.server.mcp.{ok, user}
import mcp4s.server.transport.*
import org.typelevel.otel4s.trace.Tracer

@description("Add two numbers")
case class Add(
    @description("First number") a: Double,
    @description("Second number") b: Double
) derives ToolInput

/** Simple MCP server without auth for conformance testing.
  *
  * Run with: mill examples.runMain mcp4s.examples.SimpleServer
  */
object SimpleServer extends IOApp.Simple:

  val tools: Tools[IO] =
    mcp.Tool[IO, Add]: args =>
      IO.pure(ok(s"Result: ${args.a + args.b}"))

  val resources: Resources[IO] =
    mcp.Resource.text[IO]("test://readme", "Test readme") {
      "This is a simple test server for conformance testing."
    }

  val prompts: Prompts[IO] =
    mcp.Prompt.withDesc[IO]("test-prompt", "A test prompt", "A simple test prompt")(
      user("Hello from test prompt")
    )

  val server: Server[IO] = Server.from[IO](
    info = ServerInfo("simple-server", "1.0.0"),
    tools = tools,
    resources = resources,
    prompts = prompts
  )

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    val httpConfig   = HttpConfig[IO]()
    IO.println("Starting Simple MCP Server on http://localhost:3000") *>
      HttpTransport.serve[IO](server, httpConfig).useForever
