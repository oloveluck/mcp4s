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

import cats.effect.*
import org.typelevel.otel4s.trace.Tracer

/** Stdio version of the calculator server.
  *
  * This version is suitable for use with Claude Desktop or other MCP clients that communicate via
  * stdio.
  *
  * To use with Claude Desktop, add to your config: { "mcpServers": { "calculator": { "command":
  * "path/to/java", "args": ["-jar", "path/to/mcp4s-examples.jar", "stdio"] } } }
  */
object CalculatorStdio extends IOApp.Simple:

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    CalculatorServer.server.stdio.run
