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

import cats.effect.IO
import com.comcast.ip4s.port
import io.circe.Json
import mcp4s.client.McpClient
import mcp4s.client.syntax.*
import mcp4s.testkit.*
import mcp4s.protocol.*
import mcp4s.server.Server
import mcp4s.server.syntax.*
import munit.CatsEffectSuite
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.otel4s.trace.Tracer

/** Exercises the fluent transport-selection syntax (`server.serve*` / `client.connect*`) end-to-end
  * against live Ember servers, so the convenience surface is covered in addition to the explicit
  * `*Transport` calls used by [[NetworkIntegrationSpec]].
  */
class TransportSyntaxSpec extends CatsEffectSuite:

  given Tracer[IO] = Tracer.noop[IO]

  private def server: Server[IO]    = TestServers.simple[IO]
  private def client: McpClient[IO] = DeterministicClients.simple[IO]

  test("server.serveHttp + client.connectHttp (caller-supplied Client) round-trips a tool call") {
    server
      .serveHttp(port"0")
      .use: http =>
        val url = s"http://localhost:${http.address.getPort}"
        EmberClientBuilder
          .default[IO]
          .build
          .use: httpClient =>
            client
              .connectHttp(url, httpClient)
              .use: conn =>
                for result <- conn.callTool(
                    "add",
                    Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))
                  )
                yield result.content.head match
                  case TextContent(text, _, _) => assertEquals(text, "5.0")
                  case _                       => fail("Expected text content")
  }

  test("client.connectHttp (auto-Ember, JVM) round-trips a tool call") {
    server
      .serveHttp(port"0")
      .use: http =>
        val url = s"http://localhost:${http.address.getPort}"
        client
          .connectHttp(url)
          .use: conn =>
            for result <- conn.callTool(
                "add",
                Json.obj("a" -> Json.fromInt(10), "b" -> Json.fromInt(4))
              )
            yield result.content.head match
              case TextContent(text, _, _) => assertEquals(text, "14.0")
              case _                       => fail("Expected text content")
  }

  test("server.serveWebSocket + client.connectWebSocket round-trips a tool call") {
    server
      .serveWebSocket(port"0")
      .use: ws =>
        val url = s"ws://localhost:${ws.address.getPort}"
        client
          .connectWebSocket(url)
          .use: conn =>
            for
              tools <- conn.listAllTools
              result <- conn.callTool(
                "add",
                Json.obj("a" -> Json.fromInt(6), "b" -> Json.fromInt(1))
              )
            yield
              assert(tools.exists(_.name == "add"))
              result.content.head match
                case TextContent(text, _, _) => assertEquals(text, "7.0")
                case _                       => fail("Expected text content")
  }
