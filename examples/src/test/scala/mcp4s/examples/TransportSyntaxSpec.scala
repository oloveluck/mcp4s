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
import mcp4s.client.{McpClient, McpClientBuilder}
import mcp4s.client.syntax.*
import mcp4s.client.transport.HttpTransportConfig
import mcp4s.testkit.*
import mcp4s.protocol.*
import mcp4s.server.{McpServer, Server}
import mcp4s.server.transport.{HttpConfig, WebSocketConfig}
import munit.CatsEffectSuite
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.otel4s.trace.Tracer

/** Exercises the transport-binding surface (`server.http(...)` / `client.http(...)` and the
  * `McpServer`/`McpClientBuilder` builders) end-to-end against live Ember servers, so the
  * convenience surface is covered in addition to the explicit `*Transport` calls used by
  * [[NetworkIntegrationSpec]].
  */
class TransportSyntaxSpec extends CatsEffectSuite:

  given Tracer[IO] = Tracer.noop[IO]

  private def server: Server[IO]    = TestServers.simple[IO]
  private def client: McpClient[IO] = DeterministicClients.simple[IO]

  test("server.http + client.http (caller-supplied Client) round-trips a tool call") {
    server
      .http(HttpConfig(port = port"0"))
      .resource
      .use: http =>
        val url = s"http://localhost:${http.address.getPort}/mcp"
        EmberClientBuilder
          .default[IO]
          .build
          .use: httpClient =>
            client
              .http(HttpTransportConfig[IO](url), httpClient)
              .use: conn =>
                for result <- conn.callTool(
                    "add",
                    Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))
                  )
                yield result.content.head match
                  case TextContent(text, _, _) => assertEquals(text, "5.0")
                  case _                       => fail("Expected text content")
  }

  test("client.http (auto-Ember, JVM) round-trips a tool call") {
    server
      .http(HttpConfig(port = port"0"))
      .resource
      .use: http =>
        val url = s"http://localhost:${http.address.getPort}/mcp"
        client
          .http(url)
          .use: conn =>
            for result <- conn.callTool(
                "add",
                Json.obj("a" -> Json.fromInt(10), "b" -> Json.fromInt(4))
              )
            yield result.content.head match
              case TextContent(text, _, _) => assertEquals(text, "14.0")
              case _                       => fail("Expected text content")
  }

  test("server.webSocket + client.webSocket round-trips a tool call") {
    server
      .webSocket(WebSocketConfig(port = port"0"))
      .resource
      .use: ws =>
        val url = s"ws://localhost:${ws.address.getPort}/ws"
        client
          .webSocket(url)
          .use: conn =>
            for
              tools  <- conn.listAllTools
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

  test("McpServer builder + McpClientBuilder round-trip a tool call") {
    import mcp4s.server.dsl.*

    case class AddArgs(a: Double, b: Double) derives Schema
    val tools = Tool("add").withDescription("Add").input[AddArgs].handle[IO] { args =>
      IO.pure(ok(s"${args.a + args.b}"))
    }

    McpServer[IO](ServerInfo("builder-server", "1.0.0"))
      .withTools(tools)
      .http(HttpConfig(port = port"0"))
      .resource
      .use: http =>
        val url = s"http://localhost:${http.address.getPort}/mcp"
        McpClientBuilder[IO](ClientInfo("builder-client", "1.0.0"))
          .http(url)
          .use: conn =>
            for
              result <- conn.callTool(
                "add",
                Json.obj("a" -> Json.fromInt(4), "b" -> Json.fromInt(4))
              )
              // capabilities are derived: tools-only server advertises no resources/prompts
              _ = assertEquals(conn.serverCapabilities.resources, None)
              _ = assertEquals(conn.serverCapabilities.prompts, None)
              _ = assert(conn.serverCapabilities.tools.isDefined)
            yield result.content.head match
              case TextContent(text, _, _) => assertEquals(text, "8.0")
              case _                       => fail("Expected text content")
  }
