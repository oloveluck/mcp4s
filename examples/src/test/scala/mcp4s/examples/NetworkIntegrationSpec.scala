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

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.port
import io.circe.Json
import mcp4s.client.*
import mcp4s.client.transport.*
import mcp4s.testkit.*
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.transport.*
import munit.CatsEffectSuite
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.*
import org.http4s.circe.*
import org.typelevel.otel4s.trace.Tracer

import scala.concurrent.duration.*

/** Comprehensive MCP network integration test suite.
  *
  * Tests cover:
  *   - Protocol compliance (lifecycle, tools, resources, prompts, bidirectional)
  *   - Session & connectivity (multiple clients, transport equivalence)
  *   - Edge cases & race conditions
  *   - Multi-server topology
  *   - Chaos testing
  *   - Performance
  */
class NetworkIntegrationSpec extends CatsEffectSuite:

  given Tracer[IO] = Tracer.noop[IO]

  // === Test Server Setup ===

  def simpleServer: Server[IO] = TestServers.simple[IO]

  def simpleClient: McpClient[IO] = DeterministicClients.simple[IO]

  def httpServerResource(server: Server[IO]): Resource[IO, org.http4s.server.Server] =
    HttpTransport.serve[IO](server, HttpConfig(port = port"0"))

  def wsServerResource(server: Server[IO]): Resource[IO, org.http4s.server.Server] =
    WebSocketTransport.serve[IO](server, WebSocketConfig(port = port"0"))

  def httpConnection(
      client: McpClient[IO],
      port: Int
  ): Resource[IO, McpConnection[IO]] =
    EmberClientBuilder
      .default[IO]
      .build
      .flatMap: httpClient =>
        HttpClientTransport
          .connect[IO](client, HttpTransportConfig(s"http://localhost:$port/mcp"), httpClient)

  /** HTTP connection with http4s Retry middleware for high-load tests */
  def resilientHttpConnection(
      client: McpClient[IO],
      port: Int
  ): Resource[IO, McpConnection[IO]] =
    import org.http4s.client.middleware.{Retry, RetryPolicy as Http4sRetryPolicy}
    EmberClientBuilder
      .default[IO]
      .build
      .flatMap: httpClient =>
        val retryPolicy = Http4sRetryPolicy[IO](
          backoff = Http4sRetryPolicy.exponentialBackoff(maxWait = 1.second, maxRetry = 5),
          retriable = (_, result) =>
            result match
              case Left(_: java.io.IOException) => true
              case _                            => false
        )
        val resilientClient = Retry(retryPolicy)(httpClient)
        HttpClientTransport
          .connect[IO](client, HttpTransportConfig(s"http://localhost:$port/mcp"), resilientClient)

  def wsConnection(
      client: McpClient[IO],
      port: Int
  ): Resource[IO, McpConnection[IO]] =
    WebSocketClientTransport
      .connect[IO](client, WebSocketTransportConfig(s"ws://localhost:$port/ws"))

  // ============================================================================
  // CATEGORY 1: PROTOCOL COMPLIANCE
  // ============================================================================

  // --- Lifecycle Tests ---

  test("Protocol: initialize returns server info and capabilities") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        IO {
          assertEquals(conn.serverInfo.name, "test-server")
          assertEquals(conn.serverInfo.version, "1.0.0")
          assert(conn.serverCapabilities.tools.isDefined)
          assert(conn.serverCapabilities.resources.isDefined)
          assert(conn.serverCapabilities.prompts.isDefined)
        }
  }

  test("Protocol: ping succeeds after initialization") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        conn.ping.as(())
  }

  test("Protocol: shutdown completes gracefully") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        conn.shutdown.as(())
  }

  test("Protocol: multiple pings succeed") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        (1 to 5).toList.traverse_(_ => conn.ping)
  }

  // --- Tools Tests ---

  test("Protocol: tools/list returns all registered tools") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for tools <- conn.listAllTools
        yield
          assertEquals(tools.length, 4)
          assert(tools.exists(_.name == "add"))
          assert(tools.exists(_.name == "slow_add"))
          assert(tools.exists(_.name == "echo"))
          assert(tools.exists(_.name == "fail"))
  }

  test("Protocol: tools/call with valid arguments succeeds") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for result <- conn.callTool(
            "add",
            Json.obj("a" -> Json.fromDouble(5.0).get, "b" -> Json.fromDouble(3.0).get)
          )
        yield
          assertEquals(result.isError.getOrElse(false), false)
          result.content.head match
            case TextContent(text, _, _) => assertEquals(text, "8.0")
            case _                       => fail("Expected text content")
  }

  test("Protocol: tools/call returns error for unknown tool") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        conn
          .callTool("nonexistent", Json.obj())
          .attempt
          .map: result =>
            assert(result.isLeft)
  }

  test("Protocol: tools/call returns error for tool that throws") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        conn
          .callTool("fail", Json.obj())
          .attempt
          .map: result =>
            assert(result.isLeft)
  }

  test("Protocol: tools/call with invalid arguments returns error") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        conn
          .callTool("add", Json.obj("x" -> Json.fromInt(1)))
          .attempt
          .map: result =>
            assert(result.isLeft)
  }

  test("Protocol: multiple concurrent tool calls succeed") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        val calls = (1 to 10).toList.map { i =>
          conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
        }
        calls.parSequence.map: results =>
          assertEquals(results.length, 10)
          assert(results.forall(!_.isError.getOrElse(false)))
  }

  // --- Resources Tests ---

  test("Protocol: resources/list returns registered resources") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for resources <- conn.listAllResources
        yield
          assertEquals(resources.length, 2)
          assert(resources.exists(_.uri == "file:///test.txt"))
          assert(resources.exists(_.uri == "file:///binary.bin"))
  }

  test("Protocol: resources/read returns text content") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for content <- conn.readResource("file:///test.txt")
        yield
          assertEquals(content.uri, "file:///test.txt")
          assertEquals(content.text, Some("Hello, World!"))
  }

  test("Protocol: resources/read returns binary content (blob)") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for content <- conn.readResource("file:///binary.bin")
        yield
          assertEquals(content.uri, "file:///binary.bin")
          assert(content.blob.isDefined)
          assertEquals(content.mimeType, Some("application/octet-stream"))
  }

  test("Protocol: resources/read returns error for unknown resource") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        conn
          .readResource("file:///nonexistent.txt")
          .attempt
          .map: result =>
            assert(result.isLeft)
  }

  test("Protocol: resources/templates/list returns templates") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for templates <- conn.listAllResourceTemplates
        yield
          assertEquals(templates.length, 1)
          assertEquals(templates.head.uriTemplate, "file:///docs/{name}")
  }

  // --- Prompts Tests ---

  test("Protocol: prompts/list returns registered prompts") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for prompts <- conn.listAllPrompts
        yield
          assertEquals(prompts.length, 1)
          assertEquals(prompts.head.name, "greeting")
  }

  test("Protocol: prompts/get returns prompt with arguments") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for result <- conn.getPrompt("greeting", Map("name" -> "Alice"))
        yield
          assertEquals(result.description, Some("A friendly greeting"))
          assertEquals(result.messages.length, 1)
          result.messages.head.content match
            case TextContent(text, _, _) => assertEquals(text, "Hello, Alice!")
            case _                       => fail("Expected text content")
  }

  test("Protocol: prompts/get returns error for unknown prompt") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for
          // Do a successful operation first to ensure connection is established
          _      <- conn.listAllPrompts
          result <- conn.getPrompt("nonexistent", Map.empty[String, String]).attempt
        yield assert(result.isLeft)
  }

  // ============================================================================
  // CATEGORY 2: SESSION & CONNECTIVITY
  // ============================================================================

  test("Sessions: multiple concurrent clients have isolated state") {
    httpServerResource(simpleServer).use: server =>
      val port       = server.address.getPort
      val client1Res = httpConnection(simpleClient, port)
      val client2Res = httpConnection(simpleClient, port)
      val client3Res = httpConnection(simpleClient, port)

      (client1Res, client2Res, client3Res).tupled.use { case (conn1, conn2, conn3) =>
        for
          _ <- IO {
            assertEquals(conn1.serverInfo.name, "test-server")
            assertEquals(conn2.serverInfo.name, "test-server")
            assertEquals(conn3.serverInfo.name, "test-server")
          }
          tools1 <- conn1.listAllTools
          tools2 <- conn2.listAllTools
          tools3 <- conn3.listAllTools
        yield
          assertEquals(tools1.length, 4)
          assertEquals(tools2.length, 4)
          assertEquals(tools3.length, 4)
      }
  }

  test("Sessions: concurrent requests from different clients don't interfere") {
    httpServerResource(simpleServer).use: server =>
      val port = server.address.getPort
      (httpConnection(simpleClient, port), httpConnection(simpleClient, port)).tupled.use {
        case (conn1, conn2) =>
          val requests = List(
            conn1.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(1))),
            conn2.callTool("add", Json.obj("a" -> Json.fromInt(10), "b" -> Json.fromInt(10))),
            conn1.callTool("add", Json.obj("a" -> Json.fromInt(100), "b" -> Json.fromInt(100))),
            conn2.callTool("add", Json.obj("a" -> Json.fromInt(1000), "b" -> Json.fromInt(1000)))
          )
          for results <- requests.parSequence
          yield
            assertEquals(results.length, 4)
            val texts = results.flatMap(_.content.collect { case TextContent(t, _, _) => t })
            assert(texts.contains("2.0"))
            assert(texts.contains("20.0"))
            assert(texts.contains("200.0"))
            assert(texts.contains("2000.0"))
      }
  }

  test("Sessions: rapid connect/disconnect cycle") {
    httpServerResource(simpleServer).use: server =>
      val port = server.address.getPort
      (1 to 5).toList.traverse_ { _ =>
        httpConnection(simpleClient, port).use: conn =>
          conn.listAllTools.map(tools => assertEquals(tools.length, 4))
        >> IO.sleep(10.millis)
      }
  }

  test("Sessions: client reconnects after previous disconnects") {
    httpServerResource(simpleServer).use: server =>
      val port = server.address.getPort
      for
        result1 <- httpConnection(simpleClient, port).use: conn =>
          conn.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(1)))
        result2 <- httpConnection(simpleClient, port).use: conn =>
          conn.callTool("add", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(2)))
      yield
        assertEquals(result1.isError.getOrElse(false), false)
        assertEquals(result2.isError.getOrElse(false), false)
  }

  // --- Transport Equivalence Tests ---

  test("Transport: HTTP and WebSocket produce identical results for listTools") {
    val server = simpleServer
    (httpServerResource(server), wsServerResource(server)).tupled.use {
      case (httpServer, wsServer) =>
        val httpPort = httpServer.address.getPort
        val wsPort   = wsServer.address.getPort
        (httpConnection(simpleClient, httpPort), wsConnection(simpleClient, wsPort)).tupled.use {
          case (httpConn, wsConn) =>
            for
              httpTools <- httpConn.listAllTools
              wsTools   <- wsConn.listAllTools
            yield assertEquals(httpTools.map(_.name).sorted, wsTools.map(_.name).sorted)
        }
    }
  }

  test("Transport: HTTP and WebSocket produce identical results for tool call") {
    val server = simpleServer
    (httpServerResource(server), wsServerResource(server)).tupled.use {
      case (httpServer, wsServer) =>
        val args = Json.obj("a" -> Json.fromDouble(7.0).get, "b" -> Json.fromDouble(3.0).get)
        (
          httpConnection(simpleClient, httpServer.address.getPort),
          wsConnection(simpleClient, wsServer.address.getPort)
        ).tupled.use { case (httpConn, wsConn) =>
          for
            httpResult <- httpConn.callTool("add", args)
            wsResult   <- wsConn.callTool("add", args)
          yield
            assertEquals(httpResult.content, wsResult.content)
            assertEquals(httpResult.isError.getOrElse(false), wsResult.isError.getOrElse(false))
        }
    }
  }

  // ============================================================================
  // CATEGORY 3: EDGE CASES & RACE CONDITIONS
  // ============================================================================

  test("Edge: response for cancelled request is handled gracefully") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for
          fiber <- conn
            .callTool("slow_add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
            .start
          _ <- IO.sleep(50.millis)
          _ <- fiber.cancel
          // The server may still complete the request, but client should handle it gracefully
          // Try another request to ensure connection is still usable
          result <- conn.callTool("add", Json.obj("a" -> Json.fromInt(3), "b" -> Json.fromInt(4)))
        yield assertEquals(result.isError.getOrElse(false), false)
  }

  test("Edge: many concurrent requests complete correctly") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        val requests = (1 to 50).toList.map { i =>
          conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
        }
        requests.parSequence.map: results =>
          assertEquals(results.length, 50)
          assert(results.forall(!_.isError.getOrElse(false)))
  }

  test("Edge: echo tool preserves message content") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        val testMessage = "Hello, World! Special chars: @#$%^&*(){}[]|\\:\";<>?,./`~"
        for result <- conn.callTool("echo", Json.obj("message" -> Json.fromString(testMessage)))
        yield result.content.head match
          case TextContent(text, _, _) => assertEquals(text, testMessage)
          case _                       => fail("Expected text content")
  }

  // ============================================================================
  // CATEGORY 4: MULTI-SERVER TOPOLOGY
  // ============================================================================

  test("MultiServer: client connects to two independent servers simultaneously") {
    val server1 = Server.fromTools[IO](
      ServerInfo("server-1", "1.0.0"),
      Tools.single[IO](
        Tool("tool1", Some("Tool on server 1"), JsonSchema.obj(Map.empty, Nil))
      )(_ => IO.pure(ToolResult.text("from server 1")))
    )

    val server2 = Server.fromTools[IO](
      ServerInfo("server-2", "1.0.0"),
      Tools.single[IO](
        Tool("tool2", Some("Tool on server 2"), JsonSchema.obj(Map.empty, Nil))
      )(_ => IO.pure(ToolResult.text("from server 2")))
    )

    (httpServerResource(server1), httpServerResource(server2)).tupled.use { case (s1, s2) =>
      (
        httpConnection(simpleClient, s1.address.getPort),
        httpConnection(simpleClient, s2.address.getPort)
      ).tupled.use { case (conn1, conn2) =>
        for
          _ <- IO {
            assertEquals(conn1.serverInfo.name, "server-1")
            assertEquals(conn2.serverInfo.name, "server-2")
          }
          tools1 <- conn1.listAllTools
          tools2 <- conn2.listAllTools
          _ = assertEquals(tools1.map(_.name), List("tool1"))
          _ = assertEquals(tools2.map(_.name), List("tool2"))
          result1 <- conn1.callTool("tool1", Json.obj())
          result2 <- conn2.callTool("tool2", Json.obj())
        yield
          result1.content.head match
            case TextContent(t, _, _) => assertEquals(t, "from server 1")
            case _                    => fail("Expected text")
          result2.content.head match
            case TextContent(t, _, _) => assertEquals(t, "from server 2")
            case _                    => fail("Expected text")
      }
    }
  }

  test("MultiServer: failure on Server A doesn't affect requests to Server B") {
    TestServers.failingAfter[IO](simpleServer, failAfter = 0, "Always fails").flatMap {
      case (failingServer, _) =>
        val goodServer = simpleServer
        (httpServerResource(failingServer), httpServerResource(goodServer)).tupled.use {
          case (s1, s2) =>
            (
              httpConnection(simpleClient, s1.address.getPort),
              httpConnection(simpleClient, s2.address.getPort)
            ).tupled.use { case (conn1, conn2) =>
              for
                // Server 1 fails
                result1 <- conn1
                  .callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
                  .attempt
                _ = assert(result1.isLeft)
                // Server 2 still works
                result2 <- conn2
                  .callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
              yield assertEquals(result2.isError.getOrElse(false), false)
            }
        }
    }
  }

  // ============================================================================
  // CATEGORY 5: CHAOS TESTING
  // ============================================================================

  test("Chaos: jittered delays complete within reasonable time") {
    TestServers
      .jittered[IO](simpleServer, minDelay = 10.millis, maxDelay = 50.millis)
      .flatMap: jitteredServer =>
        httpServerResource(jitteredServer).use: server =>
          httpConnection(simpleClient, server.address.getPort).use: conn =>
            for results <- (1 to 5).toList.parTraverse { i =>
                conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
              }
            yield
              assertEquals(results.length, 5)
              assert(results.forall(!_.isError.getOrElse(false)))
  }

  // ============================================================================
  // CATEGORY 6: PERFORMANCE
  // ============================================================================

  test("Performance: 100 sequential tool calls complete") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        (1 to 100).toList
          .traverse { i =>
            conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
          }
          .map: results =>
            assertEquals(results.length, 100)
            assert(results.forall(!_.isError.getOrElse(false)))
  }

  test("Performance: 100 parallel tool calls complete") {
    httpServerResource(simpleServer).use: server =>
      resilientHttpConnection(simpleClient, server.address.getPort).use: conn =>
        (1 to 100).toList
          .parTraverse { i =>
            conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
          }
          .map: results =>
            assertEquals(results.length, 100)
            assert(results.forall(!_.isError.getOrElse(false)))
  }

  test("Performance: counting server tracks all calls accurately") {
    TestServers.counting[IO](simpleServer).flatMap { case (countingServer, getCounts) =>
      httpServerResource(countingServer).use: server =>
        httpConnection(simpleClient, server.address.getPort).use: conn =>
          for
            _      <- conn.listAllTools
            _      <- conn.listAllTools
            _      <- conn.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
            _      <- conn.listAllResources
            _      <- conn.readResource("file:///test.txt")
            _      <- conn.listAllPrompts
            _      <- conn.getPrompt("greeting", Map("name" -> "Test"))
            counts <- getCounts
          yield
            assertEquals(counts.listTools, 2)
            assertEquals(counts.toolCalls, 1)
            assertEquals(counts.listResources, 1)
            assertEquals(counts.resourceReads, 1)
            assertEquals(counts.listPrompts, 1)
            assertEquals(counts.promptGets, 1)
            assertEquals(counts.total, 7)
    }
  }

  // ============================================================================
  // HEALTH CHECK
  // ============================================================================

  test("Health: HTTP health endpoint returns ok") {
    httpServerResource(simpleServer).use: server =>
      val port = server.address.getPort
      EmberClientBuilder
        .default[IO]
        .build
        .use: httpClient =>
          val request = Request[IO](
            method = Method.GET,
            uri = Uri.unsafeFromString(s"http://localhost:$port/health")
          )
          httpClient
            .expect[Json](request)
            .map: response =>
              assertEquals(response.hcursor.get[String]("status"), Right("ok"))
  }

  test("Health: WebSocket health endpoint returns ok") {
    wsServerResource(simpleServer).use: server =>
      val port = server.address.getPort
      EmberClientBuilder
        .default[IO]
        .build
        .use: httpClient =>
          val request = Request[IO](
            method = Method.GET,
            uri = Uri.unsafeFromString(s"http://localhost:$port/health")
          )
          httpClient
            .expect[Json](request)
            .map: response =>
              assertEquals(response.hcursor.get[String]("status"), Right("ok"))
  }

  // ============================================================================
  // CATEGORY 7: ADDITIONAL PROTOCOL COMPLIANCE
  // ============================================================================

  test("Protocol: callToolIfSupported returns Some for supported tool") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for result <- conn.callToolIfSupported(
            ToolName("add"),
            Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))
          )
        yield
          assert(result.isDefined)
          assertEquals(result.get.isError.getOrElse(false), false)
  }

  test("Protocol: readResourceIfSupported returns Some for supported resource") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for result <- conn.readResourceIfSupported(ResourceUri("file:///test.txt"))
        yield
          assert(result.isDefined)
          assertEquals(result.get.uri, "file:///test.txt")
  }

  test("Protocol: getPromptIfSupported returns Some for supported prompt") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for result <- conn.getPromptIfSupported(PromptName("greeting"), Map("name" -> "Test"))
        yield assert(result.isDefined)
  }

  test("Protocol: capability checks are accurate") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        IO {
          assert(conn.supportsTools)
          assert(conn.supportsResources)
          assert(conn.supportsPrompts)
          // Task support depends on server configuration
        }
  }

  // ============================================================================
  // ADDITIONAL EDGE CASES
  // ============================================================================

  test("Edge: very long string argument is handled correctly") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        val longString = "x" * 10000
        for result <- conn.callTool("echo", Json.obj("message" -> Json.fromString(longString)))
        yield result.content.head match
          case TextContent(text, _, _) => assertEquals(text.length, 10000)
          case _                       => fail("Expected text content")
  }

  test("Edge: unicode characters in arguments are preserved") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        val unicodeMessage = "Hello 世界! 🌍🎉 αβγδ ℃℉"
        for result <- conn.callTool("echo", Json.obj("message" -> Json.fromString(unicodeMessage)))
        yield result.content.head match
          case TextContent(text, _, _) => assertEquals(text, unicodeMessage)
          case _                       => fail("Expected text content")
  }

  test("Edge: empty string argument is handled") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for result <- conn.callTool("echo", Json.obj("message" -> Json.fromString("")))
        yield result.content.head match
          case TextContent(text, _, _) => assertEquals(text, "")
          case _                       => fail("Expected text content")
  }

  test("Edge: zero and negative numbers work correctly") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for result <- conn.callTool(
            "add",
            Json.obj("a" -> Json.fromDouble(-5.5).get, "b" -> Json.fromDouble(0.0).get)
          )
        yield result.content.head match
          case TextContent(text, _, _) => assertEquals(text, "-5.5")
          case _                       => fail("Expected text content")
  }

  test("Edge: floating point precision is maintained") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for result <- conn.callTool(
            "add",
            Json.obj("a" -> Json.fromDouble(0.1).get, "b" -> Json.fromDouble(0.2).get)
          )
        yield result.content.head match
          case TextContent(text, _, _) =>
            val value = text.toDouble
            assert(value > 0.29 && value < 0.31) // Account for floating point
          case _ => fail("Expected text content")
  }

  // ============================================================================
  // MULTIPLE OPERATIONS IN SEQUENCE
  // ============================================================================

  test("Sequence: full workflow - list, call, read, prompt") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for
          // List available capabilities
          tools     <- conn.listAllTools
          resources <- conn.listAllResources
          prompts   <- conn.listAllPrompts
          templates <- conn.listAllResourceTemplates
          // Use each capability
          toolResult <- conn.callTool(
            "add",
            Json.obj("a" -> Json.fromInt(5), "b" -> Json.fromInt(5))
          )
          resourceContent <- conn.readResource("file:///test.txt")
          promptResult    <- conn.getPrompt("greeting", Map("name" -> "Workflow"))
        yield
          assertEquals(tools.length, 4)
          assertEquals(resources.length, 2)
          assertEquals(prompts.length, 1)
          assertEquals(templates.length, 1)
          assertEquals(toolResult.isError.getOrElse(false), false)
          assertEquals(resourceContent.text, Some("Hello, World!"))
          assertEquals(promptResult.messages.length, 1)
  }

  test("Sequence: repeated operations on same connection") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for
          // Repeat the same operation multiple times
          r1 <- conn.listAllTools
          r2 <- conn.listAllTools
          r3 <- conn.listAllTools
          r4 <- conn.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(1)))
          r5 <- conn.callTool("add", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(2)))
          r6 <- conn.callTool("add", Json.obj("a" -> Json.fromInt(3), "b" -> Json.fromInt(3)))
        yield
          assertEquals(r1.length, r2.length)
          assertEquals(r2.length, r3.length)
          assert(
            !r4.isError.getOrElse(false) && !r5.isError.getOrElse(false) && !r6.isError.getOrElse(
              false
            )
          )
  }

  // ============================================================================
  // WEBSOCKET SPECIFIC TESTS
  // ============================================================================

  test("WebSocket: multiple concurrent clients") {
    wsServerResource(simpleServer).use: server =>
      val port = server.address.getPort
      (
        wsConnection(simpleClient, port),
        wsConnection(simpleClient, port),
        wsConnection(simpleClient, port)
      ).tupled
        .use { case (conn1, conn2, conn3) =>
          for
            t1 <- conn1.listAllTools
            t2 <- conn2.listAllTools
            t3 <- conn3.listAllTools
          yield
            assertEquals(t1.length, 4)
            assertEquals(t2.length, 4)
            assertEquals(t3.length, 4)
        }
  }

  test("WebSocket: rapid tool calls") {
    wsServerResource(simpleServer).use: server =>
      wsConnection(simpleClient, server.address.getPort).use: conn =>
        (1 to 20).toList
          .traverse { i =>
            conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
          }
          .map: results =>
            assertEquals(results.length, 20)
            assert(results.forall(!_.isError.getOrElse(false)))
  }

  // ============================================================================
  // LARGE PAYLOADS
  // ============================================================================

  test("Performance: large number of concurrent connections") {
    httpServerResource(simpleServer).use: server =>
      val port        = server.address.getPort
      val connections = (1 to 10).toList.map(_ => httpConnection(simpleClient, port))
      connections.sequence.use: conns =>
        conns
          .parTraverse: conn =>
            conn.listAllTools.map(tools => assertEquals(tools.length, 4))
          .void
  }

  test("Performance: mixed operations in parallel") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        val operations = List(
          conn.listAllTools,
          conn.listAllResources,
          conn.listAllPrompts,
          conn
            .callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(1)))
            .map(_ => List.empty[Tool]),
          conn.readResource("file:///test.txt").map(_ => List.empty[Tool]),
          conn.ping.map(_ => List.empty[Tool])
        )
        operations.parSequence.map: results =>
          assertEquals(results.length, 6)
  }

  // ============================================================================
  // CATEGORY 8: RIGOROUS PERFORMANCE TESTS
  // ============================================================================

  test("Performance: parallel is faster than sequential for slow operations") {
    httpServerResource(simpleServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        val args = Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))
        for
          seqStart <- IO.monotonic
          _        <- (1 to 10).toList.traverse(_ => conn.callTool("slow_add", args))
          seqEnd   <- IO.monotonic
          seqTime = seqEnd - seqStart
          parStart <- IO.monotonic
          _        <- (1 to 10).toList.parTraverse(_ => conn.callTool("slow_add", args))
          parEnd   <- IO.monotonic
          parTime = parEnd - parStart
        yield assert(
          parTime < seqTime / 2,
          s"Parallel time ($parTime) should be less than half of sequential time ($seqTime)"
        )
  }

  test("Performance: sustained parallel load loses no requests") {
    TestServers.counting[IO](simpleServer).flatMap { case (countingServer, getCounts) =>
      httpServerResource(countingServer).use: server =>
        resilientHttpConnection(simpleClient, server.address.getPort).use: conn =>
          for
            results <- (1 to 200).toList.parTraverse { i =>
              conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
            }
            counts <- getCounts
          yield
            assertEquals(results.length, 200)
            assert(
              results.forall(!_.isError.getOrElse(false)),
              "All 200 results should be non-error"
            )
            assertEquals(counts.toolCalls, 200)
    }
  }

  test("Performance: multi-connection parallel throughput") {
    httpServerResource(simpleServer).use: server =>
      val port        = server.address.getPort
      val connections = (1 to 5).toList.map(_ => resilientHttpConnection(simpleClient, port))
      connections.sequence.use: conns =>
        conns
          .parTraverse: conn =>
            (1 to 20).toList.parTraverse { i =>
              conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
            }
          .map: allResults =>
            val flat = allResults.flatten
            assertEquals(flat.length, 100)
            assert(flat.forall(!_.isError.getOrElse(false)), "All 100 results should succeed")
            // Verify correct values
            flat.foreach: result =>
              result.content.head match
                case TextContent(text, _, _) =>
                  val value = text.toDouble
                  assert(value > 0, s"Expected positive result, got $value")
                case _ => fail("Expected text content")
  }

  // ============================================================================
  // HTTP4S MIDDLEWARE EXAMPLE
  // Demonstrates using http4s Retry + Timeout middleware for resilience,
  // instead of a built-in ResilienceConfig.
  // ============================================================================

  test("Middleware: http4s Retry middleware handles transient server failures") {
    import org.http4s.client.middleware.{Retry, RetryPolicy as Http4sRetryPolicy}

    TestServers.failingAfter[IO](simpleServer, failAfter = 2, "Transient error").flatMap {
      case (failingServer, _) =>
        httpServerResource(failingServer).use: server =>
          EmberClientBuilder
            .default[IO]
            .build
            .use: rawHttpClient =>
              // Compose http4s Retry middleware on the raw Client[F]
              // Use a custom retriable that retries on connection-level exceptions (MCP uses POST)
              val retryPolicy = Http4sRetryPolicy[IO](
                backoff = Http4sRetryPolicy.exponentialBackoff(maxWait = 1.second, maxRetry = 5),
                retriable = (_, result) =>
                  result match
                    case Left(_: java.io.IOException) => true
                    case _                            => false
              )
              val resilientClient = Retry(retryPolicy)(rawHttpClient)

              HttpClientTransport
                .connect[IO](
                  simpleClient,
                  HttpTransportConfig(s"http://localhost:${server.address.getPort}/mcp"),
                  resilientClient
                )
                .use: conn =>
                  for result <- conn
                      .callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
                  yield assertEquals(result.isError.getOrElse(false), false)
    }
  }

  // ============================================================================
  // CATEGORY 7: BIDIRECTIONAL (server-initiated requests back to the client)
  // ============================================================================

  /** A server whose tool asks the client for an LLM completion via sampling. */
  def samplingServer: Server[IO] =
    import mcp4s.server.dsl.*
    case class AskArgs(question: String) derives Schema
    val ask =
      Tool("ask").withDescription("Ask the LLM via sampling").input[AskArgs].handleWith[IO] {
        (args, ctx) =>
          ctx.sampling
            .createMessage(
              CreateMessageParams(
                messages = List(SamplingMessage(Role.User, SamplingTextContent(args.question))),
                maxTokens = 100
              )
            )
            .map { result =>
              result.content match
                case SamplingTextContent(text) => ok(text)
                case _                         => error("unexpected content")
            }
      }
    Server.fromTools[IO](ServerInfo("sampling-server", "1.0.0"), ask)

  /** A server whose tool elicits structured input from the client. */
  def elicitingServer: Server[IO] =
    import mcp4s.server.dsl.*
    val confirm =
      Tool("confirm").withDescription("Confirm via elicitation").handleWith[IO] { (_, ctx) =>
        ctx.elicitation
          .elicit(ElicitFormParams("Please confirm", JsonSchema.empty))
          .map(result => ok(s"action=${result.action}"))
      }
    Server.fromTools[IO](ServerInfo("eliciting-server", "1.0.0"), confirm)

  test("Bidirectional: HTTP client answers a server-initiated sampling request") {
    httpServerResource(samplingServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for result <- conn.callTool("ask", Json.obj("question" -> Json.fromString("2+2?")))
        yield
          assertEquals(result.isError.getOrElse(false), false)
          assertEquals(result.textContent, "Echo: 2+2?")
  }

  test("Bidirectional: WebSocket client answers a server-initiated sampling request") {
    wsServerResource(samplingServer).use: server =>
      wsConnection(simpleClient, server.address.getPort).use: conn =>
        for result <- conn.callTool("ask", Json.obj("question" -> Json.fromString("2+2?")))
        yield
          assertEquals(result.isError.getOrElse(false), false)
          assertEquals(result.textContent, "Echo: 2+2?")
  }

  test("Bidirectional: HTTP client answers a server-initiated elicitation request") {
    httpServerResource(elicitingServer).use: server =>
      httpConnection(simpleClient, server.address.getPort).use: conn =>
        for result <- conn.callTool("confirm", Json.obj())
        yield
          assertEquals(result.isError.getOrElse(false), false)
          assert(result.textContent.toLowerCase.contains("accept"))
  }

  test("Bidirectional: sampling raises SamplingNotSupported when the client lacks the capability") {
    val bareClient = McpClient.from[IO](ClientInfo("bare", "1.0.0"))
    httpServerResource(samplingServer).use: server =>
      httpConnection(bareClient, server.address.getPort).use: conn =>
        for result <- conn.callTool("ask", Json.obj("question" -> Json.fromString("x"))).attempt
        yield assert(result.isLeft || result.exists(_.isError.getOrElse(false)))
  }
