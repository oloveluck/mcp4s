package mcp4s.examples

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.port
import io.circe.Json
import mcp4s.client.*
import mcp4s.client.resilient.*
import mcp4s.client.retry.*
import mcp4s.client.transport.*
import mcp4s.examples.fixtures.*
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
  * - Protocol compliance (lifecycle, tools, resources, prompts, bidirectional)
  * - Network resilience (timeouts, retries, circuit breaker)
  * - Session & connectivity (multiple clients, transport equivalence)
  * - Edge cases & race conditions
  * - Multi-server topology
  * - Chaos testing
  * - Performance
  */
class NetworkIntegrationSpec extends CatsEffectSuite:

  given Tracer[IO] = Tracer.noop[IO]

  // === Test Server Setup ===

  def simpleServer: McpServer[IO] = TestServers.simple[IO]

  def simpleClient: McpClient[IO] = DeterministicClients.simple[IO]

  def httpServerResource(server: McpServer[IO]): Resource[IO, org.http4s.server.Server] =
    HttpTransport.serve[IO](server, HttpConfig(port = port"0"))

  def wsServerResource(server: McpServer[IO]): Resource[IO, org.http4s.server.Server] =
    WebSocketTransport.serve[IO](server, WebSocketConfig(port = port"0"))

  def httpConnection(client: McpClient[IO], port: Int): Resource[IO, McpConnection[IO]] =
    EmberClientBuilder.default[IO].build.flatMap { httpClient =>
      HttpClientTransport.connect[IO](client, HttpClientConfig(s"http://localhost:$port"), httpClient)
    }

  def wsConnection(client: McpClient[IO], port: Int): Resource[IO, McpConnection[IO]] =
    WebSocketClientTransport.connect[IO](client, WebSocketClientConfig(s"ws://localhost:$port"))

  // ============================================================================
  // CATEGORY 1: PROTOCOL COMPLIANCE
  // ============================================================================

  // --- Lifecycle Tests ---

  test("Protocol: initialize returns server info and capabilities") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        IO {
          assertEquals(conn.serverInfo.name, "test-server")
          assertEquals(conn.serverInfo.version, "1.0.0")
          assert(conn.serverCapabilities.tools.isDefined)
          assert(conn.serverCapabilities.resources.isDefined)
          assert(conn.serverCapabilities.prompts.isDefined)
        }
      }
    }
  }

  test("Protocol: ping succeeds after initialization") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        conn.ping.as(())
      }
    }
  }

  test("Protocol: shutdown completes gracefully") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        conn.shutdown.as(())
      }
    }
  }

  test("Protocol: multiple pings succeed") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        (1 to 5).toList.traverse_(_ => conn.ping)
      }
    }
  }

  // --- Tools Tests ---

  test("Protocol: tools/list returns all registered tools") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          tools <- conn.listTools
        yield
          assertEquals(tools.length, 4)
          assert(tools.exists(_.name == "add"))
          assert(tools.exists(_.name == "slow_add"))
          assert(tools.exists(_.name == "echo"))
          assert(tools.exists(_.name == "fail"))
      }
    }
  }

  test("Protocol: tools/call with valid arguments succeeds") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          result <- conn.callTool("add", Json.obj("a" -> Json.fromDouble(5.0).get, "b" -> Json.fromDouble(3.0).get))
        yield
          assertEquals(result.isError.getOrElse(false), false)
          result.content.head match
            case TextContent(text, _, _) => assertEquals(text, "8.0")
            case _ => fail("Expected text content")
      }
    }
  }

  test("Protocol: tools/call returns error for unknown tool") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        conn.callTool("nonexistent", Json.obj()).attempt.map { result =>
          assert(result.isLeft)
        }
      }
    }
  }

  test("Protocol: tools/call returns error for tool that throws") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        conn.callTool("fail", Json.obj()).attempt.map { result =>
          assert(result.isLeft)
        }
      }
    }
  }

  test("Protocol: tools/call with invalid arguments returns error") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        conn.callTool("add", Json.obj("x" -> Json.fromInt(1))).attempt.map { result =>
          assert(result.isLeft)
        }
      }
    }
  }

  test("Protocol: multiple concurrent tool calls succeed") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        val calls = (1 to 10).toList.map { i =>
          conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
        }
        calls.parSequence.map { results =>
          assertEquals(results.length, 10)
          assert(results.forall(!_.isError.getOrElse(false)))
        }
      }
    }
  }

  // --- Resources Tests ---

  test("Protocol: resources/list returns registered resources") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          resources <- conn.listResources
        yield
          assertEquals(resources.length, 2)
          assert(resources.exists(_.uri == "file:///test.txt"))
          assert(resources.exists(_.uri == "file:///binary.bin"))
      }
    }
  }

  test("Protocol: resources/read returns text content") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          content <- conn.readResource("file:///test.txt")
        yield
          assertEquals(content.uri, "file:///test.txt")
          assertEquals(content.text, Some("Hello, World!"))
      }
    }
  }

  test("Protocol: resources/read returns binary content (blob)") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          content <- conn.readResource("file:///binary.bin")
        yield
          assertEquals(content.uri, "file:///binary.bin")
          assert(content.blob.isDefined)
          assertEquals(content.mimeType, Some("application/octet-stream"))
      }
    }
  }

  test("Protocol: resources/read returns error for unknown resource") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        conn.readResource("file:///nonexistent.txt").attempt.map { result =>
          assert(result.isLeft)
        }
      }
    }
  }

  test("Protocol: resources/templates/list returns templates") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          templates <- conn.listResourceTemplates
        yield
          assertEquals(templates.length, 1)
          assertEquals(templates.head.uriTemplate, "file:///docs/{name}")
      }
    }
  }

  // --- Prompts Tests ---

  test("Protocol: prompts/list returns registered prompts") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          prompts <- conn.listPrompts
        yield
          assertEquals(prompts.length, 1)
          assertEquals(prompts.head.name, "greeting")
      }
    }
  }

  test("Protocol: prompts/get returns prompt with arguments") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          result <- conn.getPrompt("greeting", Map("name" -> "Alice"))
        yield
          assertEquals(result.description, Some("A friendly greeting"))
          assertEquals(result.messages.length, 1)
          result.messages.head.content match
            case TextContent(text, _, _) => assertEquals(text, "Hello, Alice!")
            case _ => fail("Expected text content")
      }
    }
  }

  test("Protocol: prompts/get returns error for unknown prompt") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          // Do a successful operation first to ensure connection is established
          _ <- conn.listPrompts
          result <- conn.getPrompt("nonexistent", Map.empty[String, String]).attempt
        yield
          assert(result.isLeft)
      }
    }
  }

  // ============================================================================
  // CATEGORY 2: NETWORK RESILIENCE
  // ============================================================================

  // --- Timeout Tests ---

  test("Resilience: request timeout fires for slow operations") {
    val delayingServer = TestServers.delaying[IO](simpleServer, 500.millis)
    httpServerResource(delayingServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          resilient <- conn.withTimeout(100.millis)
          result <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))).attempt
        yield
          assert(result.isLeft)
          assert(result.left.exists(_.isInstanceOf[java.util.concurrent.TimeoutException]))
      }
    }
  }

  test("Resilience: request succeeds within timeout") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          resilient <- conn.withTimeout(5.seconds)
          result <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
        yield
          assertEquals(result.isError.getOrElse(false), false)
      }
    }
  }

  test("Resilience: tool execution timeout for slow_add") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          resilient <- conn.withTimeout(100.millis)
          result <- resilient.callTool("slow_add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))).attempt
        yield
          assert(result.isLeft)
      }
    }
  }

  // --- Retry Tests ---

  test("Resilience: retry succeeds after transient failure") {
    TestServers.failingAfter[IO](simpleServer, failAfter = 2, "Transient error").flatMap { case (failingServer, _) =>
      httpServerResource(failingServer).use { server =>
        httpConnection(simpleClient, server.address.getPort).use { conn =>
          for
            resilient <- conn.withRetry(RetryPolicy.fixedDelay(maxRetries = 3, delay = 10.millis, retryOn = _ => true))
            result <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
          yield
            assertEquals(result.isError.getOrElse(false), false)
        }
      }
    }
  }

  test("Resilience: retry gives up after max retries") {
    TestServers.failingAfter[IO](simpleServer, failAfter = 0, "Always fails").flatMap { case (failingServer, _) =>
      httpServerResource(failingServer).use { server =>
        httpConnection(simpleClient, server.address.getPort).use { conn =>
          for
            resilient <- conn.withRetry(RetryPolicy.fixedDelay(maxRetries = 2, delay = 10.millis, retryOn = _ => true))
            result <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))).attempt
          yield
            assert(result.isLeft)
        }
      }
    }
  }

  test("Resilience: exponential backoff increases delay") {
    val policy = RetryPolicy.exponentialBackoff(
      maxRetries = 3,
      baseDelay = 10.millis,
      maxDelay = 1.second,
      jitterFactor = 0.0
    )
    assertEquals(policy.delay(1).toMillis, 10L)
    assertEquals(policy.delay(2).toMillis, 20L)
    assertEquals(policy.delay(3).toMillis, 40L)
  }

  test("Resilience: no retry policy means single attempt") {
    TestServers.failingAfter[IO](simpleServer, failAfter = 0, "Fails").flatMap { case (failingServer, getCount) =>
      httpServerResource(failingServer).use { server =>
        httpConnection(simpleClient, server.address.getPort).use { conn =>
          for
            resilient <- conn.withRetry(RetryPolicy.noRetry)
            _ <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))).attempt
            count <- getCount
          yield
            assertEquals(count, 1)
        }
      }
    }
  }

  // --- Circuit Breaker Tests ---

  test("Resilience: circuit breaker opens after failure threshold") {
    val cbConfig = CircuitBreakerConfig(failureThreshold = 2, resetTimeout = 1.minute)
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          resilient <- conn.withCircuitBreaker(cbConfig)
          cb = resilient.circuitBreaker.get
          // Cause failures
          _ <- resilient.callTool("fail", Json.obj()).attempt
          _ <- resilient.callTool("fail", Json.obj()).attempt
          state <- cb.state
        yield
          assertEquals(state, CircuitState.Open)
      }
    }
  }

  test("Resilience: circuit breaker fails fast when open") {
    val cbConfig = CircuitBreakerConfig(failureThreshold = 1, resetTimeout = 1.minute)
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          resilient <- conn.withCircuitBreaker(cbConfig)
          cb = resilient.circuitBreaker.get
          // Open the circuit
          _ <- resilient.callTool("fail", Json.obj()).attempt
          state <- cb.state
          _ = assertEquals(state, CircuitState.Open)
          // Now operations should fail fast
          executed <- Ref.of[IO, Boolean](false)
          result <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))).attempt
        yield
          assert(result.isLeft)
          assert(result.left.exists(_.isInstanceOf[CircuitBreakerOpenException]))
      }
    }
  }

  test("Resilience: circuit breaker transitions to half-open after reset timeout") {
    val cbConfig = CircuitBreakerConfig(failureThreshold = 1, resetTimeout = 50.millis)
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          resilient <- conn.withCircuitBreaker(cbConfig)
          cb = resilient.circuitBreaker.get
          // Open the circuit
          _ <- resilient.callTool("fail", Json.obj()).attempt
          state1 <- cb.state
          _ = assertEquals(state1, CircuitState.Open)
          // Wait for reset timeout
          _ <- IO.sleep(100.millis)
          // Next call should be allowed (half-open)
          result <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
        yield
          assertEquals(result.isError.getOrElse(false), false)
      }
    }
  }

  test("Resilience: circuit breaker closes after success threshold in half-open") {
    val cbConfig = CircuitBreakerConfig(
      failureThreshold = 1,
      resetTimeout = 50.millis,
      successThreshold = 2
    )
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          resilient <- conn.withCircuitBreaker(cbConfig)
          cb = resilient.circuitBreaker.get
          // Open the circuit
          _ <- resilient.callTool("fail", Json.obj()).attempt
          _ <- IO.sleep(100.millis)
          // First success
          _ <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
          state1 <- cb.state
          _ = assertEquals(state1, CircuitState.HalfOpen)
          // Second success - should close
          _ <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3)))
          state2 <- cb.state
        yield
          assertEquals(state2, CircuitState.Closed)
      }
    }
  }

  test("Resilience: circuit breaker stats are accurate") {
    val cbConfig = CircuitBreakerConfig(failureThreshold = 5)
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          resilient <- conn.withCircuitBreaker(cbConfig)
          _ <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
          _ <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3)))
          _ <- resilient.callTool("fail", Json.obj()).attempt
          stats <- resilient.circuitBreakerStats
        yield
          assert(stats.isDefined)
          assertEquals(stats.get.totalRequests, 3L)
          assertEquals(stats.get.failures, 1)
      }
    }
  }

  // ============================================================================
  // CATEGORY 3: SESSION & CONNECTIVITY
  // ============================================================================

  test("Sessions: multiple concurrent clients have isolated state") {
    httpServerResource(simpleServer).use { server =>
      val port = server.address.getPort
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
          tools1 <- conn1.listTools
          tools2 <- conn2.listTools
          tools3 <- conn3.listTools
        yield
          assertEquals(tools1.length, 4)
          assertEquals(tools2.length, 4)
          assertEquals(tools3.length, 4)
      }
    }
  }

  test("Sessions: concurrent requests from different clients don't interfere") {
    httpServerResource(simpleServer).use { server =>
      val port = server.address.getPort
      (httpConnection(simpleClient, port), httpConnection(simpleClient, port)).tupled.use { case (conn1, conn2) =>
        val requests = List(
          conn1.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(1))),
          conn2.callTool("add", Json.obj("a" -> Json.fromInt(10), "b" -> Json.fromInt(10))),
          conn1.callTool("add", Json.obj("a" -> Json.fromInt(100), "b" -> Json.fromInt(100))),
          conn2.callTool("add", Json.obj("a" -> Json.fromInt(1000), "b" -> Json.fromInt(1000)))
        )
        for
          results <- requests.parSequence
        yield
          assertEquals(results.length, 4)
          val texts = results.flatMap(_.content.collect { case TextContent(t, _, _) => t })
          assert(texts.contains("2.0"))
          assert(texts.contains("20.0"))
          assert(texts.contains("200.0"))
          assert(texts.contains("2000.0"))
      }
    }
  }

  test("Sessions: rapid connect/disconnect cycle") {
    httpServerResource(simpleServer).use { server =>
      val port = server.address.getPort
      (1 to 5).toList.traverse_ { _ =>
        httpConnection(simpleClient, port).use { conn =>
          conn.listTools.map(tools => assertEquals(tools.length, 4))
        } >> IO.sleep(10.millis)
      }
    }
  }

  test("Sessions: client reconnects after previous disconnects") {
    httpServerResource(simpleServer).use { server =>
      val port = server.address.getPort
      for
        result1 <- httpConnection(simpleClient, port).use { conn =>
          conn.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(1)))
        }
        result2 <- httpConnection(simpleClient, port).use { conn =>
          conn.callTool("add", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(2)))
        }
      yield
        assertEquals(result1.isError.getOrElse(false), false)
        assertEquals(result2.isError.getOrElse(false), false)
    }
  }

  // --- Transport Equivalence Tests ---

  test("Transport: HTTP and WebSocket produce identical results for listTools") {
    val server = simpleServer
    (httpServerResource(server), wsServerResource(server)).tupled.use { case (httpServer, wsServer) =>
      val httpPort = httpServer.address.getPort
      val wsPort = wsServer.address.getPort
      (httpConnection(simpleClient, httpPort), wsConnection(simpleClient, wsPort)).tupled.use { case (httpConn, wsConn) =>
        for
          httpTools <- httpConn.listTools
          wsTools <- wsConn.listTools
        yield
          assertEquals(httpTools.map(_.name).sorted, wsTools.map(_.name).sorted)
      }
    }
  }

  test("Transport: HTTP and WebSocket produce identical results for tool call") {
    val server = simpleServer
    (httpServerResource(server), wsServerResource(server)).tupled.use { case (httpServer, wsServer) =>
      val args = Json.obj("a" -> Json.fromDouble(7.0).get, "b" -> Json.fromDouble(3.0).get)
      (
        httpConnection(simpleClient, httpServer.address.getPort),
        wsConnection(simpleClient, wsServer.address.getPort)
      ).tupled.use { case (httpConn, wsConn) =>
        for
          httpResult <- httpConn.callTool("add", args)
          wsResult <- wsConn.callTool("add", args)
        yield
          assertEquals(httpResult.content, wsResult.content)
          assertEquals(httpResult.isError.getOrElse(false), wsResult.isError.getOrElse(false))
      }
    }
  }

  // ============================================================================
  // CATEGORY 4: EDGE CASES & RACE CONDITIONS
  // ============================================================================

  test("Edge: response for cancelled request is handled gracefully") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          fiber <- conn.callTool("slow_add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))).start
          _ <- IO.sleep(50.millis)
          _ <- fiber.cancel
          // The server may still complete the request, but client should handle it gracefully
          // Try another request to ensure connection is still usable
          result <- conn.callTool("add", Json.obj("a" -> Json.fromInt(3), "b" -> Json.fromInt(4)))
        yield
          assertEquals(result.isError.getOrElse(false), false)
      }
    }
  }

  test("Edge: many concurrent requests complete correctly") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        val requests = (1 to 50).toList.map { i =>
          conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
        }
        requests.parSequence.map { results =>
          assertEquals(results.length, 50)
          assert(results.forall(!_.isError.getOrElse(false)))
        }
      }
    }
  }

  test("Edge: echo tool preserves message content") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        val testMessage = "Hello, World! Special chars: @#$%^&*(){}[]|\\:\";<>?,./`~"
        for
          result <- conn.callTool("echo", Json.obj("message" -> Json.fromString(testMessage)))
        yield
          result.content.head match
            case TextContent(text, _, _) => assertEquals(text, testMessage)
            case _ => fail("Expected text content")
      }
    }
  }

  // ============================================================================
  // CATEGORY 5: MULTI-SERVER TOPOLOGY
  // ============================================================================

  test("MultiServer: client connects to two independent servers simultaneously") {
    val server1 = McpServer.builder[IO]
      .withInfo(ServerInfo("server-1", "1.0.0"))
      .withTool(
        Tool(
          name = "tool1",
          description = Some("Tool on server 1"),
          inputSchema = JsonSchema.obj(Map.empty, Nil)
        ),
        _ => IO.pure(ToolResult.text("from server 1"))
      )
      .build

    val server2 = McpServer.builder[IO]
      .withInfo(ServerInfo("server-2", "1.0.0"))
      .withTool(
        Tool(
          name = "tool2",
          description = Some("Tool on server 2"),
          inputSchema = JsonSchema.obj(Map.empty, Nil)
        ),
        _ => IO.pure(ToolResult.text("from server 2"))
      )
      .build

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
          tools1 <- conn1.listTools
          tools2 <- conn2.listTools
          _ = assertEquals(tools1.map(_.name), List("tool1"))
          _ = assertEquals(tools2.map(_.name), List("tool2"))
          result1 <- conn1.callTool("tool1", Json.obj())
          result2 <- conn2.callTool("tool2", Json.obj())
        yield
          result1.content.head match
            case TextContent(t, _, _) => assertEquals(t, "from server 1")
            case _ => fail("Expected text")
          result2.content.head match
            case TextContent(t, _, _) => assertEquals(t, "from server 2")
            case _ => fail("Expected text")
      }
    }
  }

  test("MultiServer: failure on Server A doesn't affect requests to Server B") {
    TestServers.failingAfter[IO](simpleServer, failAfter = 0, "Always fails").flatMap { case (failingServer, _) =>
      val goodServer = simpleServer
      (httpServerResource(failingServer), httpServerResource(goodServer)).tupled.use { case (s1, s2) =>
        (
          httpConnection(simpleClient, s1.address.getPort),
          httpConnection(simpleClient, s2.address.getPort)
        ).tupled.use { case (conn1, conn2) =>
          for
            // Server 1 fails
            result1 <- conn1.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))).attempt
            _ = assert(result1.isLeft)
            // Server 2 still works
            result2 <- conn2.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
          yield
            assertEquals(result2.isError.getOrElse(false), false)
        }
      }
    }
  }

  // ============================================================================
  // CATEGORY 6: CHAOS TESTING
  // ============================================================================

  test("Chaos: random failures with high retry count eventually succeed") {
    // 30% failure rate, but with 10 retries we should succeed eventually
    TestServers.chaotic[IO](simpleServer, failureRate = 0.3, seed = Some(42L)).flatMap { chaoticServer =>
      httpServerResource(chaoticServer).use { server =>
        httpConnection(simpleClient, server.address.getPort).use { conn =>
          for
            resilient <- conn.withRetry(RetryPolicy.fixedDelay(maxRetries = 10, delay = 10.millis, retryOn = _ => true))
            result <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
          yield
            assertEquals(result.isError.getOrElse(false), false)
        }
      }
    }
  }

  test("Chaos: jittered delays complete within reasonable time") {
    TestServers.jittered[IO](simpleServer, minDelay = 10.millis, maxDelay = 50.millis).flatMap { jitteredServer =>
      httpServerResource(jitteredServer).use { server =>
        httpConnection(simpleClient, server.address.getPort).use { conn =>
          for
            resilient <- conn.withTimeout(5.seconds)
            results <- (1 to 5).toList.parTraverse { i =>
              resilient.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
            }
          yield
            assertEquals(results.length, 5)
            assert(results.forall(!_.isError.getOrElse(false)))
        }
      }
    }
  }

  // ============================================================================
  // CATEGORY 7: PERFORMANCE
  // ============================================================================

  test("Performance: 100 sequential tool calls complete") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        (1 to 100).toList.traverse { i =>
          conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
        }.map { results =>
          assertEquals(results.length, 100)
          assert(results.forall(!_.isError.getOrElse(false)))
        }
      }
    }
  }

  test("Performance: 100 parallel tool calls complete") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        (1 to 100).toList.parTraverse { i =>
          conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
        }.map { results =>
          assertEquals(results.length, 100)
          assert(results.forall(!_.isError.getOrElse(false)))
        }
      }
    }
  }

  test("Performance: counting server tracks all calls accurately") {
    TestServers.counting[IO](simpleServer).flatMap { case (countingServer, getCounts) =>
      httpServerResource(countingServer).use { server =>
        httpConnection(simpleClient, server.address.getPort).use { conn =>
          for
            _ <- conn.listTools
            _ <- conn.listTools
            _ <- conn.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
            _ <- conn.listResources
            _ <- conn.readResource("file:///test.txt")
            _ <- conn.listPrompts
            _ <- conn.getPrompt("greeting", Map("name" -> "Test"))
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
    }
  }

  // ============================================================================
  // HEALTH CHECK
  // ============================================================================

  test("Health: HTTP health endpoint returns ok") {
    httpServerResource(simpleServer).use { server =>
      val port = server.address.getPort
      EmberClientBuilder.default[IO].build.use { httpClient =>
        val request = Request[IO](
          method = Method.GET,
          uri = Uri.unsafeFromString(s"http://localhost:$port/health")
        )
        httpClient.expect[Json](request).map { response =>
          assertEquals(response.hcursor.get[String]("status"), Right("ok"))
        }
      }
    }
  }

  test("Health: WebSocket health endpoint returns ok") {
    wsServerResource(simpleServer).use { server =>
      val port = server.address.getPort
      EmberClientBuilder.default[IO].build.use { httpClient =>
        val request = Request[IO](
          method = Method.GET,
          uri = Uri.unsafeFromString(s"http://localhost:$port/health")
        )
        httpClient.expect[Json](request).map { response =>
          assertEquals(response.hcursor.get[String]("status"), Right("ok"))
        }
      }
    }
  }

  // ============================================================================
  // CATEGORY 8: ADDITIONAL PROTOCOL COMPLIANCE
  // ============================================================================

  test("Protocol: callToolIfSupported returns Some for supported tool") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          result <- conn.callToolIfSupported(ToolName("add"), Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
        yield
          assert(result.isDefined)
          assertEquals(result.get.isError.getOrElse(false), false)
      }
    }
  }

  test("Protocol: readResourceIfSupported returns Some for supported resource") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          result <- conn.readResourceIfSupported(ResourceUri("file:///test.txt"))
        yield
          assert(result.isDefined)
          assertEquals(result.get.uri, "file:///test.txt")
      }
    }
  }

  test("Protocol: getPromptIfSupported returns Some for supported prompt") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          result <- conn.getPromptIfSupported(PromptName("greeting"), Map("name" -> "Test"))
        yield
          assert(result.isDefined)
      }
    }
  }

  test("Protocol: capability checks are accurate") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        IO {
          assert(conn.supportsTools)
          assert(conn.supportsResources)
          assert(conn.supportsPrompts)
          // Task support depends on server configuration
        }
      }
    }
  }

  // ============================================================================
  // ADDITIONAL RESILIENCE TESTS
  // ============================================================================

  test("Resilience: full resilience config with retry, timeout, and circuit breaker") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        val config = ResilienceConfig.builder
          .withRetry(RetryPolicy.exponentialBackoff(maxRetries = 3))
          .withTimeout(5.seconds)
          .withCircuitBreaker(CircuitBreakerConfig(failureThreshold = 5))
          .build
        for
          resilient <- conn.withResilience(config)
          result <- resilient.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
        yield
          assertEquals(result.isError.getOrElse(false), false)
      }
    }
  }

  test("Resilience: custom isFailure predicate for circuit breaker") {
    // Test that the custom isFailure predicate is applied correctly
    // Using a predicate that only counts specific exception types
    val cbConfig = CircuitBreakerConfig(
      failureThreshold = 1,
      isFailure = {
        // Only count TimeoutException as failure, not other types
        case _: java.util.concurrent.TimeoutException => true
        case _                                        => false
      }
    )
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          resilient <- conn.withCircuitBreaker(cbConfig)
          cb = resilient.circuitBreaker.get
          // Tool errors come through as McpError which is not a TimeoutException
          // So this should NOT trip the circuit
          _ <- resilient.callTool("fail", Json.obj()).attempt
          state <- cb.state
        yield
          assertEquals(state, CircuitState.Closed)
      }
    }
  }

  // ============================================================================
  // ADDITIONAL EDGE CASES
  // ============================================================================

  test("Edge: very long string argument is handled correctly") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        val longString = "x" * 10000
        for
          result <- conn.callTool("echo", Json.obj("message" -> Json.fromString(longString)))
        yield
          result.content.head match
            case TextContent(text, _, _) => assertEquals(text.length, 10000)
            case _ => fail("Expected text content")
      }
    }
  }

  test("Edge: unicode characters in arguments are preserved") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        val unicodeMessage = "Hello 世界! 🌍🎉 αβγδ ℃℉"
        for
          result <- conn.callTool("echo", Json.obj("message" -> Json.fromString(unicodeMessage)))
        yield
          result.content.head match
            case TextContent(text, _, _) => assertEquals(text, unicodeMessage)
            case _ => fail("Expected text content")
      }
    }
  }

  test("Edge: empty string argument is handled") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          result <- conn.callTool("echo", Json.obj("message" -> Json.fromString("")))
        yield
          result.content.head match
            case TextContent(text, _, _) => assertEquals(text, "")
            case _ => fail("Expected text content")
      }
    }
  }

  test("Edge: zero and negative numbers work correctly") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          result <- conn.callTool("add", Json.obj("a" -> Json.fromDouble(-5.5).get, "b" -> Json.fromDouble(0.0).get))
        yield
          result.content.head match
            case TextContent(text, _, _) => assertEquals(text, "-5.5")
            case _ => fail("Expected text content")
      }
    }
  }

  test("Edge: floating point precision is maintained") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          result <- conn.callTool("add", Json.obj("a" -> Json.fromDouble(0.1).get, "b" -> Json.fromDouble(0.2).get))
        yield
          result.content.head match
            case TextContent(text, _, _) =>
              val value = text.toDouble
              assert(value > 0.29 && value < 0.31) // Account for floating point
            case _ => fail("Expected text content")
      }
    }
  }

  // ============================================================================
  // MULTIPLE OPERATIONS IN SEQUENCE
  // ============================================================================

  test("Sequence: full workflow - list, call, read, prompt") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          // List available capabilities
          tools <- conn.listTools
          resources <- conn.listResources
          prompts <- conn.listPrompts
          templates <- conn.listResourceTemplates
          // Use each capability
          toolResult <- conn.callTool("add", Json.obj("a" -> Json.fromInt(5), "b" -> Json.fromInt(5)))
          resourceContent <- conn.readResource("file:///test.txt")
          promptResult <- conn.getPrompt("greeting", Map("name" -> "Workflow"))
        yield
          assertEquals(tools.length, 4)
          assertEquals(resources.length, 2)
          assertEquals(prompts.length, 1)
          assertEquals(templates.length, 1)
          assertEquals(toolResult.isError.getOrElse(false), false)
          assertEquals(resourceContent.text, Some("Hello, World!"))
          assertEquals(promptResult.messages.length, 1)
      }
    }
  }

  test("Sequence: repeated operations on same connection") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        for
          // Repeat the same operation multiple times
          r1 <- conn.listTools
          r2 <- conn.listTools
          r3 <- conn.listTools
          r4 <- conn.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(1)))
          r5 <- conn.callTool("add", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(2)))
          r6 <- conn.callTool("add", Json.obj("a" -> Json.fromInt(3), "b" -> Json.fromInt(3)))
        yield
          assertEquals(r1.length, r2.length)
          assertEquals(r2.length, r3.length)
          assert(!r4.isError.getOrElse(false) && !r5.isError.getOrElse(false) && !r6.isError.getOrElse(false))
      }
    }
  }

  // ============================================================================
  // WEBSOCKET SPECIFIC TESTS
  // ============================================================================

  test("WebSocket: multiple concurrent clients") {
    wsServerResource(simpleServer).use { server =>
      val port = server.address.getPort
      (wsConnection(simpleClient, port), wsConnection(simpleClient, port), wsConnection(simpleClient, port)).tupled
        .use { case (conn1, conn2, conn3) =>
          for
            t1 <- conn1.listTools
            t2 <- conn2.listTools
            t3 <- conn3.listTools
          yield
            assertEquals(t1.length, 4)
            assertEquals(t2.length, 4)
            assertEquals(t3.length, 4)
        }
    }
  }

  test("WebSocket: rapid tool calls") {
    wsServerResource(simpleServer).use { server =>
      wsConnection(simpleClient, server.address.getPort).use { conn =>
        (1 to 20).toList.traverse { i =>
          conn.callTool("add", Json.obj("a" -> Json.fromInt(i), "b" -> Json.fromInt(i)))
        }.map { results =>
          assertEquals(results.length, 20)
          assert(results.forall(!_.isError.getOrElse(false)))
        }
      }
    }
  }

  // ============================================================================
  // LARGE PAYLOADS
  // ============================================================================

  test("Performance: large number of concurrent connections") {
    httpServerResource(simpleServer).use { server =>
      val port = server.address.getPort
      val connections = (1 to 10).toList.map(_ => httpConnection(simpleClient, port))
      connections.sequence.use { conns =>
        conns.parTraverse { conn =>
          conn.listTools.map(tools => assertEquals(tools.length, 4))
        }.void
      }
    }
  }

  test("Performance: mixed operations in parallel") {
    httpServerResource(simpleServer).use { server =>
      httpConnection(simpleClient, server.address.getPort).use { conn =>
        val operations = List(
          conn.listTools,
          conn.listResources,
          conn.listPrompts,
          conn.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(1))).map(_ => List.empty[Tool]),
          conn.readResource("file:///test.txt").map(_ => List.empty[Tool]),
          conn.ping.map(_ => List.empty[Tool])
        )
        operations.parSequence.map { results =>
          assertEquals(results.length, 6)
        }
      }
    }
  }
