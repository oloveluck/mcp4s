package mcp4s.examples

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.port
import scala.concurrent.duration.*
import io.circe.Json
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.*
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.transport.*
import mcp4s.client.transport.*
import munit.CatsEffectSuite

class WebSocketIntegrationSpec extends CatsEffectSuite:

  given Tracer[IO] = Tracer.noop[IO]

  // === Test Server Setup ===

  val testTool: Tool = Tool(
    name = "multiply",
    description = Some("Multiply two numbers"),
    inputSchema = JsonSchema.obj(
      Map(
        "a" -> JsonSchema.number(Some("First number")),
        "b" -> JsonSchema.number(Some("Second number"))
      ),
      List("a", "b")
    )
  )

  val testResource: mcp4s.protocol.Resource = mcp4s.protocol.Resource(
    uri = "file:///ws-test.txt",
    name = "WebSocket Test File",
    mimeType = Some("text/plain")
  )

  val testPrompt: Prompt = Prompt(
    name = "ws-greeting",
    description = Some("WebSocket greeting"),
    arguments = List(PromptArgument("name", Some("Name to greet"), required = true))
  )

  def createTestServer: McpServer[IO] = McpServer.builder[IO]
    .withInfo(ServerInfo("ws-test-server", "1.0.0"))
    .withTool(testTool, args => {
      val cursor = args.hcursor
      for
        a <- cursor.get[Double]("a").liftTo[IO]
        b <- cursor.get[Double]("b").liftTo[IO]
      yield ToolResult.text(s"${a * b}")
    })
    .withResource(testResource, _ => IO.pure(ResourceContent.text("file:///ws-test.txt", "WebSocket test content")))
    .withPrompt(testPrompt, args => {
      val name = args.getOrElse("name", "World")
      IO.pure(GetPromptResult(
        Some("A WebSocket greeting"),
        List(PromptMessage(Role.User, TextContent(s"Hello via WebSocket, $name!")))
      ))
    })
    .build

  def testClient: McpClient[IO] = McpClient.builder[IO]
    .withInfo(ClientInfo("ws-test-client", "1.0.0"))
    .build

  // Use port 0 for random available port
  def wsServerResource: Resource[IO, org.http4s.server.Server] =
    WebSocketTransport.serve[IO](createTestServer, WebSocketConfig(port = port"0"))

  def wsConnectedClient(serverPort: Int): Resource[IO, McpConnection[IO]] =
    WebSocketClientTransport.connect[IO](
      testClient,
      WebSocketClientConfig(url = s"ws://localhost:$serverPort")
    )

  // === WebSocket Integration Tests ===

  test("WebSocket: client connects and receives server info") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      wsConnectedClient(port).use { conn =>
        IO {
          assertEquals(conn.serverInfo.name, "ws-test-server")
          assertEquals(conn.serverInfo.version, "1.0.0")
        }
      }
    }
  }

  test("WebSocket: client receives server capabilities") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      wsConnectedClient(port).use { conn =>
        IO {
          assert(conn.serverCapabilities.tools.isDefined)
          assert(conn.serverCapabilities.resources.isDefined)
          assert(conn.serverCapabilities.prompts.isDefined)
        }
      }
    }
  }

  test("WebSocket: client lists tools from server") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      wsConnectedClient(port).use { conn =>
        for
          tools <- conn.listTools
        yield
          assertEquals(tools.length, 1)
          assertEquals(tools.head.name, "multiply")
      }
    }
  }

  test("WebSocket: client calls tool and receives result") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      wsConnectedClient(port).use { conn =>
        for
          result <- conn.callTool("multiply", Json.obj(
            "a" -> Json.fromDouble(5.0).get,
            "b" -> Json.fromDouble(3.0).get
          ))
        yield
          assertEquals(result.isError.getOrElse(false), false)
          assertEquals(result.content.length, 1)
          result.content.head match
            case TextContent(text, _, _) =>
              assertEquals(text, "15.0")
            case _ =>
              fail("Expected text content")
      }
    }
  }

  test("WebSocket: client handles tool error") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      wsConnectedClient(port).use { conn =>
        conn.callTool("nonexistent", Json.obj()).attempt.map { result =>
          assert(result.isLeft)
        }
      }
    }
  }

  test("WebSocket: client lists resources from server") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      wsConnectedClient(port).use { conn =>
        for
          resources <- conn.listResources
        yield
          assertEquals(resources.length, 1)
          assertEquals(resources.head.uri, "file:///ws-test.txt")
      }
    }
  }

  test("WebSocket: client reads resource from server") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      wsConnectedClient(port).use { conn =>
        for
          content <- conn.readResource("file:///ws-test.txt")
        yield
          assertEquals(content.uri, "file:///ws-test.txt")
          assertEquals(content.text, Some("WebSocket test content"))
      }
    }
  }

  test("WebSocket: client lists prompts from server") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      wsConnectedClient(port).use { conn =>
        for
          prompts <- conn.listPrompts
        yield
          assertEquals(prompts.length, 1)
          assertEquals(prompts.head.name, "ws-greeting")
      }
    }
  }

  test("WebSocket: client gets prompt from server") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      wsConnectedClient(port).use { conn =>
        for
          result <- conn.getPrompt("ws-greeting", Map("name" -> "WebSocket"))
        yield
          assertEquals(result.description, Some("A WebSocket greeting"))
          assertEquals(result.messages.length, 1)
          result.messages.head.content match
            case TextContent(text, _, _) =>
              assertEquals(text, "Hello via WebSocket, WebSocket!")
            case _ =>
              fail("Expected text content")
      }
    }
  }

  test("WebSocket: client pings server") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      wsConnectedClient(port).use { conn =>
        conn.ping
      }
    }
  }

  test("WebSocket: multiple concurrent tool calls") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      wsConnectedClient(port).use { conn =>
        val calls = List(
          conn.callTool("multiply", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))),
          conn.callTool("multiply", Json.obj("a" -> Json.fromInt(4), "b" -> Json.fromInt(5))),
          conn.callTool("multiply", Json.obj("a" -> Json.fromInt(6), "b" -> Json.fromInt(7)))
        )
        import cats.syntax.parallel.*
        for
          results <- calls.parSequence
        yield
          assertEquals(results.length, 3)
          assert(results.forall(!_.isError.getOrElse(false)))
      }
    }
  }

  test("WebSocket: health endpoint works") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      import org.http4s.ember.client.EmberClientBuilder
      import org.http4s.*
      import org.http4s.circe.*

      EmberClientBuilder.default[IO].build.use { httpClient =>
        val request = Request[IO](
          method = Method.GET,
          uri = Uri.unsafeFromString(s"http://localhost:$port/health")
        )
        for
          response <- httpClient.expect[Json](request)
        yield
          assertEquals(response.hcursor.get[String]("status"), Right("ok"))
      }
    }
  }

  // === Concurrency Tests ===

  test("WebSocket: multiple concurrent clients have isolated state") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      // Connect 3 clients simultaneously
      val client1 = wsConnectedClient(port)
      val client2 = wsConnectedClient(port)
      val client3 = wsConnectedClient(port)

      (client1, client2, client3).tupled.use { case (conn1, conn2, conn3) =>
        for
          // Each client should have received proper server info (proves isolated initialization)
          _ <- IO {
            assertEquals(conn1.serverInfo.name, "ws-test-server")
            assertEquals(conn2.serverInfo.name, "ws-test-server")
            assertEquals(conn3.serverInfo.name, "ws-test-server")
          }
          // Each client can list tools independently
          tools1 <- conn1.listTools
          tools2 <- conn2.listTools
          tools3 <- conn3.listTools
        yield
          assertEquals(tools1.length, 1)
          assertEquals(tools2.length, 1)
          assertEquals(tools3.length, 1)
      }
    }
  }

  test("WebSocket: concurrent requests from different clients don't interfere") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      val client1 = wsConnectedClient(port)
      val client2 = wsConnectedClient(port)

      (client1, client2).tupled.use { case (conn1, conn2) =>
        import cats.syntax.parallel.*
        // Send different requests from each client concurrently
        val requests = List(
          conn1.callTool("multiply", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))),
          conn2.callTool("multiply", Json.obj("a" -> Json.fromInt(10), "b" -> Json.fromInt(10))),
          conn1.callTool("multiply", Json.obj("a" -> Json.fromInt(5), "b" -> Json.fromInt(5))),
          conn2.callTool("multiply", Json.obj("a" -> Json.fromInt(7), "b" -> Json.fromInt(7)))
        )
        for
          results <- requests.parSequence
        yield
          // Verify each result matches its request (no cross-contamination)
          assertEquals(results.length, 4)
          assert(results.forall(!_.isError.getOrElse(false)))
          // Results should be: 6.0, 100.0, 25.0, 49.0
          val texts = results.flatMap(_.content.collect { case TextContent(t, _, _) => t })
          assert(texts.contains("6.0"))
          assert(texts.contains("100.0"))
          assert(texts.contains("25.0"))
          assert(texts.contains("49.0"))
      }
    }
  }

  test("WebSocket: rapid connect/disconnect cycle") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      val iterations = (1 to 10).toList
      iterations.traverse_ { i =>
        wsConnectedClient(port).use { conn =>
          for
            tools <- conn.listTools
            _ <- IO(assertEquals(tools.length, 1))
          yield ()
        } >> IO.sleep(50.millis) // Allow resource cleanup between iterations
      }
    }
  }

  test("WebSocket: clients connect sequentially after previous disconnects") {
    wsServerResource.use { server =>
      val port = server.address.getPort
      for
        // First client connects, does work, disconnects
        result1 <- wsConnectedClient(port).use { conn =>
          conn.callTool("multiply", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(2)))
        }
        // Second client connects to same server (should get fresh state)
        result2 <- wsConnectedClient(port).use { conn =>
          conn.callTool("multiply", Json.obj("a" -> Json.fromInt(3), "b" -> Json.fromInt(3)))
        }
      yield
        // Both should succeed independently
        assertEquals(result1.isError.getOrElse(false), false)
        assertEquals(result2.isError.getOrElse(false), false)
    }
  }
