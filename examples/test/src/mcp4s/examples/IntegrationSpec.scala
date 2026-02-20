package mcp4s.examples

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.port
import io.circe.Json
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.*
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.transport.*
import mcp4s.client.transport.*
import munit.CatsEffectSuite
import org.http4s.ember.client.EmberClientBuilder

class IntegrationSpec extends CatsEffectSuite:

  given Tracer[IO] = Tracer.noop[IO]

  // === Test Server Setup ===

  val testTool: Tool = Tool(
    name = "add",
    description = Some("Add two numbers"),
    inputSchema = JsonSchema.obj(
      Map(
        "a" -> JsonSchema.number(Some("First number")),
        "b" -> JsonSchema.number(Some("Second number"))
      ),
      List("a", "b")
    )
  )

  val testResource: mcp4s.protocol.Resource = mcp4s.protocol.Resource(
    uri = "file:///test.txt",
    name = "Test File",
    mimeType = Some("text/plain")
  )

  val testPrompt: Prompt = Prompt(
    name = "greeting",
    description = Some("Generate a greeting"),
    arguments = List(PromptArgument("name", Some("Name to greet"), required = true))
  )

  def createTestServer: McpServer[IO] = McpServer.builder[IO]
    .withInfo(ServerInfo("test-server", "1.0.0"))
    .withTool(testTool, args => {
      val cursor = args.hcursor
      for
        a <- cursor.get[Double]("a").liftTo[IO]
        b <- cursor.get[Double]("b").liftTo[IO]
      yield ToolResult.text(s"${a + b}")
    })
    .withResource(testResource, _ => IO.pure(ResourceContent.text("file:///test.txt", "Test file content")))
    .withPrompt(testPrompt, args => {
      val name = args.getOrElse("name", "World")
      IO.pure(GetPromptResult(
        Some("A greeting prompt"),
        List(PromptMessage(Role.User, TextContent(s"Hello, $name!")))
      ))
    })
    .build

  def testClient: McpClient[IO] = McpClient.builder[IO]
    .withInfo(ClientInfo("test-client", "1.0.0"))
    .build

  // Use a random port for testing to avoid conflicts
  def serverResource: Resource[IO, org.http4s.server.Server] =
    HttpTransport.serve[IO](createTestServer, HttpConfig(port = port"0"))

  def connectedClient(serverPort: Int): Resource[IO, McpConnection[IO]] =
    EmberClientBuilder.default[IO].build.flatMap{ httpClient =>
      HttpClientTransport.connect[IO](
        testClient,
        HttpClientConfig(s"http://localhost:$serverPort"),
        httpClient
      )
    }

  // === Integration Tests ===

  test("client connects to server and receives server info") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        IO {
          assertEquals(conn.serverInfo.name, "test-server")
          assertEquals(conn.serverInfo.version, "1.0.0")
        }
      }
    }
  }

  test("client receives server capabilities") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        IO {
          assert(conn.serverCapabilities.tools.isDefined)
          assert(conn.serverCapabilities.resources.isDefined)
          assert(conn.serverCapabilities.prompts.isDefined)
        }
      }
    }
  }

  test("client lists tools from server") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        for
          tools <- conn.listTools
        yield
          assertEquals(tools.length, 1)
          assertEquals(tools.head.name, "add")
      }
    }
  }

  test("client calls tool and receives result") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        for
          result <- conn.callTool("add", Json.obj(
            "a" -> Json.fromDouble(5.0).get,
            "b" -> Json.fromDouble(3.0).get
          ))
        yield
          assertEquals(result.isError.getOrElse(false), false)
          assertEquals(result.content.length, 1)
          result.content.head match
            case TextContent(text, _, _) =>
              assertEquals(text, "8.0")
            case _ =>
              fail("Expected text content")
      }
    }
  }

  test("client handles tool error") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        conn.callTool("nonexistent", Json.obj()).attempt.map { result =>
          assert(result.isLeft)
        }
      }
    }
  }

  test("client lists resources from server") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        for
          resources <- conn.listResources
        yield
          assertEquals(resources.length, 1)
          assertEquals(resources.head.uri, "file:///test.txt")
      }
    }
  }

  test("client reads resource from server") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        for
          content <- conn.readResource("file:///test.txt")
        yield
          assertEquals(content.uri, "file:///test.txt")
          assertEquals(content.text, Some("Test file content"))
      }
    }
  }

  test("client lists prompts from server") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        for
          prompts <- conn.listPrompts
        yield
          assertEquals(prompts.length, 1)
          assertEquals(prompts.head.name, "greeting")
      }
    }
  }

  test("client gets prompt from server") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        for
          result <- conn.getPrompt("greeting", Map("name" -> "Alice"))
        yield
          assertEquals(result.description, Some("A greeting prompt"))
          assertEquals(result.messages.length, 1)
          result.messages.head.content match
            case TextContent(text, _, _) =>
              assertEquals(text, "Hello, Alice!")
            case _ =>
              fail("Expected text content")
      }
    }
  }

  test("client pings server") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        for
          _ <- conn.ping
        yield ()
      }
    }
  }

  test("client shuts down gracefully") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        for
          _ <- conn.shutdown
        yield ()
      }
    }
  }

  test("multiple concurrent tool calls") {
    serverResource.use { server =>
      val port = server.address.getPort
      connectedClient(port).use { conn =>
        val calls = List(
          conn.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(1))),
          conn.callTool("add", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(2))),
          conn.callTool("add", Json.obj("a" -> Json.fromInt(3), "b" -> Json.fromInt(3)))
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

  test("server health endpoint works") {
    serverResource.use { server =>
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
