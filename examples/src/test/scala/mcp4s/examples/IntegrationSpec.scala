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

import cats.effect.{IO, Ref, Resource}
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

  def createTestServer: Server[IO] =
    val tools =
      Tools.single[IO](testTool) { args =>
        val cursor = args.hcursor
        for
          a <- cursor.get[Double]("a").liftTo[IO]
          b <- cursor.get[Double]("b").liftTo[IO]
        yield ToolResult.text(s"${a + b}")
      } |+|
        Tools.singleWithContext[IO](
          Tool("slow_add", Some("Add with progress"), JsonSchema.empty)
        ) { (args, ctx) =>
          val cursor = args.hcursor
          for
            a <- cursor.get[Double]("a").liftTo[IO]
            b <- cursor.get[Double]("b").liftTo[IO]
            _ <- ctx.progress(0, Some(3))
            _ <- IO.sleep(20.millis)
            _ <- ctx.progress(1, Some(3))
            _ <- IO.sleep(20.millis)
            _ <- ctx.progress(2, Some(3))
            _ <- IO.sleep(20.millis)
            _ <- ctx.progress(3, Some(3))
          yield ToolResult.text(s"${a + b}")
        }
    val resources = Resources.single[IO](testResource)(_ =>
      IO.pure(ResourceContent.text("file:///test.txt", "Test file content"))
    )
    val prompts = Prompts.single[IO](testPrompt) { args =>
      val name = args.getOrElse("name", "World")
      IO.pure(
        GetPromptResult(
          Some("A greeting prompt"),
          List(PromptMessage(Role.User, TextContent(s"Hello, $name!")))
        )
      )
    }
    Server.from[IO](ServerInfo("test-server", "1.0.0"), tools, resources, prompts)

  def testClient: McpClient[IO] = McpClient.from[IO](ClientInfo("test-client", "1.0.0"))

  // Use a random port for testing to avoid conflicts
  def serverResource: Resource[IO, org.http4s.server.Server] =
    HttpTransport.serve[IO](createTestServer, HttpConfig(port = port"0"))

  def connectedClient(serverPort: Int): Resource[IO, McpConnection[IO]] =
    EmberClientBuilder
      .default[IO]
      .build
      .flatMap: httpClient =>
        HttpClientTransport.connect[IO](
          testClient,
          HttpTransportConfig[IO](s"http://localhost:$serverPort/mcp"),
          httpClient
        )

  // === Integration Tests ===

  test("client connects to server and receives server info") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        IO {
          assertEquals(conn.serverInfo.name, "test-server")
          assertEquals(conn.serverInfo.version, "1.0.0")
        }
  }

  test("client receives server capabilities") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        IO {
          assert(conn.serverCapabilities.tools.isDefined)
          assert(conn.serverCapabilities.resources.isDefined)
          assert(conn.serverCapabilities.prompts.isDefined)
        }
  }

  test("client lists tools from server") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        for tools <- conn.listAllTools
        yield
          assertEquals(tools.length, 2)
          assert(tools.exists(_.name == "add"))
  }

  test("client calls tool and receives result") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        for result <- conn.callTool(
            "add",
            Json.obj(
              "a" -> Json.fromDouble(5.0).get,
              "b" -> Json.fromDouble(3.0).get
            )
          )
        yield
          assertEquals(result.isError.getOrElse(false), false)
          assertEquals(result.content.length, 1)
          result.content.head match
            case TextContent(text, _, _) =>
              assertEquals(text, "8.0")
            case _ =>
              fail("Expected text content")
  }

  test("client handles tool error") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        conn
          .callTool("nonexistent", Json.obj())
          .attempt
          .map: result =>
            assert(result.isLeft)
  }

  test("client lists resources from server") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        for resources <- conn.listAllResources
        yield
          assertEquals(resources.length, 1)
          assertEquals(resources.head.uri, "file:///test.txt")
  }

  test("client reads resource from server") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        for content <- conn.readResource("file:///test.txt")
        yield
          assertEquals(content.uri, "file:///test.txt")
          assertEquals(content.text, Some("Test file content"))
  }

  test("client lists prompts from server") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        for prompts <- conn.listAllPrompts
        yield
          assertEquals(prompts.length, 1)
          assertEquals(prompts.head.name, "greeting")
  }

  test("client gets prompt from server") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        for result <- conn.getPrompt("greeting", Map("name" -> "Alice"))
        yield
          assertEquals(result.description, Some("A greeting prompt"))
          assertEquals(result.messages.length, 1)
          result.messages.head.content match
            case TextContent(text, _, _) =>
              assertEquals(text, "Hello, Alice!")
            case _ =>
              fail("Expected text content")
  }

  test("client pings server") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        for _ <- conn.ping
        yield ()
  }

  test("client shuts down gracefully") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        for _ <- conn.shutdown
        yield ()
  }

  test("multiple concurrent tool calls") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        val calls = List(
          conn.callTool("add", Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(1))),
          conn.callTool("add", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(2))),
          conn.callTool("add", Json.obj("a" -> Json.fromInt(3), "b" -> Json.fromInt(3)))
        )
        import cats.syntax.parallel.*
        for results <- calls.parSequence
        yield
          assertEquals(results.length, 3)
          assert(results.forall(!_.isError.getOrElse(false)))
  }

  test("client calls tool with progress and receives progress notifications") {
    serverResource.use: server =>
      val port = server.address.getPort
      connectedClient(port).use: conn =>
        for
          progressUpdates <- Ref.of[IO, List[ProgressParams]](Nil)
          result <- conn.callTool(
            "slow_add",
            Json.obj("a" -> Json.fromDouble(5.0).get, "b" -> Json.fromDouble(3.0).get),
            p => progressUpdates.update(_ :+ p)
          )
          updates <- progressUpdates.get
        yield
          // Verify the tool result
          assertEquals(result.isError.getOrElse(false), false)
          result.content.head match
            case TextContent(text, _, _) => assertEquals(text, "8.0")
            case _                       => fail("Expected text content")

          // Verify progress notifications were received
          assert(updates.nonEmpty, s"Expected progress notifications, got none")
          // The server sends 4 progress updates: 0/3, 1/3, 2/3, 3/3
          assertEquals(updates.length, 4)
          assertEquals(updates.head.progress, 0.0)
          assertEquals(updates.last.progress, 3.0)
          assert(updates.forall(_.total == Some(3.0)))
  }

  test("server health endpoint works") {
    serverResource.use: server =>
      val port = server.address.getPort
      import org.http4s.ember.client.EmberClientBuilder
      import org.http4s.*
      import org.http4s.circe.*

      EmberClientBuilder
        .default[IO]
        .build
        .use: httpClient =>
          val request = Request[IO](
            method = Method.GET,
            uri = Uri.unsafeFromString(s"http://localhost:$port/health")
          )
          for response <- httpClient.expect[Json](request)
          yield assertEquals(response.hcursor.get[String]("status"), Right("ok"))
  }
