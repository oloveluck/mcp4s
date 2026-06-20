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

package mcp4s.server

import cats.effect.IO
import cats.syntax.semigroup.*
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class ServerCompositionSpec extends CatsEffectSuite:

  // === Test Fixtures ===

  val addTool: Tool = Tool(
    name = "add",
    description = Some("Add two numbers"),
    inputSchema = JsonSchema.obj(
      Map("a" -> JsonSchema.number(), "b" -> JsonSchema.number()),
      List("a", "b")
    )
  )

  val subtractTool: Tool = Tool(
    name = "subtract",
    description = Some("Subtract two numbers"),
    inputSchema = JsonSchema.obj(
      Map("a" -> JsonSchema.number(), "b" -> JsonSchema.number()),
      List("a", "b")
    )
  )

  val fileResource: Resource = Resource(
    uri = "file:///test.txt",
    name = "Test File",
    mimeType = Some("text/plain")
  )

  val configResource: Resource = Resource(
    uri = "file:///config.json",
    name = "Config File",
    mimeType = Some("application/json")
  )

  val greetingPrompt: Prompt = Prompt(
    name = "greeting",
    description = Some("A greeting prompt"),
    arguments = List(PromptArgument("name", Some("User name"), required = true))
  )

  val farewellPrompt: Prompt = Prompt(
    name = "farewell",
    description = Some("A farewell prompt"),
    arguments = List(PromptArgument("name", Some("User name"), required = true))
  )

  def serverWithAdd: Server[IO] =
    Server.fromTools[IO](
      ServerInfo("add-server", "1.0.0"),
      Tools.single[IO](addTool) { args =>
        val a = args.hcursor.get[Int]("a").getOrElse(0)
        val b = args.hcursor.get[Int]("b").getOrElse(0)
        IO.pure(ToolResult.text(s"${a + b}"))
      }
    )

  def serverWithSubtract: Server[IO] =
    Server.fromTools[IO](
      ServerInfo("subtract-server", "1.0.0"),
      Tools.single[IO](subtractTool) { args =>
        val a = args.hcursor.get[Int]("a").getOrElse(0)
        val b = args.hcursor.get[Int]("b").getOrElse(0)
        IO.pure(ToolResult.text(s"${a - b}"))
      }
    )

  def serverWithResource: Server[IO] =
    Server.from[IO](
      ServerInfo("resource-server", "1.0.0"),
      Tools.empty[IO],
      Resources.single[IO](fileResource)(_ =>
        IO.pure(ResourceContent.text("file:///test.txt", "file content"))
      ),
      Prompts.empty[IO]
    )

  def serverWithConfigResource: Server[IO] =
    Server.from[IO](
      ServerInfo("config-server", "1.0.0"),
      Tools.empty[IO],
      Resources.single[IO](configResource)(_ =>
        IO.pure(ResourceContent.text("file:///config.json", "{}"))
      ),
      Prompts.empty[IO]
    )

  def serverWithGreetingPrompt: Server[IO] =
    Server.from[IO](
      ServerInfo("greeting-server", "1.0.0"),
      Tools.empty[IO],
      Resources.empty[IO],
      Prompts.single[IO](greetingPrompt) { args =>
        IO.pure(
          GetPromptResult(
            Some("A greeting"),
            List(
              PromptMessage(Role.User, TextContent(s"Hello, ${args.getOrElse("name", "World")}!"))
            )
          )
        )
      }
    )

  def serverWithFarewellPrompt: Server[IO] =
    Server.from[IO](
      ServerInfo("farewell-server", "1.0.0"),
      Tools.empty[IO],
      Resources.empty[IO],
      Prompts.single[IO](farewellPrompt) { args =>
        IO.pure(
          GetPromptResult(
            Some("A farewell"),
            List(
              PromptMessage(Role.User, TextContent(s"Goodbye, ${args.getOrElse("name", "World")}!"))
            )
          )
        )
      }
    )

  def emptyServer: Server[IO] =
    Server.from[IO](
      ServerInfo("empty-server", "1.0.0"),
      Tools.empty[IO],
      Resources.empty[IO],
      Prompts.empty[IO]
    )

  // === Basic Composition Tests ===

  test("combine merges tools from both servers") {
    val combined = serverWithAdd |+| serverWithSubtract
    for tools <- combined.listTools
    yield assertEquals(tools.map(_.name).toSet, Set("add", "subtract"))
  }

  test("combine uses left server's info") {
    val combined = serverWithAdd |+| serverWithSubtract
    assertEquals(combined.info.name, "add-server")
  }

  test("withInfo overrides combined server info") {
    val combined = (serverWithAdd |+| serverWithSubtract).withInfo(ServerInfo("combined", "2.0.0"))
    assertEquals(combined.info.name, "combined")
    assertEquals(combined.info.version, "2.0.0")
  }

  // === Tool Conflict Resolution ===

  test("combine prefers left server's tool on name conflict") {
    // Create two servers with same tool name but different handlers
    val serverV1 = Server.fromTools[IO](
      ServerInfo("v1", "1.0.0"),
      Tools.single[IO](addTool)(_ => IO.pure(ToolResult.text("v1")))
    )

    val serverV2 = Server.fromTools[IO](
      ServerInfo("v2", "1.0.0"),
      Tools.single[IO](addTool)(_ => IO.pure(ToolResult.text("v2")))
    )

    val combined = serverV1 |+| serverV2
    for result <- combined.callTool("add", Json.obj())
    yield assertEquals(result.textContent, "v1")
  }

  test("combine delegates to right server when left doesn't have tool") {
    val combined = serverWithAdd |+| serverWithSubtract
    for result <- combined.callTool("subtract", Json.obj("a" -> 5.asJson, "b" -> 3.asJson))
    yield assertEquals(result.textContent, "2")
  }

  test("combine raises ToolNotFound when neither server has tool") {
    val combined = serverWithAdd |+| serverWithSubtract
    for result <- combined.callTool("multiply", Json.obj()).attempt
    yield
      assert(result.isLeft)
      assert(result.left.exists(_.isInstanceOf[McpError.ToolNotFound]))
  }

  // === Resource Composition ===

  test("combine merges resources from both servers") {
    val combined = serverWithResource |+| serverWithConfigResource
    for resources <- combined.listResources
    yield assertEquals(resources.map(_.uri).toSet, Set("file:///test.txt", "file:///config.json"))
  }

  test("combine prefers left server's resource on URI conflict") {
    val serverA = Server.from[IO](
      ServerInfo("a", "1.0.0"),
      Tools.empty[IO],
      Resources.single[IO](fileResource)(_ =>
        IO.pure(ResourceContent.text("file:///test.txt", "content A"))
      ),
      Prompts.empty[IO]
    )

    val serverB = Server.from[IO](
      ServerInfo("b", "1.0.0"),
      Tools.empty[IO],
      Resources.single[IO](fileResource)(_ =>
        IO.pure(ResourceContent.text("file:///test.txt", "content B"))
      ),
      Prompts.empty[IO]
    )

    val combined = serverA |+| serverB
    for content <- combined.readResource("file:///test.txt")
    yield assertEquals(content.text, Some("content A"))
  }

  test("combine delegates to right server when left doesn't have resource") {
    val combined = serverWithResource |+| serverWithConfigResource
    for content <- combined.readResource("file:///config.json")
    yield assertEquals(content.text, Some("{}"))
  }

  // === Prompt Composition ===

  test("combine merges prompts from both servers") {
    val combined = serverWithGreetingPrompt |+| serverWithFarewellPrompt
    for prompts <- combined.listPrompts
    yield assertEquals(prompts.map(_.name).toSet, Set("greeting", "farewell"))
  }

  test("combine prefers left server's prompt on name conflict") {
    val serverA = Server.from[IO](
      ServerInfo("a", "1.0.0"),
      Tools.empty[IO],
      Resources.empty[IO],
      Prompts.single[IO](greetingPrompt)(_ =>
        IO.pure(GetPromptResult(Some("A"), List(PromptMessage(Role.User, TextContent("A")))))
      )
    )

    val serverB = Server.from[IO](
      ServerInfo("b", "1.0.0"),
      Tools.empty[IO],
      Resources.empty[IO],
      Prompts.single[IO](greetingPrompt)(_ =>
        IO.pure(GetPromptResult(Some("B"), List(PromptMessage(Role.User, TextContent("B")))))
      )
    )

    val combined = serverA |+| serverB
    for result <- combined.getPrompt("greeting", Map.empty)
    yield assertEquals(result.description, Some("A"))
  }

  test("combine delegates to right server when left doesn't have prompt") {
    val combined = serverWithGreetingPrompt |+| serverWithFarewellPrompt
    for result <- combined.getPrompt("farewell", Map("name" -> "Alice"))
    yield assert(result.messages.head.content.asInstanceOf[TextContent].text.contains("Goodbye"))
  }

  // === Capabilities Merging ===

  test("combine merges capabilities correctly") {
    val toolsOnly     = serverWithAdd
    val resourcesOnly = serverWithResource

    val combined = toolsOnly |+| resourcesOnly
    assert(combined.capabilities.tools.isDefined)
    assert(combined.capabilities.resources.isDefined)
  }

  test("combine with empty server preserves capabilities") {
    val combined = serverWithAdd |+| emptyServer
    assert(combined.capabilities.tools.isDefined)
  }

  // === Semigroup Laws ===

  test("Semigroup associativity holds for listTools") {
    val a = serverWithAdd
    val b = serverWithSubtract
    val c = serverWithResource

    for
      abc1 <- ((a |+| b) |+| c).listTools
      abc2 <- (a |+| (b |+| c)).listTools
    yield assertEquals(abc1.map(_.name).toSet, abc2.map(_.name).toSet)
  }

  test("combine using cats Semigroup syntax") {
    val combined = serverWithAdd |+| serverWithSubtract
    for tools <- combined.listTools
    yield assertEquals(tools.map(_.name).toSet, Set("add", "subtract"))
  }

  // === Edge Cases ===

  test("combining empty servers produces empty server") {
    val combined = emptyServer |+| emptyServer
    for
      tools     <- combined.listTools
      resources <- combined.listResources
      prompts   <- combined.listPrompts
    yield
      assert(tools.isEmpty)
      assert(resources.isEmpty)
      assert(prompts.isEmpty)
  }

  test("withInfo preserves tool functionality") {
    val combined = (serverWithAdd |+| serverWithSubtract).withInfo(ServerInfo("new", "1.0.0"))
    for result <- combined.callTool("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson))
    yield assertEquals(result.textContent, "5")
  }
