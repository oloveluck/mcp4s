package mcp4s.server

import cats.effect.IO
import cats.syntax.semigroup.*
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpServerCompositionSpec extends CatsEffectSuite:

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

  def serverWithAdd: McpServer[IO] = McpServer.builder[IO]
    .withInfo(ServerInfo("add-server", "1.0.0"))
    .withTool(addTool, args =>
      val a = args.hcursor.get[Int]("a").getOrElse(0)
      val b = args.hcursor.get[Int]("b").getOrElse(0)
      IO.pure(ToolResult.text(s"${a + b}"))
    )
    .build

  def serverWithSubtract: McpServer[IO] = McpServer.builder[IO]
    .withInfo(ServerInfo("subtract-server", "1.0.0"))
    .withTool(subtractTool, args =>
      val a = args.hcursor.get[Int]("a").getOrElse(0)
      val b = args.hcursor.get[Int]("b").getOrElse(0)
      IO.pure(ToolResult.text(s"${a - b}"))
    )
    .build

  def serverWithResource: McpServer[IO] = McpServer.builder[IO]
    .withInfo(ServerInfo("resource-server", "1.0.0"))
    .withResource(fileResource, _ => IO.pure(ResourceContent.text("file:///test.txt", "file content")))
    .build

  def serverWithConfigResource: McpServer[IO] = McpServer.builder[IO]
    .withInfo(ServerInfo("config-server", "1.0.0"))
    .withResource(configResource, _ => IO.pure(ResourceContent.text("file:///config.json", "{}")))
    .build

  def serverWithGreetingPrompt: McpServer[IO] = McpServer.builder[IO]
    .withInfo(ServerInfo("greeting-server", "1.0.0"))
    .withPrompt(greetingPrompt, args =>
      IO.pure(GetPromptResult(
        Some("A greeting"),
        List(PromptMessage(Role.User, TextContent(s"Hello, ${args.getOrElse("name", "World")}!")))
      ))
    )
    .build

  def serverWithFarewellPrompt: McpServer[IO] = McpServer.builder[IO]
    .withInfo(ServerInfo("farewell-server", "1.0.0"))
    .withPrompt(farewellPrompt, args =>
      IO.pure(GetPromptResult(
        Some("A farewell"),
        List(PromptMessage(Role.User, TextContent(s"Goodbye, ${args.getOrElse("name", "World")}!")))
      ))
    )
    .build

  def emptyServer: McpServer[IO] = McpServer.builder[IO]
    .withInfo(ServerInfo("empty-server", "1.0.0"))
    .build

  // === Basic Composition Tests ===

  test("combine merges tools from both servers") {
    val combined = serverWithAdd <+> serverWithSubtract
    for
      tools <- combined.listTools
    yield
      assertEquals(tools.map(_.name).toSet, Set("add", "subtract"))
  }

  test("combine uses left server's info") {
    val combined = serverWithAdd <+> serverWithSubtract
    assertEquals(combined.info.name, "add-server")
  }

  test("withInfo overrides combined server info") {
    val combined = (serverWithAdd <+> serverWithSubtract).withInfo(ServerInfo("combined", "2.0.0"))
    assertEquals(combined.info.name, "combined")
    assertEquals(combined.info.version, "2.0.0")
  }

  // === Tool Conflict Resolution ===

  test("combine prefers left server's tool on name conflict") {
    // Create two servers with same tool name but different handlers
    val serverV1 = McpServer.builder[IO]
      .withInfo(ServerInfo("v1", "1.0.0"))
      .withTool(addTool, _ => IO.pure(ToolResult.text("v1")))
      .build

    val serverV2 = McpServer.builder[IO]
      .withInfo(ServerInfo("v2", "1.0.0"))
      .withTool(addTool, _ => IO.pure(ToolResult.text("v2")))
      .build

    val combined = serverV1 <+> serverV2
    for
      result <- combined.callTool("add", Json.obj())
    yield
      assertEquals(result.textContent, "v1")
  }

  test("combine delegates to right server when left doesn't have tool") {
    val combined = serverWithAdd <+> serverWithSubtract
    for
      result <- combined.callTool("subtract", Json.obj("a" -> 5.asJson, "b" -> 3.asJson))
    yield
      assertEquals(result.textContent, "2")
  }

  test("combine raises ToolNotFound when neither server has tool") {
    val combined = serverWithAdd <+> serverWithSubtract
    for
      result <- combined.callTool("multiply", Json.obj()).attempt
    yield
      assert(result.isLeft)
      assert(result.left.exists(_.isInstanceOf[McpError.ToolNotFound]))
  }

  // === Resource Composition ===

  test("combine merges resources from both servers") {
    val combined = serverWithResource <+> serverWithConfigResource
    for
      resources <- combined.listResources
    yield
      assertEquals(resources.map(_.uri).toSet, Set("file:///test.txt", "file:///config.json"))
  }

  test("combine prefers left server's resource on URI conflict") {
    val serverA = McpServer.builder[IO]
      .withInfo(ServerInfo("a", "1.0.0"))
      .withResource(fileResource, _ => IO.pure(ResourceContent.text("file:///test.txt", "content A")))
      .build

    val serverB = McpServer.builder[IO]
      .withInfo(ServerInfo("b", "1.0.0"))
      .withResource(fileResource, _ => IO.pure(ResourceContent.text("file:///test.txt", "content B")))
      .build

    val combined = serverA <+> serverB
    for
      content <- combined.readResource("file:///test.txt")
    yield
      assertEquals(content.text, Some("content A"))
  }

  test("combine delegates to right server when left doesn't have resource") {
    val combined = serverWithResource <+> serverWithConfigResource
    for
      content <- combined.readResource("file:///config.json")
    yield
      assertEquals(content.text, Some("{}"))
  }

  // === Prompt Composition ===

  test("combine merges prompts from both servers") {
    val combined = serverWithGreetingPrompt <+> serverWithFarewellPrompt
    for
      prompts <- combined.listPrompts
    yield
      assertEquals(prompts.map(_.name).toSet, Set("greeting", "farewell"))
  }

  test("combine prefers left server's prompt on name conflict") {
    val serverA = McpServer.builder[IO]
      .withInfo(ServerInfo("a", "1.0.0"))
      .withPrompt(greetingPrompt, _ =>
        IO.pure(GetPromptResult(Some("A"), List(PromptMessage(Role.User, TextContent("A")))))
      )
      .build

    val serverB = McpServer.builder[IO]
      .withInfo(ServerInfo("b", "1.0.0"))
      .withPrompt(greetingPrompt, _ =>
        IO.pure(GetPromptResult(Some("B"), List(PromptMessage(Role.User, TextContent("B")))))
      )
      .build

    val combined = serverA <+> serverB
    for
      result <- combined.getPrompt("greeting", Map.empty)
    yield
      assertEquals(result.description, Some("A"))
  }

  test("combine delegates to right server when left doesn't have prompt") {
    val combined = serverWithGreetingPrompt <+> serverWithFarewellPrompt
    for
      result <- combined.getPrompt("farewell", Map("name" -> "Alice"))
    yield
      assert(result.messages.head.content.asInstanceOf[TextContent].text.contains("Goodbye"))
  }

  // === Capabilities Merging ===

  test("combine merges capabilities correctly") {
    val toolsOnly = serverWithAdd
    val resourcesOnly = serverWithResource

    val combined = toolsOnly <+> resourcesOnly
    assert(combined.capabilities.tools.isDefined)
    assert(combined.capabilities.resources.isDefined)
  }

  test("combine with empty server preserves capabilities") {
    val combined = serverWithAdd <+> emptyServer
    assert(combined.capabilities.tools.isDefined)
  }

  // === Semigroup Laws ===

  test("Semigroup associativity holds for listTools") {
    val a = serverWithAdd
    val b = serverWithSubtract
    val c = serverWithResource

    for
      abc1 <- ((a <+> b) <+> c).listTools
      abc2 <- (a <+> (b <+> c)).listTools
    yield
      assertEquals(abc1.map(_.name).toSet, abc2.map(_.name).toSet)
  }

  test("combine using cats Semigroup syntax") {
    val combined = serverWithAdd |+| serverWithSubtract
    for
      tools <- combined.listTools
    yield
      assertEquals(tools.map(_.name).toSet, Set("add", "subtract"))
  }

  // === Edge Cases ===

  test("combining empty servers produces empty server") {
    val combined = emptyServer <+> emptyServer
    for
      tools <- combined.listTools
      resources <- combined.listResources
      prompts <- combined.listPrompts
    yield
      assert(tools.isEmpty)
      assert(resources.isEmpty)
      assert(prompts.isEmpty)
  }

  test("withInfo preserves tool functionality") {
    val combined = (serverWithAdd <+> serverWithSubtract).withInfo(ServerInfo("new", "1.0.0"))
    for
      result <- combined.callTool("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson))
    yield
      assertEquals(result.textContent, "5")
  }
