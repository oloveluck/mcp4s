package mcp4s.server

import cats.effect.IO
import io.circe.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import munit.CatsEffectSuite

class DispatcherSpec extends CatsEffectSuite:

  given Tracer[IO] = Tracer.noop[IO]

  // === Test Fixtures ===

  val testTool: Tool = Tool(
    name = "calculate",
    description = Some("Perform calculations"),
    inputSchema = JsonSchema.obj(
      Map("expression" -> JsonSchema.string(Some("Math expression"))),
      List("expression")
    )
  )

  val testResource: Resource = Resource(
    uri = "file:///test.txt",
    name = "Test File",
    mimeType = Some("text/plain")
  )

  val testPrompt: Prompt = Prompt(
    name = "greeting",
    description = Some("A greeting prompt"),
    arguments = List(PromptArgument("name", Some("User name"), required = true))
  )

  def testServer: McpServer[IO] = new McpServer[IO]:
    val info: ServerInfo = ServerInfo("test-server", "1.0.0")
    val capabilities: ServerCapabilities = ServerCapabilities(
      tools = Some(ToolsCapability()),
      resources = Some(ResourcesCapability()),
      prompts = Some(PromptsCapability())
    )

    def listTools: IO[List[Tool]] = IO.pure(List(testTool))

    def callTool(name: String, arguments: Json): IO[ToolResult] =
      if name == "calculate" then IO.pure(ToolResult.text("42"))
      else IO.raiseError(McpError.ToolNotFound(name))

    def listResources: IO[List[Resource]] = IO.pure(List(testResource))

    def listResourceTemplates: IO[List[ResourceTemplate]] = IO.pure(Nil)

    def readResource(uri: String): IO[ResourceContent] =
      if uri == "file:///test.txt" then IO.pure(ResourceContent.text("file:///test.txt", "Test content"))
      else IO.raiseError(McpError.ResourceNotFound(uri))

    def listPrompts: IO[List[Prompt]] = IO.pure(List(testPrompt))

    def getPrompt(name: String, arguments: Map[String, String]): IO[GetPromptResult] =
      if name == "greeting" then
        IO.pure(GetPromptResult(
          Some("A greeting"),
          List(PromptMessage(Role.User, TextContent(s"Hello, ${arguments.getOrElse("name", "World")}!")))
        ))
      else IO.raiseError(McpError.PromptNotFound(name))

  // === Helper Methods ===

  def createDispatcher: IO[Dispatcher[IO]] = Dispatcher[IO](testServer)

  def sendRequest(dispatcher: Dispatcher[IO], method: String, params: Json = Json.obj()): IO[JsonRpcMessage] =
    val req = JsonRpcRequest(RequestId.NumberId(1), method, Some(params))
    dispatcher.dispatch(req).map(_.get)

  def sendNotification(dispatcher: Dispatcher[IO], method: String, params: Json = Json.obj()): IO[Unit] =
    val notif = JsonRpcNotification(method, Some(params))
    dispatcher.dispatch(notif).void

  // === Helper for valid init params ===

  val validInitParams: Json = Json.obj(
    "protocolVersion" -> Json.fromString("2025-11-25"),
    "capabilities" -> Json.obj(),
    "clientInfo" -> Json.obj("name" -> Json.fromString("test-client"), "version" -> Json.fromString("1.0"))
  )

  // === Lifecycle Tests ===

  test("handleInitialize returns server info and capabilities") {
    for
      dispatcher <- createDispatcher
      response <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
    yield
      response match
        case JsonRpcResponse(_, result) =>
          assertEquals(result.hcursor.get[String]("protocolVersion"), Right("2025-11-25"))
          assert(result.hcursor.downField("serverInfo").get[String]("name").contains("test-server"))
          assert(result.hcursor.downField("capabilities").downField("tools").succeeded)
        case _ => fail("Expected response")
  }

  test("handleInitialize validates protocol version") {
    for
      dispatcher <- createDispatcher
      response <- sendRequest(dispatcher, McpMethod.Initialize, Json.obj(
        "protocolVersion" -> Json.fromString("invalid-version"),
        "capabilities" -> Json.obj(),
        "clientInfo" -> Json.obj("name" -> Json.fromString("test"), "version" -> Json.fromString("1.0"))
      ))
    yield
      response match
        case JsonRpcErrorResponse(_, error) =>
          assertEquals(error.code, JsonRpcErrorCode.InvalidRequest)
          assert(error.message.contains("Protocol version mismatch"))
        case _ => fail("Expected error response for invalid protocol version")
  }

  test("handleInitialize accepts backwards compatible version") {
    for
      dispatcher <- createDispatcher
      response <- sendRequest(dispatcher, McpMethod.Initialize, Json.obj(
        "protocolVersion" -> Json.fromString("2024-11-05"),
        "capabilities" -> Json.obj(),
        "clientInfo" -> Json.obj("name" -> Json.fromString("test"), "version" -> Json.fromString("1.0"))
      ))
    yield
      response match
        case JsonRpcResponse(_, result) =>
          // Server responds with its own version
          assertEquals(result.hcursor.get[String]("protocolVersion"), Right("2025-11-25"))
        case _ => fail("Expected response for compatible version")
  }

  test("initialized notification is accepted after initialize") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
    yield ()
  }

  test("double initialize returns error") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
    yield
      response match
        case JsonRpcErrorResponse(_, error) =>
          assertEquals(error.code, JsonRpcErrorCode.InvalidRequest)
        case _ => fail("Expected error response")
  }

  test("shutdown returns empty result") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.Shutdown)
    yield
      response match
        case JsonRpcResponse(_, result) =>
          assertEquals(result, Json.obj())
        case _ => fail("Expected response")
  }

  // === Tool Tests ===

  test("tools/list returns registered tools") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.ToolsList)
    yield
      response match
        case JsonRpcResponse(_, result) =>
          val tools = result.hcursor.get[List[Tool]]("tools")
          assert(tools.isRight)
          assertEquals(tools.toOption.get.length, 1)
          assertEquals(tools.toOption.get.head.name, "calculate")
        case _ => fail("Expected response")
  }

  test("tools/call invokes correct handler") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.ToolsCall, Json.obj(
        "name" -> Json.fromString("calculate"),
        "arguments" -> Json.obj("expression" -> Json.fromString("1+1"))
      ))
    yield
      response match
        case JsonRpcResponse(_, result) =>
          val toolResult = result.as[ToolResult]
          assert(toolResult.isRight)
          assertEquals(toolResult.toOption.get.isError, false)
        case _ => fail("Expected response")
  }

  test("tools/call returns error for unknown tool") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.ToolsCall, Json.obj(
        "name" -> Json.fromString("unknown_tool"),
        "arguments" -> Json.obj()
      ))
    yield
      response match
        case JsonRpcErrorResponse(_, error) =>
          assertEquals(error.code, JsonRpcErrorCode.MethodNotFound)
        case _ => fail("Expected error response")
  }

  test("tools/list requires initialization") {
    for
      dispatcher <- createDispatcher
      response <- sendRequest(dispatcher, McpMethod.ToolsList)
    yield
      response match
        case JsonRpcErrorResponse(_, error) =>
          assertEquals(error.code, JsonRpcErrorCode.InvalidRequest)
        case _ => fail("Expected error response")
  }

  // === Resource Tests ===

  test("resources/list returns registered resources") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.ResourcesList)
    yield
      response match
        case JsonRpcResponse(_, result) =>
          val resources = result.hcursor.get[List[Resource]]("resources")
          assert(resources.isRight)
          assertEquals(resources.toOption.get.length, 1)
        case _ => fail("Expected response")
  }

  test("resources/read returns resource content") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.ResourcesRead, Json.obj(
        "uri" -> Json.fromString("file:///test.txt")
      ))
    yield
      response match
        case JsonRpcResponse(_, result) =>
          val contents = result.hcursor.get[List[ResourceContent]]("contents")
          assert(contents.isRight)
          assertEquals(contents.toOption.get.head.text, Some("Test content"))
        case _ => fail("Expected response")
  }

  test("resources/read returns error for unknown resource") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.ResourcesRead, Json.obj(
        "uri" -> Json.fromString("file:///nonexistent.txt")
      ))
    yield
      response match
        case JsonRpcErrorResponse(_, error) =>
          assertEquals(error.code, JsonRpcErrorCode.InvalidParams)
        case _ => fail("Expected error response")
  }

  test("resources/templates/list returns templates") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.ResourcesTemplatesList)
    yield
      response match
        case JsonRpcResponse(_, result) =>
          val templates = result.hcursor.get[List[ResourceTemplate]]("resourceTemplates")
          assert(templates.isRight)
        case _ => fail("Expected response")
  }

  // === Prompt Tests ===

  test("prompts/list returns registered prompts") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.PromptsList)
    yield
      response match
        case JsonRpcResponse(_, result) =>
          val prompts = result.hcursor.get[List[Prompt]]("prompts")
          assert(prompts.isRight)
          assertEquals(prompts.toOption.get.length, 1)
        case _ => fail("Expected response")
  }

  test("prompts/get returns prompt messages") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.PromptsGet, Json.obj(
        "name" -> Json.fromString("greeting"),
        "arguments" -> Json.obj("name" -> Json.fromString("Alice"))
      ))
    yield
      response match
        case JsonRpcResponse(_, result) =>
          val promptResult = result.as[GetPromptResult]
          assert(promptResult.isRight)
          assertEquals(promptResult.toOption.get.messages.length, 1)
        case _ => fail("Expected response")
  }

  test("prompts/get returns error for unknown prompt") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, validInitParams)
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.PromptsGet, Json.obj(
        "name" -> Json.fromString("unknown_prompt")
      ))
    yield
      response match
        case JsonRpcErrorResponse(_, error) =>
          assertEquals(error.code, JsonRpcErrorCode.InvalidParams)
        case _ => fail("Expected error response")
  }

  // === Ping Tests ===

  test("ping returns empty object") {
    for
      dispatcher <- createDispatcher
      response <- sendRequest(dispatcher, McpMethod.Ping)
    yield
      response match
        case JsonRpcResponse(_, result) =>
          assertEquals(result, Json.obj())
        case _ => fail("Expected response")
  }

  // === Error Handling Tests ===

  test("Unknown method returns MethodNotFound error") {
    for
      dispatcher <- createDispatcher
      _ <- sendRequest(dispatcher, McpMethod.Initialize, Json.obj(
        "protocolVersion" -> Json.fromString("2025-11-25"),
        "capabilities" -> Json.obj(),
        "clientInfo" -> Json.obj("name" -> Json.fromString("test"), "version" -> Json.fromString("1.0"))
      ))
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, "unknown/method")
    yield
      response match
        case JsonRpcErrorResponse(_, error) =>
          assertEquals(error.code, JsonRpcErrorCode.MethodNotFound)
          assert(error.message.contains("unknown/method"))
        case JsonRpcResponse(_, _) => fail("Expected error response")
        case _: JsonRpcRequest => fail("Unexpected request")
        case _: JsonRpcNotification => fail("Unexpected notification")
  }

  test("Handler exception returns InternalError") {
    val failingServer = new McpServer[IO]:
      val info: ServerInfo = ServerInfo("failing-server", "1.0.0")
      val capabilities: ServerCapabilities = ServerCapabilities(tools = Some(ToolsCapability()))

      def listTools: IO[List[Tool]] = IO.raiseError(new RuntimeException("Unexpected error"))

      def callTool(name: String, arguments: Json): IO[ToolResult] =
        IO.raiseError(new RuntimeException("Crash"))

      def listResources: IO[List[Resource]] = IO.pure(Nil)
      def listResourceTemplates: IO[List[ResourceTemplate]] = IO.pure(Nil)
      def readResource(uri: String): IO[ResourceContent] = IO.raiseError(new RuntimeException("Crash"))
      def listPrompts: IO[List[Prompt]] = IO.pure(Nil)
      def getPrompt(name: String, arguments: Map[String, String]): IO[GetPromptResult] =
        IO.raiseError(new RuntimeException("Crash"))

    for
      dispatcher <- Dispatcher[IO](failingServer)
      _ <- sendRequest(dispatcher, McpMethod.Initialize, Json.obj())
      _ <- sendNotification(dispatcher, McpMethod.Initialized)
      response <- sendRequest(dispatcher, McpMethod.ToolsList)
    yield
      response match
        case JsonRpcErrorResponse(_, error) =>
          assertEquals(error.code, JsonRpcErrorCode.InternalError)
        case JsonRpcResponse(_, _) => fail("Expected error response, got success")
        case _: JsonRpcRequest => fail("Unexpected request")
        case _: JsonRpcNotification => fail("Unexpected notification")
  }
