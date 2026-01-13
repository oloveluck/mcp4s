package mcp4s.protocol

import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import munit.FunSuite
import mcp4s.protocol.Codecs.given

class CodecsSpec extends FunSuite:

  // === RequestId Tests ===

  test("RequestId.StringId encodes to JSON string") {
    val id = RequestId.StringId("test-123")
    assertEquals(id.asJson, Json.fromString("test-123"))
  }

  test("RequestId.NumberId encodes to JSON number") {
    val id = RequestId.NumberId(42L)
    assertEquals(id.asJson, Json.fromLong(42L))
  }

  test("RequestId.NullId encodes to JSON null") {
    assertEquals(RequestId.NullId.asJson, Json.Null)
  }

  test("RequestId decodes from JSON string") {
    val json = Json.fromString("abc")
    assertEquals(json.as[RequestId], Right(RequestId.StringId("abc")))
  }

  test("RequestId decodes from JSON number") {
    val json = Json.fromLong(123)
    assertEquals(json.as[RequestId], Right(RequestId.NumberId(123)))
  }

  // === JsonRpcRequest Tests ===

  test("JsonRpcRequest encodes correctly") {
    val req = JsonRpcRequest(
      id = RequestId.StringId("1"),
      method = "test/method",
      params = Some(Json.obj("key" -> Json.fromString("value")))
    )
    val json = req.asJson
    assertEquals(json.hcursor.get[String]("jsonrpc"), Right("2.0"))
    assertEquals(json.hcursor.get[String]("id"), Right("1"))
    assertEquals(json.hcursor.get[String]("method"), Right("test/method"))
  }

  test("JsonRpcRequest decodes correctly") {
    val json = parse("""{"jsonrpc":"2.0","id":"1","method":"test"}""").toOption.get
    val req = json.as[JsonRpcRequest]
    assert(req.isRight)
    assertEquals(req.toOption.get.method, "test")
  }

  // === JsonRpcResponse Tests ===

  test("JsonRpcResponse encodes correctly") {
    val resp = JsonRpcResponse(
      id = RequestId.NumberId(1),
      result = Json.obj("data" -> Json.fromString("test"))
    )
    val json = resp.asJson
    assertEquals(json.hcursor.get[String]("jsonrpc"), Right("2.0"))
    assertEquals(json.hcursor.get[Long]("id"), Right(1L))
    assert(json.hcursor.downField("result").succeeded)
  }

  // === JsonRpcError Tests ===

  test("JsonRpcError encodes correctly") {
    val err = JsonRpcError(-32600, "Invalid Request", None)
    val json = err.asJson
    assertEquals(json.hcursor.get[Int]("code"), Right(-32600))
    assertEquals(json.hcursor.get[String]("message"), Right("Invalid Request"))
  }

  // === Tool Tests ===

  test("Tool encodes correctly") {
    val tool = Tool(
      name = "test-tool",
      description = Some("A test tool"),
      inputSchema = JsonSchema.obj(
        Map("input" -> JsonSchema.string(Some("Input value"))),
        List("input")
      )
    )
    val json = tool.asJson
    assertEquals(json.hcursor.get[String]("name"), Right("test-tool"))
    assertEquals(json.hcursor.get[Option[String]]("description"), Right(Some("A test tool")))
  }

  // === Content Tests ===

  test("TextContent encodes correctly") {
    val content: Content = TextContent("Hello, world!")
    val json = content.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("text"))
    assertEquals(json.hcursor.get[String]("text"), Right("Hello, world!"))
  }

  test("TextContent decodes correctly") {
    val json = parse("""{"type":"text","text":"test"}""").toOption.get
    val content = json.as[Content]
    assertEquals(content, Right(TextContent("test")))
  }

  // === ToolResult Tests ===

  test("ToolResult.text creates text result") {
    val result = ToolResult.text("Success")
    assertEquals(result.content.length, 1)
    assertEquals(result.isError, false)
    assert(result.content.head.isInstanceOf[TextContent])
  }

  test("ToolResult.error creates error result") {
    val result = ToolResult.error("Something went wrong")
    assertEquals(result.isError, true)
  }

  // === Role Tests ===

  test("Role encodes correctly") {
    assertEquals(Role.User.asJson, Json.fromString("user"))
    assertEquals(Role.Assistant.asJson, Json.fromString("assistant"))
  }

  test("Role decodes correctly") {
    assertEquals(Json.fromString("user").as[Role], Right(Role.User))
    assertEquals(Json.fromString("assistant").as[Role], Right(Role.Assistant))
  }

  // === ServerCapabilities Tests ===

  test("ServerCapabilities.empty has no capabilities") {
    val caps = ServerCapabilities.empty
    assertEquals(caps.tools, None)
    assertEquals(caps.resources, None)
    assertEquals(caps.prompts, None)
  }

  test("ServerCapabilities.withTools has tools capability") {
    val caps = ServerCapabilities.withTools
    assert(caps.tools.isDefined)
    assertEquals(caps.resources, None)
  }

  // === InitializeResult Tests ===

  test("InitializeResult encodes correctly") {
    val result = InitializeResult(
      protocolVersion = McpVersion.Current,
      capabilities = ServerCapabilities.withTools,
      serverInfo = ServerInfo("test", "1.0")
    )
    val json = result.asJson
    assertEquals(json.hcursor.get[String]("protocolVersion"), Right(McpVersion.Current))
    assert(json.hcursor.downField("capabilities").downField("tools").succeeded)
  }

  // === Root Tests (Client Feature) ===

  test("Root roundtrip") {
    val root = Root("file:///home/user/workspace", Some("Workspace"))
    val json = root.asJson
    assertEquals(json.as[Root], Right(root))
  }

  test("ListRootsResult roundtrip") {
    val result = ListRootsResult(List(
      Root("file:///home/user/project", Some("Project")),
      Root("file:///tmp/test")
    ))
    val json = result.asJson
    assertEquals(json.as[ListRootsResult], Right(result))
  }

  // === Sampling Type Tests ===

  test("SamplingTextContent roundtrip") {
    val content: SamplingContent = SamplingTextContent("Hello, world!")
    val json = content.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("text"))
    assertEquals(json.as[SamplingContent], Right(content))
  }

  test("SamplingImageContent roundtrip") {
    val content: SamplingContent = SamplingImageContent("base64data", "image/png")
    val json = content.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("image"))
    assertEquals(json.as[SamplingContent], Right(content))
  }

  test("SamplingAudioContent roundtrip") {
    val content: SamplingContent = SamplingAudioContent("audiodata", "audio/wav")
    val json = content.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("audio"))
    assertEquals(json.as[SamplingContent], Right(content))
  }

  test("ToolUseContent roundtrip") {
    val content: SamplingContent = ToolUseContent("tool-1", "calculate", Json.obj("x" -> Json.fromInt(5)))
    val json = content.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("tool_use"))
    assertEquals(json.as[SamplingContent], Right(content))
  }

  test("ToolResultContent roundtrip") {
    val content: SamplingContent = ToolResultContent("tool-1", List(TextContent("result")), false)
    val json = content.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("tool_result"))
    assertEquals(json.as[SamplingContent], Right(content))
  }

  test("SamplingMessage roundtrip") {
    val msg = SamplingMessage(Role.User, SamplingTextContent("Hello"))
    val json = msg.asJson
    assertEquals(json.as[SamplingMessage], Right(msg))
  }

  test("ModelPreferences roundtrip") {
    val prefs = ModelPreferences(
      hints = Some(List(ModelHint(Some("claude-3")))),
      costPriority = Some(0.5),
      speedPriority = Some(0.8),
      intelligencePriority = Some(0.9)
    )
    val json = prefs.asJson
    assertEquals(json.as[ModelPreferences], Right(prefs))
  }

  test("ToolChoice.Auto roundtrip") {
    val choice: ToolChoice = ToolChoice.Auto
    val json = choice.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("auto"))
    assertEquals(json.as[ToolChoice], Right(choice))
  }

  test("ToolChoice.None roundtrip") {
    val choice: ToolChoice = ToolChoice.None
    val json = choice.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("none"))
    assertEquals(json.as[ToolChoice], Right(choice))
  }

  test("ToolChoice.Specific roundtrip") {
    val choice: ToolChoice = ToolChoice.Specific("my_tool")
    val json = choice.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("tool"))
    assertEquals(json.hcursor.get[String]("name"), Right("my_tool"))
    assertEquals(json.as[ToolChoice], Right(choice))
  }

  test("CreateMessageParams roundtrip") {
    val params = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("Hello"))),
      maxTokens = 1000,
      modelPreferences = Some(ModelPreferences(costPriority = Some(0.5))),
      systemPrompt = Some("You are helpful"),
      temperature = Some(0.7)
    )
    val json = params.asJson
    assertEquals(json.as[CreateMessageParams], Right(params))
  }

  test("CreateMessageResult roundtrip") {
    val result = CreateMessageResult(
      role = Role.Assistant,
      content = SamplingTextContent("Hello!"),
      model = "claude-3-sonnet",
      stopReason = Some("endTurn")
    )
    val json = result.asJson
    assertEquals(json.as[CreateMessageResult], Right(result))
  }

  // === Elicitation Type Tests ===

  test("ElicitFormParams roundtrip") {
    val params: ElicitParams = ElicitFormParams(
      message = "Enter your name",
      requestedSchema = JsonSchema.obj(
        Map("name" -> JsonSchema.string(Some("Your name"))),
        List("name")
      )
    )
    val json = params.asJson
    assertEquals(json.hcursor.get[String]("mode"), Right("form"))
    assertEquals(json.as[ElicitParams], Right(params))
  }

  test("ElicitUrlParams roundtrip") {
    val params: ElicitParams = ElicitUrlParams(
      message = "Complete authentication",
      elicitationId = "auth-123",
      url = "https://example.com/auth"
    )
    val json = params.asJson
    assertEquals(json.hcursor.get[String]("mode"), Right("url"))
    assertEquals(json.as[ElicitParams], Right(params))
  }

  test("ElicitAction roundtrip") {
    assertEquals((ElicitAction.Accept: ElicitAction).asJson, Json.fromString("accept"))
    assertEquals((ElicitAction.Decline: ElicitAction).asJson, Json.fromString("decline"))
    assertEquals((ElicitAction.Cancel: ElicitAction).asJson, Json.fromString("cancel"))
    assertEquals(Json.fromString("accept").as[ElicitAction], Right(ElicitAction.Accept))
  }

  test("ElicitResult roundtrip") {
    val result = ElicitResult(
      action = ElicitAction.Accept,
      content = Some(Map("name" -> Json.fromString("Alice")))
    )
    val json = result.asJson
    assertEquals(json.as[ElicitResult], Right(result))
  }

  // === Content Type Tests ===

  test("ImageContent encodes correctly") {
    val content: Content = ImageContent("base64data", "image/png")
    val json = content.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("image"))
    assertEquals(json.hcursor.get[String]("data"), Right("base64data"))
    assertEquals(json.hcursor.get[String]("mimeType"), Right("image/png"))
  }

  test("ImageContent decodes correctly") {
    val json = parse("""{"type":"image","data":"abc","mimeType":"image/jpeg"}""").toOption.get
    val content = json.as[Content]
    assertEquals(content, Right(ImageContent("abc", "image/jpeg")))
  }

  test("AudioContent encodes correctly") {
    val content: Content = AudioContent("audiodata", "audio/mp3")
    val json = content.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("audio"))
    assertEquals(json.hcursor.get[String]("data"), Right("audiodata"))
  }

  test("AudioContent decodes correctly") {
    val json = parse("""{"type":"audio","data":"wav","mimeType":"audio/wav"}""").toOption.get
    val content = json.as[Content]
    assertEquals(content, Right(AudioContent("wav", "audio/wav")))
  }

  test("ResourceLinkContent roundtrip") {
    val content: Content = ResourceLinkContent(
      uri = "file:///doc.txt",
      name = "Document",
      title = Some("My Document"),
      mimeType = Some("text/plain"),
      size = Some(1024)
    )
    val json = content.asJson
    assertEquals(json.hcursor.get[String]("type"), Right("resource_link"))
    assertEquals(json.as[Content], Right(content))
  }

  // === Complex Types Tests ===

  test("ServerCapabilities with all options") {
    val caps = ServerCapabilities(
      tools = Some(ToolsCapability(listChanged = Some(true))),
      resources = Some(ResourcesCapability(subscribe = Some(true), listChanged = Some(true))),
      prompts = Some(PromptsCapability(listChanged = Some(false))),
      logging = Some(LoggingCapability()),
      completions = Some(CompletionsCapability())
    )
    val json = caps.asJson
    assertEquals(json.as[ServerCapabilities], Right(caps))
  }

  test("ClientCapabilities with all options") {
    val caps = ClientCapabilities(
      roots = Some(RootsCapability(listChanged = Some(true))),
      sampling = Some(SamplingCapability()),
      elicitation = Some(ElicitationCapability())
    )
    val json = caps.asJson
    assertEquals(json.as[ClientCapabilities], Right(caps))
  }

  test("Tool with annotations") {
    val tool = Tool(
      name = "file_read",
      description = Some("Read a file"),
      inputSchema = JsonSchema.obj(
        Map("path" -> JsonSchema.string(Some("File path"))),
        List("path")
      ),
      title = Some("File Reader"),
      annotations = Some(ToolAnnotations(
        readOnlyHint = Some(true),
        destructiveHint = Some(false),
        idempotentHint = Some(true)
      ))
    )
    val json = tool.asJson
    assertEquals(json.as[Tool], Right(tool))
  }

  test("Resource with annotations") {
    val resource = Resource(
      uri = "file:///test.txt",
      name = "Test File",
      title = Some("Test"),
      mimeType = Some("text/plain"),
      annotations = Some(Annotations(
        audience = Some(List("user")),
        priority = Some(0.8)
      )),
      size = Some(2048)
    )
    val json = resource.asJson
    assertEquals(json.as[Resource], Right(resource))
  }

  // === Error Cases ===

  test("Unknown content type returns error") {
    val json = parse("""{"type":"unknown","data":"test"}""").toOption.get
    assert(json.as[Content].isLeft)
  }

  test("Missing required field fails") {
    val json = parse("""{"type":"text"}""").toOption.get  // Missing "text" field
    assert(json.as[Content].isLeft)
  }

  test("Unknown role returns error") {
    val json = Json.fromString("admin")
    assert(json.as[Role].isLeft)
  }

  test("Unknown ToolChoice type returns error") {
    val json = parse("""{"type":"invalid"}""").toOption.get
    assert(json.as[ToolChoice].isLeft)
  }

  // === LogLevel Tests ===

  test("LogLevel roundtrip") {
    assertEquals(LogLevel.Debug.asJson, Json.fromString("debug"))
    assertEquals(LogLevel.Error.asJson, Json.fromString("error"))
    assertEquals(Json.fromString("warning").as[LogLevel], Right(LogLevel.Warning))
  }

  // === Cursor Tests ===

  test("Cursor roundtrip") {
    val cursor = Cursor("abc123")
    val json = cursor.asJson
    assertEquals(json, Json.fromString("abc123"))
    assertEquals(json.as[Cursor], Right(cursor))
  }
