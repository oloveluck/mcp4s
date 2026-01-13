package mcp4s.client

import cats.effect.{Deferred, IO, Ref}
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import munit.CatsEffectSuite

class McpConnectionSpec extends CatsEffectSuite:

  // === Test Fixtures ===

  val testServerInfo: ServerInfo = ServerInfo("test-server", "1.0.0")

  val testServerCapabilities: ServerCapabilities = ServerCapabilities(
    tools = Some(ToolsCapability()),
    resources = Some(ResourcesCapability()),
    prompts = Some(PromptsCapability())
  )

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

  val testResourceTemplate: ResourceTemplate = ResourceTemplate(
    uriTemplate = "file:///{path}",
    name = "File Template"
  )

  val testPrompt: Prompt = Prompt(
    name = "greeting",
    description = Some("A greeting prompt"),
    arguments = List(PromptArgument("name", Some("User name"), required = true))
  )

  // === Mock Connection Factory ===

  def createConnection(handler: JsonRpcRequest => IO[Json]): IO[McpConnection[IO]] =
    createConnection(handler, _ => IO.unit)

  def createConnection(
      handler: JsonRpcRequest => IO[Json],
      notificationHandler: JsonRpcNotification => IO[Unit]
  ): IO[McpConnection[IO]] =
    for
      requestIdGen <- Ref.of[IO, Long](0L)
      inFlightRef <- Ref.of[IO, Map[RequestId, Deferred[IO, Unit]]](Map.empty)
    yield new McpConnectionImpl[IO](
      testServerInfo,
      testServerCapabilities,
      handler,
      notificationHandler,
      requestIdGen,
      inFlightRef
    )

  def mockResponse(method: String, response: Json): JsonRpcRequest => IO[Json] =
    req => if req.method == method then IO.pure(response) else IO.raiseError(new RuntimeException(s"Unexpected method: ${req.method}"))

  // === Server Info Tests ===

  test("connection exposes server info") {
    createConnection(_ => IO.pure(Json.obj())).map { conn =>
      assertEquals(conn.serverInfo.name, "test-server")
      assertEquals(conn.serverInfo.version, "1.0.0")
    }
  }

  test("connection exposes server capabilities") {
    createConnection(_ => IO.pure(Json.obj())).map { conn =>
      assert(conn.serverCapabilities.tools.isDefined)
      assert(conn.serverCapabilities.resources.isDefined)
      assert(conn.serverCapabilities.prompts.isDefined)
    }
  }

  // === List Tools Tests ===

  test("listTools parses response correctly") {
    val response = Json.obj("tools" -> List(testTool).asJson)
    for
      conn <- createConnection(mockResponse(McpMethod.ToolsList, response))
      tools <- conn.listTools
    yield
      assertEquals(tools.length, 1)
      assertEquals(tools.head.name, "calculate")
  }

  test("listTools returns empty list") {
    val response = Json.obj("tools" -> Json.arr())
    for
      conn <- createConnection(mockResponse(McpMethod.ToolsList, response))
      tools <- conn.listTools
    yield assertEquals(tools.length, 0)
  }

  // === Call Tool Tests ===

  test("callTool sends correct request format") {
    var capturedRequest: Option[JsonRpcRequest] = None
    val response = ToolResult.text("42").asJson

    for
      conn <- createConnection { req =>
        capturedRequest = Some(req)
        IO.pure(response)
      }
      _ <- conn.callTool("calculate", Map("expression" -> "1+1"))
    yield
      assert(capturedRequest.isDefined)
      assertEquals(capturedRequest.get.method, McpMethod.ToolsCall)
      val params = capturedRequest.get.params.get
      assertEquals(params.hcursor.get[String]("name"), Right("calculate"))
  }

  test("callTool parses result correctly") {
    val response = ToolResult.text("42").asJson
    for
      conn <- createConnection(mockResponse(McpMethod.ToolsCall, response))
      result <- conn.callTool("calculate", Map.empty[String, String])
    yield
      assertEquals(result.isError, false)
      assertEquals(result.content.length, 1)
  }

  test("callTool handles error result") {
    val response = ToolResult.error("Calculation failed").asJson
    for
      conn <- createConnection(mockResponse(McpMethod.ToolsCall, response))
      result <- conn.callTool("calculate", Map.empty[String, String])
    yield assertEquals(result.isError, true)
  }

  // === List Resources Tests ===

  test("listResources parses response correctly") {
    val response = Json.obj("resources" -> List(testResource).asJson)
    for
      conn <- createConnection(mockResponse(McpMethod.ResourcesList, response))
      resources <- conn.listResources
    yield
      assertEquals(resources.length, 1)
      assertEquals(resources.head.uri, "file:///test.txt")
  }

  test("listResourceTemplates parses response correctly") {
    val response = Json.obj("resourceTemplates" -> List(testResourceTemplate).asJson)
    for
      conn <- createConnection(mockResponse(McpMethod.ResourcesTemplatesList, response))
      templates <- conn.listResourceTemplates
    yield
      assertEquals(templates.length, 1)
      assertEquals(templates.head.uriTemplate, "file:///{path}")
  }

  // === Read Resource Tests ===

  test("readResource returns first content") {
    val content = ResourceContent.text("file:///test.txt", "Test content")
    val response = Json.obj("contents" -> List(content).asJson)
    for
      conn <- createConnection(mockResponse(McpMethod.ResourcesRead, response))
      result <- conn.readResource("file:///test.txt")
    yield
      assertEquals(result.uri, "file:///test.txt")
      assertEquals(result.text, Some("Test content"))
  }

  test("readResource sends correct URI") {
    var capturedRequest: Option[JsonRpcRequest] = None
    val content = ResourceContent.text("file:///data.json", "{}")
    val response = Json.obj("contents" -> List(content).asJson)

    for
      conn <- createConnection { req =>
        capturedRequest = Some(req)
        IO.pure(response)
      }
      _ <- conn.readResource("file:///data.json")
    yield
      assert(capturedRequest.isDefined)
      val params = capturedRequest.get.params.get
      assertEquals(params.hcursor.get[String]("uri"), Right("file:///data.json"))
  }

  test("readResource errors on empty contents") {
    val response = Json.obj("contents" -> Json.arr())
    for
      conn <- createConnection(mockResponse(McpMethod.ResourcesRead, response))
      result <- conn.readResource("file:///missing.txt").attempt
    yield
      assert(result.isLeft)
      result.left.toOption.get match
        case McpError.ResourceNotFound(uri) =>
          assertEquals(uri, "file:///missing.txt")
        case other =>
          fail(s"Expected ResourceNotFound, got $other")
  }

  // === List Prompts Tests ===

  test("listPrompts parses response correctly") {
    val response = Json.obj("prompts" -> List(testPrompt).asJson)
    for
      conn <- createConnection(mockResponse(McpMethod.PromptsList, response))
      prompts <- conn.listPrompts
    yield
      assertEquals(prompts.length, 1)
      assertEquals(prompts.head.name, "greeting")
  }

  // === Get Prompt Tests ===

  test("getPrompt sends arguments correctly") {
    var capturedRequest: Option[JsonRpcRequest] = None
    val response = GetPromptResult(
      Some("A greeting"),
      List(PromptMessage(Role.User, TextContent("Hello, Alice!")))
    ).asJson

    for
      conn <- createConnection { req =>
        capturedRequest = Some(req)
        IO.pure(response)
      }
      _ <- conn.getPrompt("greeting", Map("name" -> "Alice"))
    yield
      assert(capturedRequest.isDefined)
      val params = capturedRequest.get.params.get
      assertEquals(params.hcursor.get[String]("name"), Right("greeting"))
      assertEquals(params.hcursor.downField("arguments").get[String]("name"), Right("Alice"))
  }

  test("getPrompt parses response correctly") {
    val response = GetPromptResult(
      Some("A greeting"),
      List(PromptMessage(Role.User, TextContent("Hello!")))
    ).asJson
    for
      conn <- createConnection(mockResponse(McpMethod.PromptsGet, response))
      result <- conn.getPrompt("greeting", Map.empty[String, String])
    yield
      assertEquals(result.description, Some("A greeting"))
      assertEquals(result.messages.length, 1)
  }

  // === Ping Tests ===

  test("ping sends request") {
    var pinged = false
    for
      conn <- createConnection { _ =>
        pinged = true
        IO.pure(Json.obj())
      }
      _ <- conn.ping
    yield assert(pinged)
  }

  test("ping completes successfully") {
    for
      conn <- createConnection(mockResponse(McpMethod.Ping, Json.obj()))
      _ <- conn.ping
    yield ()
  }

  // === Shutdown Tests ===

  test("shutdown sends request") {
    var shutdownCalled = false
    for
      conn <- createConnection { req =>
        if req.method == McpMethod.Shutdown then shutdownCalled = true
        IO.pure(Json.obj())
      }
      _ <- conn.shutdown
    yield assert(shutdownCalled)
  }

  // === Request ID Generation Tests ===

  test("request IDs are incremented") {
    val ids = scala.collection.mutable.ArrayBuffer[Long]()
    for
      conn <- createConnection { req =>
        req.id match
          case RequestId.NumberId(n) => ids += n
          case _ => ()
        IO.pure(Json.obj("tools" -> Json.arr()))
      }
      _ <- conn.listTools
      _ <- conn.listTools
      _ <- conn.listTools
    yield
      assertEquals(ids.toList, List(1L, 2L, 3L))
  }

  // === Error Handling Tests ===

  test("connection propagates transport errors") {
    for
      conn <- createConnection { _ =>
        IO.raiseError(new RuntimeException("Transport error"))
      }
      result <- conn.listTools.attempt
    yield
      assert(result.isLeft)
      assert(result.left.toOption.get.getMessage.contains("Transport error"))
  }

  test("connection handles malformed response") {
    for
      conn <- createConnection(_ => IO.pure(Json.obj("invalid" -> Json.fromString("response"))))
      result <- conn.listTools.attempt
    yield assert(result.isLeft)
  }

  // === Cancel Tests ===

  test("cancel sends cancellation notification") {
    var sentNotification: Option[JsonRpcNotification] = None

    for
      conn <- createConnection(
        _ => IO.pure(Json.obj()),
        notif => IO { sentNotification = Some(notif) }
      )
      _ <- conn.cancel(RequestId.NumberId(42), Some("User cancelled"))
    yield
      assert(sentNotification.isDefined)
      assertEquals(sentNotification.get.method, McpMethod.Cancelled)
      val params = sentNotification.get.params.get.as[CancelledParams]
      assert(params.isRight)
      assertEquals(params.toOption.get.requestId, RequestId.NumberId(42))
      assertEquals(params.toOption.get.reason, Some("User cancelled"))
  }

  test("cancel works without reason") {
    var sentNotification: Option[JsonRpcNotification] = None

    for
      conn <- createConnection(
        _ => IO.pure(Json.obj()),
        notif => IO { sentNotification = Some(notif) }
      )
      _ <- conn.cancel(RequestId.StringId("req-123"))
    yield
      assert(sentNotification.isDefined)
      val params = sentNotification.get.params.get.as[CancelledParams]
      assertEquals(params.toOption.get.requestId, RequestId.StringId("req-123"))
      assertEquals(params.toOption.get.reason, None)
  }

  // === Fiber Cancellation Tests ===

  test("fiber cancellation sends notification") {
    import scala.concurrent.duration.*
    var sentNotification: Option[JsonRpcNotification] = None

    for
      conn <- createConnection(
        _ => IO.never,  // Never completes
        notif => IO { sentNotification = Some(notif) }
      )
      fiber <- conn.callTool("slow", Map.empty[String, String]).start
      _ <- IO.sleep(50.millis)
      _ <- fiber.cancel
    yield
      assert(sentNotification.isDefined)
      assertEquals(sentNotification.get.method, McpMethod.Cancelled)
  }
