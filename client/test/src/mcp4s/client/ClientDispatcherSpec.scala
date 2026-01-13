package mcp4s.client

import cats.effect.IO
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import munit.CatsEffectSuite

class ClientDispatcherSpec extends CatsEffectSuite:

  // === Test Fixtures ===

  val testRoots: List[Root] = List(
    Root("file:///home/user", Some("Home")),
    Root("file:///workspace", Some("Workspace"))
  )

  val testSamplingResult: CreateMessageResult = CreateMessageResult(
    role = Role.Assistant,
    content = SamplingTextContent("Hello!"),
    model = "claude-3-sonnet",
    stopReason = Some("endTurn")
  )

  val testElicitResult: ElicitResult = ElicitResult(
    action = ElicitAction.Accept,
    content = Some(Map("name" -> Json.fromString("Alice")))
  )

  def testClient: McpClient[IO] = McpClient.builder[IO]
    .withInfo(ClientInfo("test-client", "1.0.0"))
    .withRoots(testRoots)
    .withSamplingHandler(_ => IO.pure(testSamplingResult))
    .withElicitationHandler(_ => IO.pure(testElicitResult))
    .build

  def minimalClient: McpClient[IO] = McpClient.builder[IO].build

  // === Helper Methods ===

  def createDispatcher: IO[ClientDispatcher[IO]] =
    ClientDispatcher[IO](testClient)

  def createMinimalDispatcher: IO[ClientDispatcher[IO]] =
    ClientDispatcher[IO](minimalClient)

  def sendRequest(dispatcher: ClientDispatcher[IO], method: String, params: Json = Json.obj()): IO[JsonRpcMessage] =
    val req = JsonRpcRequest(RequestId.NumberId(1), method, Some(params))
    dispatcher.dispatch(req).map(_.get)

  def sendNotification(dispatcher: ClientDispatcher[IO], method: String, params: Json = Json.obj()): IO[Unit] =
    val notif = JsonRpcNotification(method, Some(params))
    dispatcher.dispatch(notif).void

  // === Roots Tests ===

  test("roots/list returns configured roots") {
    for
      dispatcher <- createDispatcher
      response <- sendRequest(dispatcher, McpMethod.RootsList)
    yield
      response match
        case JsonRpcResponse(_, result) =>
          val roots = result.as[ListRootsResult]
          assert(roots.isRight)
          assertEquals(roots.toOption.get.roots.length, 2)
          assertEquals(roots.toOption.get.roots.head.uri, "file:///home/user")
        case _ => fail("Expected response")
  }

  test("roots/list returns empty list when no roots configured") {
    for
      dispatcher <- createMinimalDispatcher
      response <- sendRequest(dispatcher, McpMethod.RootsList)
    yield
      response match
        case JsonRpcResponse(_, result) =>
          val roots = result.as[ListRootsResult]
          assert(roots.isRight)
          assertEquals(roots.toOption.get.roots.length, 0)
        case _ => fail("Expected response")
  }

  // === Sampling Tests ===

  test("sampling/createMessage invokes handler") {
    val params = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("Hi"))),
      maxTokens = 100
    )

    for
      dispatcher <- createDispatcher
      response <- sendRequest(dispatcher, McpMethod.SamplingCreateMessage, params.asJson)
    yield
      response match
        case JsonRpcResponse(_, result) =>
          val msgResult = result.as[CreateMessageResult]
          assert(msgResult.isRight)
          assertEquals(msgResult.toOption.get.model, "claude-3-sonnet")
          assertEquals(msgResult.toOption.get.role, Role.Assistant)
        case _ => fail("Expected response")
  }

  test("sampling/createMessage returns error without handler") {
    val params = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("Hi"))),
      maxTokens = 100
    )

    for
      dispatcher <- createMinimalDispatcher
      response <- sendRequest(dispatcher, McpMethod.SamplingCreateMessage, params.asJson)
    yield
      response match
        case JsonRpcErrorResponse(_, error) =>
          assertEquals(error.code, JsonRpcErrorCode.MethodNotFound)
        case _ => fail("Expected error response")
  }

  // === Elicitation Tests ===

  test("elicitation/create invokes handler") {
    val params = ElicitFormParams(
      message = "Enter your name",
      requestedSchema = JsonSchema.obj(
        Map("name" -> JsonSchema.string(Some("Name"))),
        List("name")
      )
    )

    for
      dispatcher <- createDispatcher
      response <- sendRequest(dispatcher, McpMethod.ElicitationCreate, params.asJson)
    yield
      response match
        case JsonRpcResponse(_, result) =>
          val elicitResult = result.as[ElicitResult]
          assert(elicitResult.isRight)
          assertEquals(elicitResult.toOption.get.action, ElicitAction.Accept)
        case _ => fail("Expected response")
  }

  test("elicitation/create returns error without handler") {
    val params = ElicitFormParams(
      message = "Enter name",
      requestedSchema = JsonSchema.obj(Map.empty, Nil)
    )

    for
      dispatcher <- createMinimalDispatcher
      response <- sendRequest(dispatcher, McpMethod.ElicitationCreate, params.asJson)
    yield
      response match
        case JsonRpcErrorResponse(_, error) =>
          assertEquals(error.code, JsonRpcErrorCode.MethodNotFound)
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

  // === Unknown Method Tests ===

  test("unknown method returns MethodNotFound error") {
    for
      dispatcher <- createDispatcher
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

  // === Request ID Preservation Tests ===

  test("response preserves request ID (number)") {
    for
      dispatcher <- createDispatcher
      req = JsonRpcRequest(RequestId.NumberId(42), McpMethod.Ping, None)
      response <- dispatcher.dispatch(req).map(_.get)
    yield
      response match
        case JsonRpcResponse(id, _) =>
          assertEquals(id, RequestId.NumberId(42))
        case _ => fail("Expected response")
  }

  test("response preserves request ID (string)") {
    for
      dispatcher <- createDispatcher
      req = JsonRpcRequest(RequestId.StringId("req-abc"), McpMethod.Ping, None)
      response <- dispatcher.dispatch(req).map(_.get)
    yield
      response match
        case JsonRpcResponse(id, _) =>
          assertEquals(id, RequestId.StringId("req-abc"))
        case _ => fail("Expected response")
  }

  test("error response preserves request ID") {
    for
      dispatcher <- createDispatcher
      req = JsonRpcRequest(RequestId.NumberId(99), "unknown/method", None)
      response <- dispatcher.dispatch(req).map(_.get)
    yield
      response match
        case JsonRpcErrorResponse(id, _) =>
          assertEquals(id, RequestId.NumberId(99))
        case _ => fail("Expected error response")
  }

  // === Response Type Tests ===

  test("dispatch returns None for notifications") {
    for
      dispatcher <- createDispatcher
      notif = JsonRpcNotification(McpMethod.Progress, None)
      response <- dispatcher.dispatch(notif)
    yield assertEquals(response, None)
  }

  test("dispatch returns Some for requests") {
    for
      dispatcher <- createDispatcher
      req = JsonRpcRequest(RequestId.NumberId(1), McpMethod.Ping, None)
      response <- dispatcher.dispatch(req)
    yield assert(response.isDefined)
  }

  test("dispatch returns None for responses") {
    for
      dispatcher <- createDispatcher
      resp = JsonRpcResponse(RequestId.NumberId(1), Json.obj())
      response <- dispatcher.dispatch(resp)
    yield assertEquals(response, None)
  }
