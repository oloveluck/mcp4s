package mcp4s.client

import cats.effect.IO
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpClientBuilderSpec extends CatsEffectSuite:

  // === Default Builder Tests ===

  test("empty builder creates client with default info") {
    val client = McpClient.builder[IO].build
    assertEquals(client.info.name, "mcp4s-client")
    assertEquals(client.info.version, "0.1.0")
  }

  test("empty builder creates client with no capabilities") {
    val client = McpClient.builder[IO].build
    assertEquals(client.capabilities.roots, None)
    assertEquals(client.capabilities.sampling, None)
    assertEquals(client.capabilities.elicitation, None)
  }

  // === Client Info Tests ===

  test("withInfo sets client info") {
    val client = McpClient.builder[IO]
      .withInfo(ClientInfo("my-client", "2.0.0"))
      .build
    assertEquals(client.info.name, "my-client")
    assertEquals(client.info.version, "2.0.0")
  }

  test("withInfo includes optional fields") {
    val client = McpClient.builder[IO]
      .withInfo(ClientInfo("my-client", "1.0", title = Some("My Client")))
      .build
    assertEquals(client.info.title, Some("My Client"))
  }

  // === Roots Capability Tests ===

  test("withRoots adds roots capability") {
    val client = McpClient.builder[IO]
      .withRoots(List(Root("file:///home/user", Some("Home"))))
      .build
    assert(client.capabilities.roots.isDefined)
  }

  test("withRoots sets listChanged to true") {
    val client = McpClient.builder[IO]
      .withRoots(List(Root("file:///workspace")))
      .build
    assertEquals(client.capabilities.roots.flatMap(_.listChanged), Some(true))
  }

  test("withRoot adds single root") {
    val client = McpClient.builder[IO]
      .withRoot(Root("file:///project"))
      .build
    assert(client.capabilities.roots.isDefined)
  }

  test("withRoot(uri, name) adds root") {
    val client = McpClient.builder[IO]
      .withRoot("file:///data", Some("Data"))
      .build
    assert(client.capabilities.roots.isDefined)
  }

  test("multiple withRoot calls accumulate") {
    val client = McpClient.builder[IO]
      .withRoot("file:///a")
      .withRoot("file:///b")
      .build

    for
      result <- client.listRoots
    yield assertEquals(result.roots.length, 2)
  }

  test("listRoots returns configured roots") {
    val roots = List(
      Root("file:///home/user", Some("Home")),
      Root("file:///workspace", Some("Workspace"))
    )
    val client = McpClient.builder[IO]
      .withRoots(roots)
      .build

    for
      result <- client.listRoots
    yield
      assertEquals(result.roots.length, 2)
      assertEquals(result.roots.head.uri, "file:///home/user")
      assertEquals(result.roots.head.name, Some("Home"))
  }

  // === Sampling Capability Tests ===

  test("withSamplingHandler adds sampling capability") {
    val client = McpClient.builder[IO]
      .withSamplingHandler(_ => IO.raiseError(new RuntimeException("not implemented")))
      .build
    assert(client.capabilities.sampling.isDefined)
  }

  test("createMessage invokes sampling handler") {
    val expectedResult = CreateMessageResult(
      role = Role.Assistant,
      content = SamplingTextContent("Hello!"),
      model = "test-model",
      stopReason = Some("endTurn")
    )

    val client = McpClient.builder[IO]
      .withSamplingHandler(_ => IO.pure(expectedResult))
      .build

    val params = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("Hi"))),
      maxTokens = 100
    )

    for
      result <- client.createMessage(params)
    yield
      assertEquals(result.role, Role.Assistant)
      assertEquals(result.model, "test-model")
  }

  test("createMessage fails without sampling handler") {
    val client = McpClient.builder[IO].build

    val params = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("Hi"))),
      maxTokens = 100
    )

    client.createMessage(params).attempt.map { result =>
      assert(result.isLeft)
      result.left.toOption.get match
        case McpError.MethodNotSupported(method) =>
          assertEquals(method, "sampling/createMessage")
        case other =>
          fail(s"Expected MethodNotSupported, got $other")
    }
  }

  // === Elicitation Capability Tests ===

  test("withElicitationHandler adds elicitation capability") {
    val client = McpClient.builder[IO]
      .withElicitationHandler(_ => IO.raiseError(new RuntimeException("not implemented")))
      .build
    assert(client.capabilities.elicitation.isDefined)
  }

  test("elicit invokes elicitation handler") {
    val expectedResult = ElicitResult(
      action = ElicitAction.Accept,
      content = Some(Map("name" -> io.circe.Json.fromString("Alice")))
    )

    val client = McpClient.builder[IO]
      .withElicitationHandler(_ => IO.pure(expectedResult))
      .build

    val params = ElicitFormParams(
      message = "Enter your name",
      requestedSchema = JsonSchema.obj(
        Map("name" -> JsonSchema.string(Some("Name"))),
        List("name")
      )
    )

    for
      result <- client.elicit(params)
    yield assertEquals(result.action, ElicitAction.Accept)
  }

  test("elicit fails without elicitation handler") {
    val client = McpClient.builder[IO].build

    val params = ElicitFormParams(
      message = "Enter name",
      requestedSchema = JsonSchema.obj(Map.empty, Nil)
    )

    client.elicit(params).attempt.map { result =>
      assert(result.isLeft)
      result.left.toOption.get match
        case McpError.MethodNotSupported(method) =>
          assertEquals(method, "elicitation/create")
        case other =>
          fail(s"Expected MethodNotSupported, got $other")
    }
  }

  // === Combined Capabilities Tests ===

  test("builder supports all capabilities together") {
    val client = McpClient.builder[IO]
      .withInfo(ClientInfo("full-client", "1.0"))
      .withRoots(List(Root("file:///workspace")))
      .withSamplingHandler(_ => IO.raiseError(new RuntimeException("not implemented")))
      .withElicitationHandler(_ => IO.raiseError(new RuntimeException("not implemented")))
      .build

    assert(client.capabilities.roots.isDefined)
    assert(client.capabilities.sampling.isDefined)
    assert(client.capabilities.elicitation.isDefined)
  }

  test("builder is immutable") {
    val builder1 = McpClient.builder[IO]
    val builder2 = builder1.withInfo(ClientInfo("modified", "1.0"))

    val client1 = builder1.build
    val client2 = builder2.build

    assertEquals(client1.info.name, "mcp4s-client")
    assertEquals(client2.info.name, "modified")
  }
