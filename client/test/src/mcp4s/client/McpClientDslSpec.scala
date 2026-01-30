package mcp4s.client

import cats.effect.IO
import cats.syntax.semigroup.*
import io.circe.Json
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpClientDslSpec extends CatsEffectSuite:

  import mcp4s.client.mcp.*

  // === Result Builder Tests ===

  test("message creates text sampling result") {
    val result = message("Hello!", "gpt-4")
    assertEquals(result.role, Role.Assistant)
    assertEquals(result.model, "gpt-4")
    assertEquals(result.stopReason, Some("endTurn"))
    result.content match
      case SamplingTextContent(text) => assertEquals(text, "Hello!")
      case _                         => fail("Expected SamplingTextContent")
  }

  test("message uses default model") {
    val result = message("Hello!")
    assertEquals(result.model, "default")
  }

  test("messageImage creates image sampling result") {
    val result = messageImage("base64data", "image/png", "gpt-4-vision")
    assertEquals(result.role, Role.Assistant)
    assertEquals(result.model, "gpt-4-vision")
    result.content match
      case SamplingImageContent(data, mime) =>
        assertEquals(data, "base64data")
        assertEquals(mime, "image/png")
      case _ => fail("Expected SamplingImageContent")
  }

  test("messageAudio creates audio sampling result") {
    val result = messageAudio("base64data", "audio/wav", "whisper-1")
    result.content match
      case SamplingAudioContent(data, mime) =>
        assertEquals(data, "base64data")
        assertEquals(mime, "audio/wav")
      case _ => fail("Expected SamplingAudioContent")
  }

  test("accept with content creates elicitation result") {
    val result = accept(Map("name" -> Json.fromString("Alice")))
    assertEquals(result.action, ElicitAction.Accept)
    assertEquals(result.content, Some(Map("name" -> Json.fromString("Alice"))))
  }

  test("accept without content creates elicitation result") {
    val result = accept
    assertEquals(result.action, ElicitAction.Accept)
    assertEquals(result.content, None)
  }

  test("decline creates elicitation result") {
    val result = decline
    assertEquals(result.action, ElicitAction.Decline)
    assertEquals(result.content, None)
  }

  test("cancel creates elicitation result") {
    val result = cancel
    assertEquals(result.action, ElicitAction.Cancel)
    assertEquals(result.content, None)
  }

  // === Handler Constructor Tests ===

  test("Sampling creates composable handler") {
    val sampling = Sampling[IO] { params =>
      IO.pure(message("Response", "test-model"))
    }

    val params = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("Hello"))),
      maxTokens = 100
    )

    for
      result <- sampling.handle(params).value
    yield
      assert(result.isDefined)
      assertEquals(result.get.model, "test-model")
  }

  test("Sampling.empty returns None") {
    val sampling = Sampling.empty[IO]
    val params = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("Hello"))),
      maxTokens = 100
    )

    for
      result <- sampling.handle(params).value
    yield assertEquals(result, None)
  }

  test("Elicitation creates composable handler") {
    val elicitation = Elicitation[IO] { params =>
      IO.pure(accept(Map("confirmed" -> Json.fromBoolean(true))))
    }

    val params = ElicitFormParams(
      message = "Confirm?",
      requestedSchema = JsonSchema.obj(Map("confirmed" -> JsonSchema.boolean()))
    )

    for
      result <- elicitation.handle(params).value
    yield
      assert(result.isDefined)
      assertEquals(result.get.action, ElicitAction.Accept)
  }

  test("Elicitation.withComplete includes complete handler") {
    var completeCalled = false
    val elicitation = Elicitation.withComplete[IO](
      handler = _ => IO.pure(accept),
      onComplete = _ => IO { completeCalled = true }
    )

    val completeParams = ElicitationCompleteParams("test-id", ElicitResult(ElicitAction.Accept, None))

    for
      _ <- elicitation.onComplete(completeParams)
    yield assert(completeCalled)
  }

  test("Elicitation.empty returns None") {
    val elicitation = Elicitation.empty[IO]
    val params = ElicitFormParams(
      message = "Test",
      requestedSchema = JsonSchema.empty
    )

    for
      result <- elicitation.handle(params).value
    yield assertEquals(result, None)
  }

  test("Roots creates composable roots") {
    val roots = Roots[IO](
      Root("file:///workspace", Some("Workspace")),
      Root("file:///home", Some("Home"))
    )

    for
      result <- roots.list
    yield
      assertEquals(result.length, 2)
      assertEquals(result.head.uri, "file:///workspace")
      assertEquals(result(1).uri, "file:///home")
  }

  test("Roots(uri, name) creates single root") {
    val roots = Roots[IO]("file:///workspace", "Workspace")

    for
      result <- roots.list
    yield
      assertEquals(result.length, 1)
      assertEquals(result.head.uri, "file:///workspace")
      assertEquals(result.head.name, Some("Workspace"))
  }

  test("Roots.empty returns empty list") {
    val roots = Roots.empty[IO]

    for
      result <- roots.list
    yield assertEquals(result, Nil)
  }

  // === Composition Tests ===

  test("sampling handlers compose with |+|") {
    // First handler only handles requests with "special" in the message
    val special = new McpSamplings[IO]:
      import cats.data.OptionT
      def handle(params: CreateMessageParams): OptionT[IO, CreateMessageResult] =
        val hasSpecial = params.messages.exists { msg =>
          msg.content match
            case SamplingTextContent(text) => text.contains("special")
            case _                         => false
        }
        if hasSpecial then OptionT.liftF(IO.pure(message("Special response", "special")))
        else OptionT.none

    val fallback = Sampling[IO] { _ =>
      IO.pure(message("Fallback response", "fallback"))
    }

    val combined = special |+| fallback

    val specialParams = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("This is special"))),
      maxTokens = 100
    )
    val normalParams = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("Normal request"))),
      maxTokens = 100
    )

    for
      specialResult <- combined.handle(specialParams).value
      normalResult <- combined.handle(normalParams).value
    yield
      assertEquals(specialResult.map(_.model), Some("special"))
      assertEquals(normalResult.map(_.model), Some("fallback"))
  }

  test("roots compose with |+|") {
    val workspace = Roots[IO]("file:///workspace", "Workspace")
    val home = Roots[IO]("file:///home", "Home")
    val combined = workspace |+| home

    for
      result <- combined.list
    yield
      assertEquals(result.length, 2)
      assertEquals(result.map(_.uri), List("file:///workspace", "file:///home"))
  }

  // === Pure Extension Tests ===

  test("pure extension lifts value into IO") {
    val result: IO[CreateMessageResult] = message("Hello", "model").pure[IO]

    for
      r <- result
    yield assertEquals(r.model, "model")
  }

  // === McpClient.from Tests ===

  test("McpClient.from works with composed handlers") {
    val sampling = Sampling[IO] { _ =>
      IO.pure(message("Hello", "test"))
    }
    val roots = Roots[IO]("file:///workspace", "Workspace")

    val client = McpClient.from[IO](
      info = ClientInfo("test-client", "1.0.0"),
      roots = Some(roots),
      sampling = Some(sampling)
    )

    assertEquals(client.info.name, "test-client")
    assert(client.capabilities.sampling.isDefined)
    assert(client.capabilities.roots.isDefined)
    assertEquals(client.capabilities.elicitation, None)
  }

  test("McpClient.from with sampling handles requests") {
    val sampling = Sampling[IO] { _ =>
      IO.pure(message("Response", "test-model"))
    }

    val client = McpClient.from[IO](
      info = ClientInfo("test", "1.0.0"),
      sampling = Some(sampling)
    )

    val params = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("Hi"))),
      maxTokens = 100
    )

    for
      result <- client.createMessage(params)
    yield
      assertEquals(result.model, "test-model")
  }

  test("McpClient.from with roots lists them") {
    val roots = Roots[IO](
      Root("file:///a", Some("A")),
      Root("file:///b", Some("B"))
    )

    val client = McpClient.from[IO](
      info = ClientInfo("test", "1.0.0"),
      roots = Some(roots)
    )

    for
      result <- client.listRoots
    yield
      assertEquals(result.roots.length, 2)
      assertEquals(result.roots.map(_.uri), List("file:///a", "file:///b"))
  }

  test("McpClient.from with elicitation handles requests") {
    val elicitation = Elicitation[IO] { _ =>
      IO.pure(accept(Map("confirmed" -> Json.fromBoolean(true))))
    }

    val client = McpClient.from[IO](
      info = ClientInfo("test", "1.0.0"),
      elicitation = Some(elicitation)
    )

    val params = ElicitFormParams(
      message = "Confirm?",
      requestedSchema = JsonSchema.empty
    )

    for
      result <- client.elicit(params)
    yield assertEquals(result.action, ElicitAction.Accept)
  }

  test("McpClient.from with empty handlers has no capabilities") {
    val client = McpClient.from[IO](
      info = ClientInfo("test", "1.0.0")
    )

    assertEquals(client.capabilities.sampling, None)
    assertEquals(client.capabilities.elicitation, None)
    assertEquals(client.capabilities.roots, None)
  }

  test("McpClient.from without sampling fails on createMessage") {
    val client = McpClient.from[IO](
      info = ClientInfo("test", "1.0.0")
    )

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

  // === Builder Integration Tests ===

  test("builder.withSampling accepts composed handler") {
    val sampling = Sampling[IO] { _ =>
      IO.pure(message("Hello", "test"))
    }

    val client = McpClient.builder[IO]
      .withInfo(ClientInfo("test", "1.0.0"))
      .withSampling(sampling)
      .build

    assert(client.capabilities.sampling.isDefined)

    val params = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("Hi"))),
      maxTokens = 100
    )

    for
      result <- client.createMessage(params)
    yield assertEquals(result.model, "test")
  }

  test("builder.withElicitation accepts composed handler") {
    val elicitation = Elicitation[IO] { _ =>
      IO.pure(accept)
    }

    val client = McpClient.builder[IO]
      .withInfo(ClientInfo("test", "1.0.0"))
      .withElicitation(elicitation)
      .build

    assert(client.capabilities.elicitation.isDefined)
  }

  test("builder.withRoots accepts composed roots") {
    val roots = Roots[IO](
      Root("file:///workspace", Some("Workspace"))
    )

    val client = McpClient.builder[IO]
      .withInfo(ClientInfo("test", "1.0.0"))
      .withRoots(roots)
      .build

    assert(client.capabilities.roots.isDefined)

    for
      result <- client.listRoots
    yield assertEquals(result.roots.head.uri, "file:///workspace")
  }

  test("builder composed handlers work with static roots") {
    val composedRoots = Roots[IO]("file:///dynamic", "Dynamic")

    val client = McpClient.builder[IO]
      .withInfo(ClientInfo("test", "1.0.0"))
      .withRoot("file:///static", Some("Static"))
      .withRoots(composedRoots)
      .build

    for
      result <- client.listRoots
    yield
      // Static roots come first, then dynamic
      assertEquals(result.roots.map(_.uri), List("file:///static", "file:///dynamic"))
  }
