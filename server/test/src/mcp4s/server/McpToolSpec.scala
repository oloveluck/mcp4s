package mcp4s.server

import cats.effect.IO
import cats.syntax.semigroup.*
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpToolSpec extends CatsEffectSuite:

  // === McpTool Convenience Constructor Tests ===

  test("McpTool.twoNumbers creates tool with two number params") {
    val add = McpTool.twoNumbers[IO]("add", "Add two numbers", "a", "b", "First number", "Second number") {
      (a, b) => IO.pure(ToolResult.text(s"${a + b}"))
    }

    for
      tools <- add.list
      _ = assertEquals(tools.size, 1)
      _ = assertEquals(tools.head.name, "add")
      _ = assertEquals(tools.head.description, Some("Add two numbers"))
      props = tools.head.inputSchema.properties.get
      _ = assertEquals(props("a").`type`, "number")
      _ = assertEquals(props("a").description, Some("First number"))
      _ = assertEquals(props("b").`type`, "number")
      _ = assertEquals(props("b").description, Some("Second number"))
    yield ()
  }

  test("McpTool.twoNumbers calls handler with decoded args") {
    val add = McpTool.twoNumbers[IO]("add", "Add") { (a, b) =>
      IO.pure(ToolResult.text(s"${a + b}"))
    }

    val json = Json.obj("a" -> Json.fromDoubleOrNull(1.5), "b" -> Json.fromDoubleOrNull(2.5))
    for
      result <- add.call("add", json).value
      _ = assertEquals(result.map(_.textContent), Some("4.0"))
    yield ()
  }

  test("McpTool.singleNumber creates tool with single number param") {
    val double = McpTool.singleNumber[IO]("double", "Double a number") { n =>
      IO.pure(ToolResult.text(s"${n * 2}"))
    }

    val json = Json.obj("value" -> Json.fromDoubleOrNull(5.0))
    for
      result <- double.call("double", json).value
      _ = assertEquals(result.map(_.textContent), Some("10.0"))
    yield ()
  }

  test("McpTool.singleString creates tool with single string param") {
    val echo = McpTool.singleString[IO]("echo", "Echo input", "message") { msg =>
      IO.pure(ToolResult.text(msg))
    }

    val json = Json.obj("message" -> Json.fromString("hello"))
    for
      result <- echo.call("echo", json).value
      _ = assertEquals(result.map(_.textContent), Some("hello"))
    yield ()
  }

  test("McpTool.singleBoolean creates tool with single boolean param") {
    val toggle = McpTool.singleBoolean[IO]("toggle", "Toggle flag") { flag =>
      IO.pure(ToolResult.text(if flag then "on" else "off"))
    }

    val json = Json.obj("flag" -> Json.fromBoolean(true))
    for
      result <- toggle.call("toggle", json).value
      _ = assertEquals(result.map(_.textContent), Some("on"))
    yield ()
  }

  test("McpTool returns None for unknown tool name") {
    val add = McpTool.singleNumber[IO]("add", "Add") { a =>
      IO.pure(ToolResult.text(s"$a"))
    }

    for
      result <- add.call("subtract", Json.obj()).value
      _ = assertEquals(result, None)
    yield ()
  }

  test("McpTool raises error for invalid args") {
    val add = McpTool.singleNumber[IO]("add", "Add") { a =>
      IO.pure(ToolResult.text(s"$a"))
    }

    for
      result <- add.call("add", Json.obj("value" -> Json.fromString("not a number"))).value.attempt
      _ = assert(result.isLeft)
    yield ()
  }

  // === McpTool composition Tests ===

  test("McpTool values compose with |+|") {
    val add = McpTool.twoNumbers[IO]("add", "Add") { (a, b) =>
      IO.pure(ToolResult.text(s"${a + b}"))
    }

    val subtract = McpTool.twoNumbers[IO]("subtract", "Subtract") { (a, b) =>
      IO.pure(ToolResult.text(s"${a - b}"))
    }

    val mathTools = add |+| subtract

    for
      tools <- mathTools.list
      _ = assertEquals(tools.map(_.name).toSet, Set("add", "subtract"))
      addResult <- mathTools.call("add", Json.obj("a" -> 3.asJson, "b" -> 2.asJson)).value
      subResult <- mathTools.call("subtract", Json.obj("a" -> 3.asJson, "b" -> 2.asJson)).value
      _ = assertEquals(addResult.map(_.textContent), Some("5.0"))
      _ = assertEquals(subResult.map(_.textContent), Some("1.0"))
    yield ()
  }

  test("McpTool.noArgs creates tool with empty schema") {
    val ping = McpTool.noArgs[IO]("ping", "Ping") {
      IO.pure(ToolResult.text("pong"))
    }

    for
      tools <- ping.list
      _ = assertEquals(tools.head.inputSchema, JsonSchema.empty)
      result <- ping.call("ping", Json.obj()).value
      _ = assertEquals(result.map(_.textContent), Some("pong"))
    yield ()
  }

  // === McpTool with ToolInput ===

  case class ReadArgs(@description("File path") path: String) derives ToolInput

  test("McpTool.annotated creates tool with annotations") {
    val readTool = McpTool.annotated[IO, ReadArgs](
      "read", "Read data", ToolAnnotations.readOnly()
    ) { args =>
      IO.pure(ToolResult.text(s"data from ${args.path}"))
    }

    for
      tools <- readTool.list
      _ = assert(tools.head.annotations.isDefined)
      _ = assertEquals(tools.head.annotations.get.readOnlyHint, Some(true))
    yield ()
  }

  case class CalcArgs(
    @description("First number") a: Double,
    @description("Second number") b: Double
  ) derives ToolInput

  test("McpTool works with derived ToolInput") {
    val add = McpTool[IO, CalcArgs]("add", "Add") { args =>
      IO.pure(ToolResult.text(s"${args.a + args.b}"))
    }

    val json = Json.obj("a" -> Json.fromDoubleOrNull(1.0), "b" -> Json.fromDoubleOrNull(2.0))
    for
      tools <- add.list
      _ = assertEquals(tools.head.inputSchema.properties.get("a").description, Some("First number"))
      result <- add.call("add", json).value
      _ = assertEquals(result.map(_.textContent), Some("3.0"))
    yield ()
  }

  // === McpTool with typed output ===

  test("McpTool.typed creates tool with output schema") {
    val add = McpTool.typed[IO, CalcArgs, Double]("add", "Add") { args =>
      IO.pure(args.a + args.b)
    }

    for
      tools <- add.list
      _ = assert(tools.head.outputSchema.isDefined)
      _ = assertEquals(tools.head.outputSchema.get.`type`, "object")
      result <- add.call("add", Json.obj("a" -> 3.asJson, "b" -> 2.asJson)).value
      _ = assert(result.isDefined)
      _ = assert(result.get.structuredContent.isDefined)
    yield ()
  }

  // === Declarative McpServer.from Tests ===

  test("McpServer.from creates server from composed parts") {
    val add = McpTool.twoNumbers[IO]("add", "Add") { (a, b) =>
      IO.pure(ToolResult.text(s"${a + b}"))
    }

    val readme = McpResource[IO]("test://readme", "README")("Hello world")

    val greeting = McpPrompt.noArgs[IO]("greet", "Greet") {
      IO.pure(GetPromptResult(None, List(PromptMessage(Role.User, TextContent("Hi")))))
    }

    val server = McpServer.from[IO](
      info = ServerInfo("test", "1.0.0"),
      tools = add,
      resources = readme,
      prompts = greeting
    )

    for
      tools <- server.listTools
      _ = assertEquals(tools.map(_.name), List("add"))
      resources <- server.listResources
      _ = assertEquals(resources.map(_.uri), List("test://readme"))
      prompts <- server.listPrompts
      _ = assertEquals(prompts.map(_.name), List("greet"))
      result <- server.callTool("add", Json.obj("a" -> 1.asJson, "b" -> 2.asJson))
      _ = assertEquals(result.textContent, "3.0")
      content <- server.readResource("test://readme")
      _ = assertEquals(content.text, Some("Hello world"))
      prompt <- server.getPrompt("greet", Map.empty)
      _ = assertEquals(prompt.messages.head.content.asInstanceOf[TextContent].text, "Hi")
    yield ()
  }

  test("McpServer.fromTools composes multiple tools with |+|") {
    val add = McpTool.twoNumbers[IO]("add", "Add") { (a, b) =>
      IO.pure(ToolResult.text(s"${a + b}"))
    }

    val mul = McpTool.twoNumbers[IO]("multiply", "Multiply") { (a, b) =>
      IO.pure(ToolResult.text(s"${a * b}"))
    }

    val server = McpServer.fromTools[IO](
      info = ServerInfo("calc", "1.0.0"),
      tools = add |+| mul
    )

    for
      tools <- server.listTools
      _ = assertEquals(tools.map(_.name).toSet, Set("add", "multiply"))
      addResult <- server.callTool("add", Json.obj("a" -> 3.asJson, "b" -> 2.asJson))
      _ = assertEquals(addResult.textContent, "5.0")
      mulResult <- server.callTool("multiply", Json.obj("a" -> 3.asJson, "b" -> 2.asJson))
      _ = assertEquals(mulResult.textContent, "6.0")
    yield ()
  }

  test("McpServer.fromTools raises ToolNotFound for unknown tool") {
    val server = McpServer.fromTools[IO](
      info = ServerInfo("test", "1.0.0"),
      tools = McpTools.empty[IO]
    )
    for
      result <- server.callTool("nonexistent", Json.obj()).attempt
      _ = assert(result.isLeft)
      _ = assert(result.left.exists(_.isInstanceOf[McpError.ToolNotFound]))
    yield ()
  }

  test("McpServer.from raises ResourceNotFound for unknown resource") {
    val server = McpServer.from[IO](
      info = ServerInfo("test", "1.0.0"),
      tools = McpTools.empty[IO],
      resources = McpResources.empty[IO],
      prompts = McpPrompts.empty[IO]
    )
    for
      result <- server.readResource("test://unknown").attempt
      _ = assert(result.isLeft)
      _ = assert(result.left.exists(_.isInstanceOf[McpError.ResourceNotFound]))
    yield ()
  }

  test("McpServer.from raises PromptNotFound for unknown prompt") {
    val server = McpServer.from[IO](
      info = ServerInfo("test", "1.0.0"),
      tools = McpTools.empty[IO],
      resources = McpResources.empty[IO],
      prompts = McpPrompts.empty[IO]
    )
    for
      result <- server.getPrompt("unknown", Map.empty).attempt
      _ = assert(result.isLeft)
      _ = assert(result.left.exists(_.isInstanceOf[McpError.PromptNotFound]))
    yield ()
  }

  // === Pure Helper Method Tests ===

  test("McpTool.pureText creates tool with pure string handler") {
    case class EchoArgs(message: String) derives ToolInput

    val echo = McpTool.pureText[IO, EchoArgs]("echo", "Echo input") { args =>
      s"Echo: ${args.message}"
    }

    val json = Json.obj("message" -> Json.fromString("hello"))
    for
      result <- echo.call("echo", json).value
      _ = assertEquals(result.map(_.textContent), Some("Echo: hello"))
    yield ()
  }

  test("McpTool.pureTextNoArgs creates no-arg tool with pure result") {
    val version = McpTool.pureTextNoArgs[IO]("version", "Get version") {
      "1.0.0"
    }

    for
      result <- version.call("version", Json.obj()).value
      _ = assertEquals(result.map(_.textContent), Some("1.0.0"))
    yield ()
  }

  test("McpTool.singleStringPure creates tool with pure string result") {
    val upper = McpTool.singleStringPure[IO]("upper", "Uppercase") { s =>
      s.toUpperCase
    }

    val json = Json.obj("input" -> Json.fromString("hello"))
    for
      result <- upper.call("upper", json).value
      _ = assertEquals(result.map(_.textContent), Some("HELLO"))
    yield ()
  }

  test("McpTool.singleNumberPure creates tool with pure string result") {
    val double = McpTool.singleNumberPure[IO]("double", "Double a number") { n =>
      s"${n * 2}"
    }

    val json = Json.obj("value" -> Json.fromDoubleOrNull(5.0))
    for
      result <- double.call("double", json).value
      _ = assertEquals(result.map(_.textContent), Some("10.0"))
    yield ()
  }

  test("McpTool.twoNumbersPure creates tool with pure string result") {
    val add = McpTool.twoNumbersPure[IO]("add", "Add two numbers") { (a, b) =>
      s"${a + b}"
    }

    val json = Json.obj("a" -> Json.fromDoubleOrNull(3.0), "b" -> Json.fromDoubleOrNull(2.0))
    for
      result <- add.call("add", json).value
      _ = assertEquals(result.map(_.textContent), Some("5.0"))
    yield ()
  }

  // === PromptResult Builder Tests ===

  test("PromptResult creates prompt messages concisely") {
    val result = PromptResult(
      PromptResult.user("Hello"),
      PromptResult.assistant("Hi there")
    )

    assertEquals(result.description, None)
    assertEquals(result.messages.size, 2)
    assertEquals(result.messages(0).role, Role.User)
    assertEquals(result.messages(0).content.asInstanceOf[TextContent].text, "Hello")
    assertEquals(result.messages(1).role, Role.Assistant)
    assertEquals(result.messages(1).content.asInstanceOf[TextContent].text, "Hi there")
  }

  test("PromptResult with description") {
    val result = PromptResult("A greeting")(
      PromptResult.user("Hello")
    )

    assertEquals(result.description, Some("A greeting"))
    assertEquals(result.messages.size, 1)
  }

  // === McpResult Namespace Tests ===

  test("McpResult.tool creates text result") {
    val result = McpResult.tool("Success!")
    assertEquals(result.textContent, "Success!")
    assertEquals(result.isError, false)
  }

  test("McpResult.toolError creates error result") {
    val result = McpResult.toolError("Failed")
    assertEquals(result.textContent, "Failed")
    assertEquals(result.isError, true)
  }

  test("McpResult.resource creates text resource") {
    val content = McpResult.resource("test://uri", "Hello")
    assertEquals(content.uri, "test://uri")
    assertEquals(content.text, Some("Hello"))
  }

  test("McpResult.prompt creates prompt result") {
    val result = McpResult.prompt(McpResult.user("Hi"))
    assertEquals(result.messages.size, 1)
    assertEquals(result.messages.head.role, Role.User)
  }

  // === String Extensions Tests ===

  test("string.asResource creates resource content") {
    val content = "Hello world".asResource("test://hello")
    assertEquals(content.uri, "test://hello")
    assertEquals(content.text, Some("Hello world"))
  }

  test("string.asToolResult creates tool result") {
    val result = "Success!".asToolResult
    assertEquals(result.textContent, "Success!")
  }

  // === Context-Aware Tool Composition Tests ===

  test("McpTool.withContext returns McpTools for composition") {
    case class QueryArgs(query: String) derives ToolInput

    val regular = McpTool.twoNumbersPure[IO]("add", "Add") { (a, b) => s"${a + b}" }

    val contextAware = McpTool.withContext[IO, QueryArgs]("smart", "Smart") { (args, ctx) =>
      IO.pure(ToolResult.text(s"Query: ${args.query}"))
    }

    // Should compile: regular and context tools compose together
    val combined = regular |+| contextAware

    for
      tools <- combined.list
      _ = assertEquals(tools.map(_.name).toSet, Set("add", "smart"))
      addResult <- combined.call("add", Json.obj("a" -> 1.asJson, "b" -> 2.asJson)).value
      smartResult <- combined.call("smart", Json.obj("query" -> "test".asJson)).value
      _ = assertEquals(addResult.map(_.textContent), Some("3.0"))
      _ = assertEquals(smartResult.map(_.textContent), Some("Query: test"))
    yield ()
  }

  test("McpTool.withContextNoArgs works without typed args") {
    val pingTool = McpTool.withContextNoArgs[IO]("ping", "Ping with context") { ctx =>
      IO.pure(ToolResult.text(s"pong (request: ${ctx.requestId})"))
    }

    for
      tools <- pingTool.list
      _ = assertEquals(tools.head.name, "ping")
      result <- pingTool.call("ping", Json.obj()).value
      _ = assert(result.isDefined)
      _ = assert(result.get.textContent.startsWith("pong"))
    yield ()
  }
