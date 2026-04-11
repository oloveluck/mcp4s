package mcp4s.server

import cats.effect.IO
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpDslDerivedSpec extends CatsEffectSuite:

  import mcp4s.server.mcp.*

  private val minimalCtx = ToolContext.minimal[IO](SamplingRequester.unsupported[IO], RequestId.NullId)

  // === Tool: derived name + description ===

  @description("Add two numbers")
  case class Add(
      @description("First number") a: Double,
      @description("Second number") b: Double
  ) derives ToolInput

  test("Tool with derived name and description") {
    val add = Tool[IO, Add] { args =>
      IO.pure(ok(s"${args.a + args.b}"))
    }

    val json = Json.obj("a" -> 3.asJson, "b" -> 2.asJson)
    for
      tools <- add.list
      _ = assertEquals(tools.size, 1)
      _ = assertEquals(tools.head.name, "add")
      _ = assertEquals(tools.head.description, Some("Add two numbers"))
      result <- add.call("add", json, minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("5.0"))
    yield ()
  }

  // === Tool: custom name, derived description ===

  test("Tool with custom name and derived description") {
    val custom = Tool[IO, Add]("custom-add") { args =>
      IO.pure(ok(s"${args.a + args.b}"))
    }

    val json = Json.obj("a" -> 1.asJson, "b" -> 2.asJson)
    for
      tools <- custom.list
      _ = assertEquals(tools.head.name, "custom-add")
      _ = assertEquals(tools.head.description, Some("Add two numbers"))
      result <- custom.call("custom-add", json, minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("3.0"))
    yield ()
  }

  // === Tool.text: derived name ===

  @description("Echo a message")
  case class Echo(@description("Message") message: String) derives ToolInput

  test("Tool.text with derived name and description") {
    val echo = Tool.text[IO, Echo] { args =>
      s"Echo: ${args.message}"
    }

    val json = Json.obj("message" -> "hello".asJson)
    for
      tools <- echo.list
      _ = assertEquals(tools.head.name, "echo")
      _ = assertEquals(tools.head.description, Some("Echo a message"))
      result <- echo.call("echo", json, minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("Echo: hello"))
    yield ()
  }

  // === Tool.withContext: derived name ===

  @description("Context-aware tool")
  case class CtxArgs(@description("Input") input: String) derives ToolInput

  test("Tool.withContext with derived name and description") {
    val ctxTool = Tool.withContext[IO, CtxArgs] { (args, ctx) =>
      IO.pure(ok(s"Input: ${args.input}, Request: ${ctx.requestId}"))
    }

    val json = Json.obj("input" -> "test".asJson)
    for
      tools <- ctxTool.list
      _ = assertEquals(tools.head.name, "ctx")
      _ = assertEquals(tools.head.description, Some("Context-aware tool"))
      result <- ctxTool.call("ctx", json, minimalCtx).value
      _ = assert(result.isDefined)
      _ = assert(result.get.textContent.contains("Input: test"))
    yield ()
  }

  test("Tool.withContext with custom name and derived description") {
    val ctxTool = Tool.withContext[IO, CtxArgs]("custom-ctx") { (args, ctx) =>
      IO.pure(ok(s"Input: ${args.input}"))
    }

    for
      tools <- ctxTool.list
      _ = assertEquals(tools.head.name, "custom-ctx")
      _ = assertEquals(tools.head.description, Some("Context-aware tool"))
    yield ()
  }

  // === Tool with Args suffix stripping ===

  @description("Search for items")
  case class SearchArgs(@description("Query") query: String) derives ToolInput

  test("Tool derives name with Args suffix stripped") {
    val search = Tool[IO, SearchArgs] { args =>
      IO.pure(ok(s"Searching: ${args.query}"))
    }

    for
      tools <- search.list
      _ = assertEquals(tools.head.name, "search")
      _ = assertEquals(tools.head.description, Some("Search for items"))
    yield ()
  }

  // === Tool with no class-level description ===

  case class NoDesc(@description("Value") value: String) derives ToolInput

  test("Tool with no class-level description uses empty string") {
    val tool = Tool[IO, NoDesc] { args =>
      IO.pure(ok(args.value))
    }

    for
      tools <- tool.list
      _ = assertEquals(tools.head.name, "no_desc")
      _ = assertEquals(tools.head.description, Some(""))
    yield ()
  }

  // === Prompt: derived name + description ===

  @description("A greeting prompt")
  case class Greet(@description("Name") name: String) derives PromptInput

  test("Prompt with derived name and description") {
    val greet = Prompt[IO, Greet] { args =>
      IO.pure(messages(user(s"Hello, ${args.name}!")))
    }

    for
      prompts <- greet.list
      _ = assertEquals(prompts.size, 1)
      _ = assertEquals(prompts.head.name, "greet")
      _ = assertEquals(prompts.head.description, Some("A greeting prompt"))
      result <- greet.get("greet", Map("name" -> "Alice")).value
      _ = assertEquals(
        result.get.messages.head.content.asInstanceOf[TextContent].text,
        "Hello, Alice!"
      )
    yield ()
  }

  // === Prompt: custom name, derived description ===

  test("Prompt with custom name and derived description") {
    val greet = Prompt[IO, Greet]("custom-greet") { args =>
      IO.pure(messages(user(s"Hello, ${args.name}!")))
    }

    for
      prompts <- greet.list
      _ = assertEquals(prompts.head.name, "custom-greet")
      _ = assertEquals(prompts.head.description, Some("A greeting prompt"))
    yield ()
  }
