package mcp4s.server

import cats.effect.IO
import cats.syntax.semigroup.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpPromptSpec extends CatsEffectSuite:

  case class GreetArgs(@description("Who to greet") name: String) derives PromptInput

  test("McpPrompt creates prompt with PromptInput-based arguments") {
    val greet = McpPrompt[IO, GreetArgs]("greet", "Greet someone") { args =>
      IO.pure(GetPromptResult(None, List(PromptMessage(Role.User, TextContent(s"Hi ${args.name}")))))
    }

    for
      prompts <- greet.list
      _ = assertEquals(prompts.size, 1)
      _ = assertEquals(prompts.head.name, "greet")
      _ = assertEquals(prompts.head.description, Some("Greet someone"))
      args = prompts.head.arguments
      _ = assertEquals(args.size, 1)
      _ = assertEquals(args.head.name, "name")
      _ = assertEquals(args.head.description, Some("Who to greet"))
      _ = assert(args.head.required)
    yield ()
  }

  test("McpPrompt calls handler with decoded args") {
    val greet = McpPrompt[IO, GreetArgs]("greet", "Greet") { args =>
      IO.pure(GetPromptResult(None, List(PromptMessage(Role.User, TextContent(s"Hi ${args.name}")))))
    }

    for
      result <- greet.get("greet", Map("name" -> "Alice")).value
      _ = assert(result.isDefined)
      msg = result.get.messages.head.content.asInstanceOf[TextContent].text
      _ = assertEquals(msg, "Hi Alice")
    yield ()
  }

  test("McpPrompt returns None for unknown prompt name") {
    val greet = McpPrompt.noArgs[IO]("greet", "Greet") {
      IO.pure(GetPromptResult(None, List(PromptMessage(Role.User, TextContent("Hi")))))
    }

    for
      result <- greet.get("other", Map.empty).value
      _ = assertEquals(result, None)
    yield ()
  }

  test("McpPrompt.noArgs creates prompt without arguments") {
    val hello = McpPrompt.noArgs[IO]("hello", "Say hello") {
      IO.pure(GetPromptResult(Some("Hello"), List(PromptMessage(Role.User, TextContent("Hello!")))))
    }

    for
      prompts <- hello.list
      _ = assert(prompts.head.arguments.isEmpty)
      result <- hello.get("hello", Map.empty).value
      _ = assertEquals(result.get.description, Some("Hello"))
    yield ()
  }

  test("McpPrompt.raw creates prompt from map handler") {
    val calc = McpPrompt.raw[IO]("calc", "Calculate",
      List(PromptArgument("op", Some("Operation"), required = true))
    ) { args =>
      IO.pure(GetPromptResult(
        Some(s"Calculate: ${args.getOrElse("op", "?")}"),
        List(PromptMessage(Role.User, TextContent(args.getOrElse("op", "?"))))
      ))
    }

    for
      result <- calc.get("calc", Map("op" -> "add")).value
      _ = assertEquals(result.get.description, Some("Calculate: add"))
    yield ()
  }

  test("McpPrompts compose with |+|") {
    val greet = McpPrompt.noArgs[IO]("greet", "Greet") {
      IO.pure(GetPromptResult(None, List(PromptMessage(Role.User, TextContent("Hi")))))
    }

    val farewell = McpPrompt.noArgs[IO]("farewell", "Farewell") {
      IO.pure(GetPromptResult(None, List(PromptMessage(Role.User, TextContent("Bye")))))
    }

    val all = greet |+| farewell

    for
      prompts <- all.list
      _ = assertEquals(prompts.map(_.name).toSet, Set("greet", "farewell"))
      g <- all.get("greet", Map.empty).value
      _ = assertEquals(g.get.messages.head.content.asInstanceOf[TextContent].text, "Hi")
      f <- all.get("farewell", Map.empty).value
      _ = assertEquals(f.get.messages.head.content.asInstanceOf[TextContent].text, "Bye")
    yield ()
  }

  case class CalcArgs(
    @description("The operation") operation: String,
    @description("The value") value: String
  ) derives PromptInput

  test("McpPrompt with PromptInput extracts argument metadata") {
    val prompt = McpPrompt[IO, CalcArgs]("calc", "Calculate") { args =>
      IO.pure(GetPromptResult(None, List(PromptMessage(Role.User, TextContent(s"${args.operation}: ${args.value}")))))
    }

    for
      prompts <- prompt.list
      args = prompts.head.arguments
      _ = assertEquals(args.size, 2)
      names = args.map(_.name).toSet
      _ = assert(names.contains("operation"))
      _ = assert(names.contains("value"))
    yield ()
  }
