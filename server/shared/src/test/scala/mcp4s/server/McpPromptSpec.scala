/*
 * Copyright 2025 MCP4S Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mcp4s.server

import cats.effect.IO
import cats.syntax.semigroup.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpPromptSpec extends CatsEffectSuite:

  import mcp4s.server.dsl.{Prompt as PromptDef, *}

  case class GreetArgs(@description("Who to greet") name: String) derives Schema

  test("Prompt with typed input derives argument metadata") {
    val greet = PromptDef("greet").withDescription("Greet someone").input[GreetArgs].handle[IO] {
      args =>
        IO.pure(
          GetPromptResult(None, List(PromptMessage(Role.User, TextContent(s"Hi ${args.name}"))))
        )
    }

    for
      prompts <- greet.list
      _    = assertEquals(prompts.size, 1)
      _    = assertEquals(prompts.head.name, "greet")
      _    = assertEquals(prompts.head.description, Some("Greet someone"))
      args = prompts.head.arguments
      _    = assertEquals(args.size, 1)
      _    = assertEquals(args.head.name, "name")
      _    = assertEquals(args.head.description, Some("Who to greet"))
      _    = assert(args.head.required)
    yield ()
  }

  test("Prompt calls handler with decoded args") {
    val greet = PromptDef("greet").withDescription("Greet").input[GreetArgs].handle[IO] { args =>
      IO.pure(
        GetPromptResult(None, List(PromptMessage(Role.User, TextContent(s"Hi ${args.name}"))))
      )
    }

    for
      result <- greet.get("greet", Map("name" -> "Alice")).value
      _   = assert(result.isDefined)
      msg = result.get.messages.head.content.asInstanceOf[TextContent].text
      _   = assertEquals(msg, "Hi Alice")
    yield ()
  }

  test("Prompt returns None for unknown prompt name") {
    val greet = PromptDef("greet").withDescription("Greet").messages[IO](user("Hi"))

    for
      result <- greet.get("other", Map.empty).value
      _ = assertEquals(result, None)
    yield ()
  }

  test("Prompt without input has no arguments") {
    val hello = PromptDef("hello")
      .withDescription("Say hello")
      .static[IO](GetPromptResult(Some("Hello"), List(PromptMessage(Role.User, TextContent("Hello!")))))

    for
      prompts <- hello.list
      _ = assert(prompts.head.arguments.isEmpty)
      result <- hello.get("hello", Map.empty).value
      _ = assertEquals(result.get.description, Some("Hello"))
    yield ()
  }

  test("Prompts.single creates prompt from raw map handler") {
    val calc = Prompts.single[IO](
      mcp4s.protocol.Prompt(
        "calc",
        Some("Calculate"),
        List(PromptArgument("op", Some("Operation"), required = true))
      )
    ) { args =>
      IO.pure(
        GetPromptResult(
          Some(s"Calculate: ${args.getOrElse("op", "?")}"),
          List(PromptMessage(Role.User, TextContent(args.getOrElse("op", "?"))))
        )
      )
    }

    for
      result <- calc.get("calc", Map("op" -> "add")).value
      _ = assertEquals(result.get.description, Some("Calculate: add"))
    yield ()
  }

  test("Prompts compose with |+|") {
    val greet    = PromptDef("greet").withDescription("Greet").messages[IO](user("Hi"))
    val farewell = PromptDef("farewell").withDescription("Farewell").messages[IO](user("Bye"))

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
  ) derives Schema

  test("Prompt with typed input extracts argument metadata") {
    val prompt = PromptDef("calc").withDescription("Calculate").input[CalcArgs].handle[IO] { args =>
      IO.pure(
        GetPromptResult(
          None,
          List(PromptMessage(Role.User, TextContent(s"${args.operation}: ${args.value}")))
        )
      )
    }

    for
      prompts <- prompt.list
      args  = prompts.head.arguments
      _     = assertEquals(args.size, 2)
      names = args.map(_.name).toSet
      _     = assert(names.contains("operation"))
      _     = assert(names.contains("value"))
    yield ()
  }
