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
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpDslDerivedSpec extends CatsEffectSuite:

  import mcp4s.server.dsl.*

  private val minimalCtx =
    ToolContext.minimal[IO](SamplingRequester.unsupported[IO], RequestId.NullId)

  // === Tool: derived name + description ===

  @description("Add two numbers")
  case class Add(
      @description("First number") a: Double,
      @description("Second number") b: Double
  ) derives Schema

  test("Tool with derived name and description") {
    val add = Tool.from[Add].handle[IO] { args =>
      IO.pure(ok(TestNum.str(args.a + args.b)))
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
    val custom = Tool.from[Add].withName("custom-add").handle[IO] { args =>
      IO.pure(ok(TestNum.str(args.a + args.b)))
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

  // === Tool with pure text handler: derived name ===

  @description("Echo a message")
  case class Echo(@description("Message") message: String) derives Schema

  test("Tool with pure text handler, derived name and description") {
    val echo = Tool.from[Echo].handle[IO] { args =>
      IO.pure(ok(s"Echo: ${args.message}"))
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

  // === Tool.handleWith (context-aware): derived name ===

  @description("Context-aware tool")
  case class CtxArgs(@description("Input") input: String) derives Schema

  test("Tool.handleWith with derived name and description") {
    val ctxTool = Tool.from[CtxArgs].handleWith[IO] { (args, ctx) =>
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

  test("Tool.handleWith with custom name and derived description") {
    val ctxTool = Tool.from[CtxArgs].withName("custom-ctx").handleWith[IO] { (args, _) =>
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
  case class SearchArgs(@description("Query") query: String) derives Schema

  test("Tool derives name with Args suffix stripped") {
    val search = Tool.from[SearchArgs].handle[IO] { args =>
      IO.pure(ok(s"Searching: ${args.query}"))
    }

    for
      tools <- search.list
      _ = assertEquals(tools.head.name, "search")
      _ = assertEquals(tools.head.description, Some("Search for items"))
    yield ()
  }

  // === Tool with no class-level description ===

  case class NoDesc(@description("Value") value: String) derives Schema

  test("Tool with no class-level description has no description") {
    val tool = Tool.from[NoDesc].handle[IO] { args =>
      IO.pure(ok(args.value))
    }

    for
      tools <- tool.list
      _ = assertEquals(tools.head.name, "no_desc")
      _ = assertEquals(tools.head.description, None)
    yield ()
  }

  // === Prompt: derived name + description ===

  @description("A greeting prompt")
  case class Greet(@description("Name") name: String) derives Schema

  test("Prompt with derived name and description") {
    val greet = Prompt.from[Greet].handle[IO] { args =>
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
    val greet = Prompt.from[Greet].withName("custom-greet").handle[IO] { args =>
      IO.pure(messages(user(s"Hello, ${args.name}!")))
    }

    for
      prompts <- greet.list
      _ = assertEquals(prompts.head.name, "custom-greet")
      _ = assertEquals(prompts.head.description, Some("A greeting prompt"))
    yield ()
  }
