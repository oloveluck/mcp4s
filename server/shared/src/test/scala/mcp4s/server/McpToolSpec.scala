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
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpToolSpec extends CatsEffectSuite:

  private val minimalCtx =
    ToolContext.minimal[IO](SamplingRequester.unsupported[IO], RequestId.NullId)

  case class CalcArgs(
      @description("First number") a: Double,
      @description("Second number") b: Double
  ) derives ToolInput

  // === McpTool composition Tests ===

  test("McpTool values compose with |+|") {
    val add = McpTool[IO, CalcArgs]("add", "Add") { args =>
      IO.pure(ToolResult.text(TestNum.str(args.a + args.b)))
    }

    val subtract = McpTool[IO, CalcArgs]("subtract", "Subtract") { args =>
      IO.pure(ToolResult.text(TestNum.str(args.a - args.b)))
    }

    val mathTools = add |+| subtract

    for
      tools <- mathTools.list
      _ = assertEquals(tools.map(_.name).toSet, Set("add", "subtract"))
      addResult <- mathTools
        .call("add", Json.obj("a" -> 3.asJson, "b" -> 2.asJson), minimalCtx)
        .value
      subResult <- mathTools
        .call("subtract", Json.obj("a" -> 3.asJson, "b" -> 2.asJson), minimalCtx)
        .value
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
      result <- ping.call("ping", Json.obj(), minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("pong"))
    yield ()
  }

  // === McpTool with ToolInput ===

  case class ReadArgs(@description("File path") path: String) derives ToolInput

  test("McpTool.annotated creates tool with annotations") {
    val readTool = McpTool.annotated[IO, ReadArgs](
      "read",
      "Read data",
      ToolAnnotations.readOnly()
    ) { args =>
      IO.pure(ToolResult.text(s"data from ${args.path}"))
    }

    for
      tools <- readTool.list
      _ = assert(tools.head.annotations.isDefined)
      _ = assertEquals(tools.head.annotations.get.readOnlyHint, Some(true))
    yield ()
  }

  test("McpTool works with derived ToolInput") {
    val add = McpTool[IO, CalcArgs]("add", "Add") { args =>
      IO.pure(ToolResult.text(TestNum.str(args.a + args.b)))
    }

    val json = Json.obj("a" -> Json.fromDoubleOrNull(1.0), "b" -> Json.fromDoubleOrNull(2.0))
    for
      tools <- add.list
      _ = assertEquals(tools.head.inputSchema.properties.get("a").description, Some("First number"))
      result <- add.call("add", json, minimalCtx).value
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
      result <- add.call("add", Json.obj("a" -> 3.asJson, "b" -> 2.asJson), minimalCtx).value
      _ = assert(result.isDefined)
      _ = assert(result.get.structuredContent.isDefined)
    yield ()
  }

  // === Declarative Server.from Tests ===

  test("Server.from creates server from composed parts") {
    val add = McpTool[IO, CalcArgs]("add", "Add") { args =>
      IO.pure(ToolResult.text(TestNum.str(args.a + args.b)))
    }

    val readme = McpResource[IO]("test://readme", "README")("Hello world")

    val greeting = McpPrompt.noArgs[IO]("greet", "Greet") {
      IO.pure(GetPromptResult(None, List(PromptMessage(Role.User, TextContent("Hi")))))
    }

    val server = Server.from[IO](
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

  test("Server.fromTools composes multiple tools with |+|") {
    val add = McpTool[IO, CalcArgs]("add", "Add") { args =>
      IO.pure(ToolResult.text(TestNum.str(args.a + args.b)))
    }

    val mul = McpTool[IO, CalcArgs]("multiply", "Multiply") { args =>
      IO.pure(ToolResult.text(TestNum.str(args.a * args.b)))
    }

    val server = Server.fromTools[IO](
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

  test("Server.fromTools raises ToolNotFound for unknown tool") {
    val server = Server.fromTools[IO](
      info = ServerInfo("test", "1.0.0"),
      tools = Tools.empty[IO]
    )
    for
      result <- server.callTool("nonexistent", Json.obj()).attempt
      _ = assert(result.isLeft)
      _ = assert(result.left.exists(_.isInstanceOf[McpError.ToolNotFound]))
    yield ()
  }

  test("Server.from raises ResourceNotFound for unknown resource") {
    val server = Server.from[IO](
      info = ServerInfo("test", "1.0.0"),
      tools = Tools.empty[IO],
      resources = Resources.empty[IO],
      prompts = Prompts.empty[IO]
    )
    for
      result <- server.readResource("test://unknown").attempt
      _ = assert(result.isLeft)
      _ = assert(result.left.exists(_.isInstanceOf[McpError.ResourceNotFound]))
    yield ()
  }

  test("Server.from raises PromptNotFound for unknown prompt") {
    val server = Server.from[IO](
      info = ServerInfo("test", "1.0.0"),
      tools = Tools.empty[IO],
      resources = Resources.empty[IO],
      prompts = Prompts.empty[IO]
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
      result <- echo.call("echo", json, minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("Echo: hello"))
    yield ()
  }

  test("McpTool.pureTextNoArgs creates no-arg tool with pure result") {
    val version = McpTool.pureTextNoArgs[IO]("version", "Get version") {
      "1.0.0"
    }

    for
      result <- version.call("version", Json.obj(), minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("1.0.0"))
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
    assertEquals(result.isError.getOrElse(false), false)
  }

  test("McpResult.toolError creates error result") {
    val result = McpResult.toolError("Failed")
    assertEquals(result.textContent, "Failed")
    assertEquals(result.isError.getOrElse(false), true)
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

  test("McpTool.withContext returns Tools for composition") {
    case class QueryArgs(query: String) derives ToolInput

    val regular = McpTool.pureText[IO, CalcArgs]("add", "Add") { args =>
      TestNum.str(args.a + args.b)
    }

    val contextAware = McpTool.withContext[IO, QueryArgs]("smart", "Smart") { (args, _) =>
      IO.pure(ToolResult.text(s"Query: ${args.query}"))
    }

    // Should compile: regular and context tools compose together
    val combined = regular |+| contextAware

    for
      tools <- combined.list
      _ = assertEquals(tools.map(_.name).toSet, Set("add", "smart"))
      addResult <- combined
        .call("add", Json.obj("a" -> 1.asJson, "b" -> 2.asJson), minimalCtx)
        .value
      smartResult <- combined.call("smart", Json.obj("query" -> "test".asJson), minimalCtx).value
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
      result <- pingTool.call("ping", Json.obj(), minimalCtx).value
      _ = assert(result.isDefined)
      _ = assert(result.get.textContent.startsWith("pong"))
    yield ()
  }
