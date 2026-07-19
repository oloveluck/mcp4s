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

class McpDslSpec extends CatsEffectSuite:

  import mcp4s.server.dsl.*

  private val minimalCtx =
    ToolContext.minimal[IO](SamplingRequester.unsupported[IO], RequestId.NullId)

  // === Result Builder Tests ===

  test("ok creates text tool result") {
    val result = ok("Success!")
    assertEquals(result.textContent, "Success!")
    assertEquals(result.isError.getOrElse(false), false)
  }

  test("error creates error tool result") {
    val result = error("Failed!")
    assertEquals(result.textContent, "Failed!")
    assertEquals(result.isError.getOrElse(false), true)
  }

  test("content creates result from multiple content items") {
    val result = content(textContent("Hello"), textContent("World"))
    assertEquals(result.content.size, 2)
    assertEquals(result.content(0).asInstanceOf[TextContent].text, "Hello")
    assertEquals(result.content(1).asInstanceOf[TextContent].text, "World")
  }

  test("text creates resource content") {
    val rc = text("test://uri", "content")
    assertEquals(rc.uri, "test://uri")
    assertEquals(rc.text, Some("content"))
  }

  test("blob creates blob resource content") {
    val rc = blob("test://uri", "data123", "application/octet-stream")
    assertEquals(rc.uri, "test://uri")
    assertEquals(rc.blob, Some("data123"))
    assertEquals(rc.mimeType, Some("application/octet-stream"))
  }

  test("messages creates prompt result without description") {
    val result = messages(user("Hello"), assistant("Hi"))
    assertEquals(result.description, None)
    assertEquals(result.messages.size, 2)
    assertEquals(result.messages(0).role, Role.User)
    assertEquals(result.messages(1).role, Role.Assistant)
  }

  test("messages with description creates prompt result with description") {
    val result = messages("A greeting")(user("Hello"))
    assertEquals(result.description, Some("A greeting"))
    assertEquals(result.messages.size, 1)
  }

  // === Content Builder Tests ===

  test("textContent creates TextContent") {
    val tc = textContent("Hello")
    assertEquals(tc.text, "Hello")
  }

  test("imageContent creates ImageContent") {
    val ic = imageContent("base64data", "image/png")
    assertEquals(ic.data, "base64data")
    assertEquals(ic.mimeType, "image/png")
  }

  test("audioContent creates AudioContent") {
    val ac = audioContent("base64data", "audio/wav")
    assertEquals(ac.data, "base64data")
    assertEquals(ac.mimeType, "audio/wav")
  }

  // === Message Builder Tests ===

  test("user creates user message with text") {
    val msg = user("Hello!")
    assertEquals(msg.role, Role.User)
    assertEquals(msg.content.asInstanceOf[TextContent].text, "Hello!")
  }

  test("user with content creates user message with custom content") {
    val msg = user(imageContent("data", "image/png"))
    assertEquals(msg.role, Role.User)
    assert(msg.content.isInstanceOf[ImageContent])
  }

  test("assistant creates assistant message with text") {
    val msg = assistant("Hi there!")
    assertEquals(msg.role, Role.Assistant)
    assertEquals(msg.content.asInstanceOf[TextContent].text, "Hi there!")
  }

  // === Tool Constructor Tests ===

  test("Tool with pure string handler (no args)") {
    val version = Tool("version").withDescription("Get version").handle[IO] { _ =>
      IO.pure(ok("1.0.0"))
    }

    for
      tools <- version.list
      _ = assertEquals(tools.size, 1)
      _ = assertEquals(tools.head.name, "version")
      _ = assertEquals(tools.head.description, Some("Get version"))
      result <- version.call("version", Json.obj(), minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("1.0.0"))
    yield ()
  }

  case class EchoArgs(message: String) derives Schema

  test("Tool with pure string handler (with args)") {
    val echo = Tool("echo").withDescription("Echo message").input[EchoArgs].handle[IO] { args =>
      IO.pure(ok(s"Echo: ${args.message}"))
    }

    val json = Json.obj("message" -> "hello".asJson)
    for
      result <- echo.call("echo", json, minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("Echo: hello"))
    yield ()
  }

  case class AddArgs(a: Double, b: Double) derives Schema

  test("Tool creates tool with effectful handler (with args)") {
    val add = Tool("add").withDescription("Add numbers").input[AddArgs].handle[IO] { args =>
      IO.pure(ok(TestNum.str(args.a + args.b)))
    }

    val json = Json.obj("a" -> 3.asJson, "b" -> 2.asJson)
    for
      result <- add.call("add", json, minimalCtx).value
      _ = assertEquals(result.map(_.textContent), Some("5.0"))
    yield ()
  }

  test("Tool.handleWith creates context-aware tool (no args)") {
    val logTool = Tool("log").withDescription("Log something").handleWith[IO] { (_, ctx) =>
      IO.pure(ok(s"Request: ${ctx.requestId}"))
    }

    for
      tools <- logTool.list
      _ = assertEquals(tools.head.name, "log")
      result <- logTool.call("log", Json.obj(), minimalCtx).value
      _ = assert(result.isDefined)
      _ = assert(result.get.textContent.startsWith("Request:"))
    yield ()
  }

  case class QueryArgs(query: String) derives Schema

  test("Tool.handleWith creates context-aware tool (with args)") {
    val smart = Tool("smart").withDescription("Smart query").input[QueryArgs].handleWith[IO] {
      (args, ctx) =>
        IO.pure(ok(s"Query: ${args.query}, Request: ${ctx.requestId}"))
    }

    val json = Json.obj("query" -> "test".asJson)
    for
      result <- smart.call("smart", json, minimalCtx).value
      _ = assert(result.isDefined)
      _ = assert(result.get.textContent.contains("Query: test"))
    yield ()
  }

  // === Resource Constructor Tests ===

  test("Resource.text creates static text resource") {
    val readme = Resource.text[IO]("test://readme", "README") {
      "Hello world"
    }

    for
      resources <- readme.list
      _ = assertEquals(resources.size, 1)
      _ = assertEquals(resources.head.uri, "test://readme")
      _ = assertEquals(resources.head.name, "README")
      content <- readme.read("test://readme").value
      _ = assertEquals(content.map(_.text), Some(Some("Hello world")))
    yield ()
  }

  test("Resource.apply creates resource with effectful handler") {
    val config = Resource[IO]("test://config", "Config") {
      IO.pure(text("test://config", """{"key":"value"}"""))
    }

    for
      resources <- config.list
      _ = assertEquals(resources.head.uri, "test://config")
      content <- config.read("test://config").value
      _ = assertEquals(content.map(_.text), Some(Some("""{"key":"value"}""")))
    yield ()
  }

  test("Resource.template creates template resource that matches patterns") {
    val users = Resource.template[IO]("test://users/{id}", "User", "Get user by ID") { uri =>
      val id = uri.split("/").last
      IO.pure(text(uri, s"""{"id":"$id"}"""))
    }

    for
      resources <- users.list
      _ = assertEquals(resources, Nil) // Template has no static resources
      templates <- users.listTemplates
      _ = assertEquals(templates.size, 1)
      _ = assertEquals(templates.head.uriTemplate, "test://users/{id}")
      _ = assertEquals(templates.head.name, "User")
      _ = assertEquals(templates.head.description, Some("Get user by ID"))
      content <- users.read("test://users/123").value
      _ = assertEquals(content.map(_.text), Some(Some("""{"id":"123"}""")))
      noMatch <- users.read("test://other/path").value
      _ = assertEquals(noMatch, None)
    yield ()
  }

  // === Prompt Constructor Tests ===

  test("Prompt.messages creates prompt with messages (no args)") {
    val greeting = Prompt("greet").withDescription("A greeting").messages[IO](
      user("Hello!"),
      assistant("Hi there!")
    )

    for
      prompts <- greeting.list
      _ = assertEquals(prompts.size, 1)
      _ = assertEquals(prompts.head.name, "greet")
      _ = assertEquals(prompts.head.description, Some("A greeting"))
      result <- greeting.get("greet", Map.empty).value
      _ = assert(result.isDefined)
      _ = assertEquals(result.get.description, None)
      _ = assertEquals(result.get.messages.size, 2)
      _ = assertEquals(result.get.messages(0).content.asInstanceOf[TextContent].text, "Hello!")
    yield ()
  }

  test("Prompt.static creates prompt with result description") {
    val help = Prompt("help").withDescription("Help prompt").static[IO](
      messages("Get help with the system")(user("How can I help you?"))
    )

    for
      result <- help.get("help", Map.empty).value
      _ = assert(result.isDefined)
      _ = assertEquals(result.get.description, Some("Get help with the system"))
    yield ()
  }

  case class GreetArgs(name: String) derives Schema

  test("Prompt.handle creates prompt with typed args") {
    val greet = Prompt("greet").withDescription("Greet someone").input[GreetArgs].handle[IO] {
      args =>
        IO.pure(messages(user(s"Hello, ${args.name}!")))
    }

    for
      prompts <- greet.list
      _ = assert(prompts.head.arguments.exists(_.name == "name"))
      result <- greet.get("greet", Map("name" -> "Alice")).value
      _ = assertEquals(
        result.get.messages.head.content.asInstanceOf[TextContent].text,
        "Hello, Alice!"
      )
    yield ()
  }

  // === Pure Lifting Tests ===

  test("cats pure lifts value into IO") {
    import cats.syntax.applicative.*
    val result: IO[ToolResult] = ok("Success").pure[IO]
    for
      r <- result
      _ = assertEquals(r.textContent, "Success")
    yield ()
  }

  // === Composition Tests ===

  test("tools compose with |+|") {
    val tool1    = Tool("t1").withDescription("Tool 1").handle[IO](_ => IO.pure(ok("result1")))
    val tool2    = Tool("t2").withDescription("Tool 2").handle[IO](_ => IO.pure(ok("result2")))
    val combined = tool1 |+| tool2

    for
      tools <- combined.list
      _ = assertEquals(tools.map(_.name).toSet, Set("t1", "t2"))
      r1 <- combined.call("t1", Json.obj(), minimalCtx).value
      r2 <- combined.call("t2", Json.obj(), minimalCtx).value
      _ = assertEquals(r1.map(_.textContent), Some("result1"))
      _ = assertEquals(r2.map(_.textContent), Some("result2"))
    yield ()
  }

  test("resources compose with |+|") {
    val r1       = Resource.text[IO]("test://r1", "R1")("content1")
    val r2       = Resource.text[IO]("test://r2", "R2")("content2")
    val combined = r1 |+| r2

    for
      resources <- combined.list
      _ = assertEquals(resources.map(_.uri).toSet, Set("test://r1", "test://r2"))
      c1 <- combined.read("test://r1").value
      c2 <- combined.read("test://r2").value
      _ = assertEquals(c1.map(_.text), Some(Some("content1")))
      _ = assertEquals(c2.map(_.text), Some(Some("content2")))
    yield ()
  }

  test("prompts compose with |+|") {
    val p1       = Prompt("p1").withDescription("Prompt 1").messages[IO](user("Prompt 1 content"))
    val p2       = Prompt("p2").withDescription("Prompt 2").messages[IO](user("Prompt 2 content"))
    val combined = p1 |+| p2

    for
      prompts <- combined.list
      _ = assertEquals(prompts.map(_.name).toSet, Set("p1", "p2"))
      r1 <- combined.get("p1", Map.empty).value
      r2 <- combined.get("p2", Map.empty).value
      _ = assertEquals(
        r1.get.messages.head.content.asInstanceOf[TextContent].text,
        "Prompt 1 content"
      )
      _ = assertEquals(
        r2.get.messages.head.content.asInstanceOf[TextContent].text,
        "Prompt 2 content"
      )
    yield ()
  }

  // === Server.from Integration Tests ===

  test("composed tools work with Server.fromTools") {
    val tools =
      Tool("echo").withDescription("Echo").handle[IO](_ => IO.pure(ok("echo"))) |+|
        Tool("ping").withDescription("Ping").handle[IO](_ => IO.pure(ok("pong")))

    val server = Server.fromTools[IO](ServerInfo("test", "1.0.0"), tools)

    for
      toolList <- server.listTools
      _ = assertEquals(toolList.map(_.name).toSet, Set("echo", "ping"))
      echoResult <- server.callTool("echo", Json.obj())
      _ = assertEquals(echoResult.textContent, "echo")
      pingResult <- server.callTool("ping", Json.obj())
      _ = assertEquals(pingResult.textContent, "pong")
    yield ()
  }

  test("composed resources work with Server.from") {
    val resources =
      Resource.text[IO]("test://readme", "README")("Hello") |+|
        Resource.template[IO]("test://users/{id}", "User") { uri =>
          IO.pure(text(uri, s"User: ${uri.split("/").last}"))
        }

    val server =
      Server.from[IO](ServerInfo("test", "1.0.0"), Tools.empty[IO], resources, Prompts.empty[IO])

    for
      resourceList <- server.listResources
      _ = assertEquals(resourceList.map(_.uri), List("test://readme"))
      templateList <- server.listResourceTemplates
      _ = assertEquals(templateList.map(_.uriTemplate), List("test://users/{id}"))
      readmeContent <- server.readResource("test://readme")
      _ = assertEquals(readmeContent.text, Some("Hello"))
      userContent <- server.readResource("test://users/42")
      _ = assertEquals(userContent.text, Some("User: 42"))
    yield ()
  }

  test("composed prompts work with Server.from") {
    val prompts =
      Prompt("greet").withDescription("Greet").messages[IO](user("Hello!")) |+|
        Prompt("help")
          .withDescription("Help")
          .static[IO](messages("Get help")(user("How can I help?")))

    val server =
      Server.from[IO](ServerInfo("test", "1.0.0"), Tools.empty[IO], Resources.empty[IO], prompts)

    for
      promptList <- server.listPrompts
      _ = assertEquals(promptList.map(_.name).toSet, Set("greet", "help"))
      greetResult <- server.getPrompt("greet", Map.empty)
      _ = assertEquals(greetResult.messages.head.content.asInstanceOf[TextContent].text, "Hello!")
      helpResult <- server.getPrompt("help", Map.empty)
      _ = assertEquals(helpResult.description, Some("Get help"))
    yield ()
  }

  test("Server.from works with DSL-created values") {
    val tools     = Tool("echo").withDescription("Echo").handle[IO](_ => IO.pure(ok("echo")))
    val resources = Resource.text[IO]("test://readme", "README")("Hello")
    val prompts   = Prompt("greet").withDescription("Greet").messages[IO](user("Hello!"))

    val server = Server.from[IO](
      info = ServerInfo("test", "1.0.0"),
      tools = tools,
      resources = resources,
      prompts = prompts
    )

    for
      toolList <- server.listTools
      _ = assertEquals(toolList.map(_.name), List("echo"))
      resourceList <- server.listResources
      _ = assertEquals(resourceList.map(_.uri), List("test://readme"))
      promptList <- server.listPrompts
      _ = assertEquals(promptList.map(_.name), List("greet"))
    yield ()
  }
