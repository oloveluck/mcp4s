package mcp4s.server.testing

import cats.effect.IO
import cats.syntax.semigroup.*
import io.circe.{Encoder, Json}
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.testing.McpToolsTest.*
import munit.CatsEffectSuite

class McpTestingSpec extends CatsEffectSuite:

  // === McpToolsTest Extension Tests ===

  val calcTools: McpTools[IO] =
    McpTool.twoNumbersPure[IO]("add", "Add") { (a, b) => s"${a + b}" } |+|
    McpTool.twoNumbersPure[IO]("subtract", "Subtract") { (a, b) => s"${a - b}" }

  test("testCall with typed arguments") {
    case class AddArgs(a: Double, b: Double) derives ToolInput, Encoder.AsObject

    for
      result <- calcTools.testCall("add", AddArgs(3.0, 2.0))
      _ = assertEquals(result.textContent, "5.0")
    yield ()
  }

  test("testCall with Json arguments") {
    for
      result <- calcTools.testCallJson("add", Json.obj("a" -> 3.asJson, "b" -> 2.asJson))
      _ = assertEquals(result.textContent, "5.0")
    yield ()
  }

  test("testCall raises ToolNotFound for unknown tool") {
    for
      result <- calcTools.testCall("unknown", Json.obj()).attempt
      _ = assert(result.isLeft)
      _ = assert(result.left.exists(_.isInstanceOf[McpError.ToolNotFound]))
    yield ()
  }

  test("hasTool returns true for existing tool") {
    for
      exists <- calcTools.hasTool("add")
      _ = assert(exists)
    yield ()
  }

  test("hasTool returns false for non-existent tool") {
    for
      exists <- calcTools.hasTool("multiply")
      _ = assert(!exists)
    yield ()
  }

  test("getTool returns Some for existing tool") {
    for
      toolOpt <- calcTools.getTool("add")
      _ = assert(toolOpt.isDefined)
      _ = assertEquals(toolOpt.get.name, "add")
    yield ()
  }

  test("getTool returns None for non-existent tool") {
    for
      toolOpt <- calcTools.getTool("multiply")
      _ = assertEquals(toolOpt, None)
    yield ()
  }

  test("assertTool returns tool definition") {
    for
      tool <- calcTools.assertTool("add")
      _ = assertEquals(tool.name, "add")
      _ = assertEquals(tool.description, Some("Add"))
    yield ()
  }

  test("assertTool raises AssertionError for non-existent tool") {
    for
      result <- calcTools.assertTool("multiply").attempt
      _ = assert(result.isLeft)
      _ = assert(result.left.exists(_.isInstanceOf[AssertionError]))
    yield ()
  }

  // === args Helper Tests ===

  test("args with single key-value pair") {
    val json = args("name" -> "Alice")
    assertEquals(json, Json.obj("name" -> "Alice".asJson))
  }

  test("args with two key-value pairs") {
    val json = args("a" -> 1.0, "b" -> 2.0)
    assertEquals(json, Json.obj("a" -> 1.0.asJson, "b" -> 2.0.asJson))
  }

  test("args with three key-value pairs") {
    val json = args("x" -> 1, "y" -> 2, "z" -> 3)
    assertEquals(json, Json.obj("x" -> 1.asJson, "y" -> 2.asJson, "z" -> 3.asJson))
  }

  test("args with four key-value pairs") {
    val json = args("a" -> 1, "b" -> "two", "c" -> true, "d" -> 4.0)
    assertEquals(json, Json.obj(
      "a" -> 1.asJson,
      "b" -> "two".asJson,
      "c" -> true.asJson,
      "d" -> 4.0.asJson
    ))
  }

  test("args.empty returns empty object") {
    assertEquals(args.empty, Json.obj())
  }

  // === McpServerTest Tests ===

  val testServer: McpServer[IO] = McpServer.from[IO](
    info = ServerInfo("test-server", "1.0.0"),
    tools = calcTools,
    resources = McpResource[IO]("test://readme", "README")("Hello world"),
    prompts = McpPrompt.noArgs[IO]("greet", "Greet") {
      IO.pure(GetPromptResult(None, List(PromptMessage(Role.User, TextContent("Hi")))))
    }
  )

  test("McpServerTest.sync creates test client") {
    val client = McpServerTest.sync(testServer)

    assertEquals(client.serverInfo.name, "test-server")
    assertEquals(client.serverInfo.version, "1.0.0")
  }

  test("McpServerTest lists tools") {
    McpServerTest(testServer).use { client =>
      for
        tools <- client.listTools
        _ = assertEquals(tools.map(_.name).toSet, Set("add", "subtract"))
      yield ()
    }
  }

  test("McpServerTest calls tools with typed args") {
    case class CalcArgs(a: Double, b: Double) derives ToolInput, Encoder.AsObject

    McpServerTest(testServer).use { client =>
      for
        result <- client.callTool("add", CalcArgs(10, 5))
        _ = assertEquals(result.textContent, "15.0")
      yield ()
    }
  }

  test("McpServerTest calls tools with Json args") {
    McpServerTest(testServer).use { client =>
      for
        result <- client.callToolJson("subtract", Json.obj("a" -> 10.asJson, "b" -> 3.asJson))
        _ = assertEquals(result.textContent, "7.0")
      yield ()
    }
  }

  test("McpServerTest lists resources") {
    McpServerTest(testServer).use { client =>
      for
        resources <- client.listResources
        _ = assertEquals(resources.map(_.uri), List("test://readme"))
      yield ()
    }
  }

  test("McpServerTest reads resources") {
    McpServerTest(testServer).use { client =>
      for
        content <- client.readResource("test://readme")
        _ = assertEquals(content.text, Some("Hello world"))
      yield ()
    }
  }

  test("McpServerTest lists prompts") {
    McpServerTest(testServer).use { client =>
      for
        prompts <- client.listPrompts
        _ = assertEquals(prompts.map(_.name), List("greet"))
      yield ()
    }
  }

  test("McpServerTest gets prompts") {
    McpServerTest(testServer).use { client =>
      for
        result <- client.getPromptMap("greet", Map.empty)
        _ = assertEquals(result.messages.length, 1)
        _ = assertEquals(result.messages.head.content.asInstanceOf[TextContent].text, "Hi")
      yield ()
    }
  }
