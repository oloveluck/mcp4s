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

package mcp4s.server.testing

import cats.effect.IO
import cats.syntax.semigroup.*
import io.circe.{Encoder, Json}
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.testing.ToolsTest.*
import munit.CatsEffectSuite
import mcp4s.server.TestSyntax.*

class McpTestingSpec extends CatsEffectSuite:

  import mcp4s.server.dsl.*

  // === ToolsTest Extension Tests ===

  case class CalcArgs(a: Double, b: Double) derives Schema

  val calcTools: Tools[IO] =
    Tool("add").withDescription("Add").input[CalcArgs].handle[IO] { args =>
      IO.pure(ok(TestNum.str(args.a + args.b)))
    } |+|
      Tool("subtract").withDescription("Subtract").input[CalcArgs].handle[IO] { args =>
        IO.pure(ok(TestNum.str(args.a - args.b)))
      }

  test("testCall with typed arguments") {
    case class AddArgs(a: Double, b: Double) derives Encoder.AsObject

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
    interceptIO[McpError.ToolNotFound](calcTools.testCall("unknown", Json.obj())).void
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
    interceptIO[AssertionError](calcTools.assertTool("multiply")).void
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
    assertEquals(
      json,
      Json.obj(
        "a" -> 1.asJson,
        "b" -> "two".asJson,
        "c" -> true.asJson,
        "d" -> 4.0.asJson
      )
    )
  }

  test("args.empty returns empty object") {
    assertEquals(args.empty, Json.obj())
  }

  // === ServerTest Tests ===

  val testServer: Server[IO] = Server.from[IO](
    info = ServerInfo("test-server", "1.0.0"),
    tools = calcTools,
    resources = Resource.text[IO]("test://readme", "README")("Hello world"),
    prompts = Prompt("greet").withDescription("Greet").messages[IO](user("Hi"))
  )

  test("ServerTest.sync creates test client") {
    val client = ServerTest.sync(testServer)

    assertEquals(client.serverInfo.name, "test-server")
    assertEquals(client.serverInfo.version, "1.0.0")
  }

  test("ServerTest lists tools") {
    ServerTest(testServer).use: client =>
      for
        tools <- client.listTools
        _ = assertEquals(tools.map(_.name).toSet, Set("add", "subtract"))
      yield ()
  }

  test("ServerTest calls tools with typed args") {
    case class CalcArgs(a: Double, b: Double) derives Encoder.AsObject

    ServerTest(testServer).use: client =>
      for
        result <- client.callTool("add", CalcArgs(10, 5))
        _ = assertEquals(result.textContent, "15.0")
      yield ()
  }

  test("ServerTest calls tools with Json args") {
    ServerTest(testServer).use: client =>
      for
        result <- client.callToolJson("subtract", Json.obj("a" -> 10.asJson, "b" -> 3.asJson))
        _ = assertEquals(result.textContent, "7.0")
      yield ()
  }

  test("ServerTest lists resources") {
    ServerTest(testServer).use: client =>
      for
        resources <- client.listResources
        _ = assertEquals(resources.map(_.uri), List("test://readme"))
      yield ()
  }

  test("ServerTest reads resources") {
    ServerTest(testServer).use: client =>
      for
        content <- client.readResource("test://readme")
        _ = assertEquals(content.text, Some("Hello world"))
      yield ()
  }

  test("ServerTest lists prompts") {
    ServerTest(testServer).use: client =>
      for
        prompts <- client.listPrompts
        _ = assertEquals(prompts.map(_.name), List("greet"))
      yield ()
  }

  test("ServerTest gets prompts") {
    ServerTest(testServer).use: client =>
      for
        result <- client.getPromptMap("greet", Map.empty)
        _ = assertEquals(result.messages.length, 1)
        _ = assertEquals(textOf(result.messages.head.content), "Hi")
      yield ()
  }
