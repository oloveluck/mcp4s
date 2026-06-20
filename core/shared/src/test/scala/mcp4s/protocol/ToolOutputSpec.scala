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

package mcp4s.protocol

import io.circe.*
import io.circe.generic.semiauto.*
import munit.FunSuite

class ToolOutputSpec extends FunSuite:

  // === Primitive ToolOutput Tests ===

  test("String ToolOutput encodes to text content") {
    val to     = summon[ToolOutput[String]]
    val result = to.encode("hello")
    assertEquals(result.asText, Some("hello"))
    assert(result.structuredContent.isDefined)
  }

  test("String ToolOutput has correct schema") {
    val to = summon[ToolOutput[String]]
    assertEquals(to.schema.`type`, "object")
    val props = to.schema.properties.get
    assertEquals(props("result").`type`.get, "string")
  }

  test("Double ToolOutput encodes to text content") {
    val to     = summon[ToolOutput[Double]]
    val result = to.encode(3.14)
    assertEquals(result.asText, Some("3.14"))
    assert(result.structuredContent.isDefined)
  }

  test("Double ToolOutput has correct schema") {
    val to    = summon[ToolOutput[Double]]
    val props = to.schema.properties.get
    assertEquals(props("result").`type`.get, "number")
  }

  test("Int ToolOutput encodes correctly") {
    val to     = summon[ToolOutput[Int]]
    val result = to.encode(42)
    assertEquals(result.asText, Some("42"))
  }

  test("Boolean ToolOutput encodes correctly") {
    val to     = summon[ToolOutput[Boolean]]
    val result = to.encode(true)
    assertEquals(result.asText, Some("true"))
  }

  test("Json ToolOutput encodes correctly") {
    val to     = summon[ToolOutput[Json]]
    val json   = Json.obj("key" -> Json.fromString("value"))
    val result = to.encode(json)
    assertEquals(result.structuredContent, Some(json))
  }

  // === Derived ToolOutput Tests ===

  case class CalcResult(
      @description("The calculation result") result: Double,
      @description("The operation performed") operation: String
  )
  object CalcResult:
    given Encoder[CalcResult] = deriveEncoder

  test("derived ToolOutput has correct schema") {
    val to = ToolOutput.derived[CalcResult]
    assertEquals(to.schema.`type`, "object")
    val props = to.schema.properties.get
    assertEquals(props("result").`type`.get, "number")
    assertEquals(props("result").description, Some("The calculation result"))
    assertEquals(props("operation").`type`.get, "string")
    assertEquals(props("operation").description, Some("The operation performed"))
    assertEquals(to.schema.required, Some(List("result", "operation")))
  }

  test("derived ToolOutput encodes to structured content") {
    val to     = ToolOutput.derived[CalcResult]
    val result = to.encode(CalcResult(42.0, "add"))
    assert(result.structuredContent.isDefined)
    val json = result.structuredContent.get
    assertEquals(json.hcursor.get[Double]("result"), Right(42.0))
    assertEquals(json.hcursor.get[String]("operation"), Right("add"))
  }

  test("derived ToolOutput also provides text content") {
    val to     = ToolOutput.derived[CalcResult]
    val result = to.encode(CalcResult(42.0, "add"))
    assert(result.asText.isDefined)
  }

  // === Option field tests ===

  case class OutputWithOptional(
      @description("Always present") name: String,
      @description("Sometimes present") tag: Option[String]
  )
  object OutputWithOptional:
    given Encoder[OutputWithOptional] = deriveEncoder

  test("derived ToolOutput excludes optional fields from required") {
    val to = ToolOutput.derived[OutputWithOptional]
    assertEquals(to.schema.required, Some(List("name")))
  }

  test("derived ToolOutput maps Option[String] to string type") {
    val to    = ToolOutput.derived[OutputWithOptional]
    val props = to.schema.properties.get
    assertEquals(props("tag").`type`.get, "string")
  }

  case class OutputAllOptional(
      a: Option[Int],
      b: Option[Double]
  )
  object OutputAllOptional:
    given Encoder[OutputAllOptional] = deriveEncoder

  test("derived ToolOutput with all optional has no required") {
    val to = ToolOutput.derived[OutputAllOptional]
    assertEquals(to.schema.required, None)
  }

  test("derived ToolOutput Option[Int] maps to integer") {
    val to    = ToolOutput.derived[OutputAllOptional]
    val props = to.schema.properties.get
    assertEquals(props("a").`type`.get, "integer")
    assertEquals(props("b").`type`.get, "number")
  }

  // === Array field tests ===

  case class OutputWithArrays(
      tags: List[String],
      scores: List[Int]
  )
  object OutputWithArrays:
    given Encoder[OutputWithArrays] = deriveEncoder

  test("derived ToolOutput List fields produce array type with items") {
    val to    = ToolOutput.derived[OutputWithArrays]
    val props = to.schema.properties.get
    assertEquals(props("tags").`type`.get, "array")
    assert(props("tags").items.isDefined, "List[String] should have items")
    assertEquals(props("tags").items.get.`type`.get, "string")
    assertEquals(props("scores").`type`.get, "array")
    assert(props("scores").items.isDefined, "List[Int] should have items")
    assertEquals(props("scores").items.get.`type`.get, "integer")
  }
