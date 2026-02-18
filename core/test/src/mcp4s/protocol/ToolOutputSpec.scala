package mcp4s.protocol

import io.circe.*
import io.circe.generic.semiauto.*
import munit.FunSuite

class ToolOutputSpec extends FunSuite:

  // === Primitive ToolOutput Tests ===

  test("String ToolOutput encodes to text content") {
    val to = summon[ToolOutput[String]]
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
    val to = summon[ToolOutput[Double]]
    val result = to.encode(3.14)
    assertEquals(result.asText, Some("3.14"))
    assert(result.structuredContent.isDefined)
  }

  test("Double ToolOutput has correct schema") {
    val to = summon[ToolOutput[Double]]
    val props = to.schema.properties.get
    assertEquals(props("result").`type`.get, "number")
  }

  test("Int ToolOutput encodes correctly") {
    val to = summon[ToolOutput[Int]]
    val result = to.encode(42)
    assertEquals(result.asText, Some("42"))
  }

  test("Boolean ToolOutput encodes correctly") {
    val to = summon[ToolOutput[Boolean]]
    val result = to.encode(true)
    assertEquals(result.asText, Some("true"))
  }

  test("Json ToolOutput encodes correctly") {
    val to = summon[ToolOutput[Json]]
    val json = Json.obj("key" -> Json.fromString("value"))
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
    val to = ToolOutput.derived[CalcResult]
    val result = to.encode(CalcResult(42.0, "add"))
    assert(result.structuredContent.isDefined)
    val json = result.structuredContent.get
    assertEquals(json.hcursor.get[Double]("result"), Right(42.0))
    assertEquals(json.hcursor.get[String]("operation"), Right("add"))
  }

  test("derived ToolOutput also provides text content") {
    val to = ToolOutput.derived[CalcResult]
    val result = to.encode(CalcResult(42.0, "add"))
    assert(result.asText.isDefined)
  }
