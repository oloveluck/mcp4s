package mcp4s.protocol

import io.circe.*
import io.circe.generic.semiauto.*
import munit.FunSuite

class ToolInputSpec extends FunSuite:

  // === Basic Derivation Tests ===

  case class SimpleArgs(name: String, count: Int)
  object SimpleArgs:
    given Decoder[SimpleArgs] = deriveDecoder
    given ToolInput[SimpleArgs] = ToolInput.derived

  test("ToolInput derives for simple case class") {
    val ti = summon[ToolInput[SimpleArgs]]
    assertEquals(ti.schema.`type`, "object")
    assert(ti.schema.properties.isDefined)
    assertEquals(ti.schema.properties.get.size, 2)
  }

  test("ToolInput schema has correct field types") {
    val ti = summon[ToolInput[SimpleArgs]]
    val props = ti.schema.properties.get
    assertEquals(props("name").`type`.get, "string")
    assertEquals(props("count").`type`.get, "integer")
  }

  test("ToolInput schema has required fields") {
    val ti = summon[ToolInput[SimpleArgs]]
    assertEquals(ti.schema.required, Some(List("name", "count")))
  }

  test("ToolInput decodes valid JSON") {
    val ti = summon[ToolInput[SimpleArgs]]
    val json = Json.obj("name" -> Json.fromString("test"), "count" -> Json.fromInt(42))
    assertEquals(ti.decode(json), Right(SimpleArgs("test", 42)))
  }

  test("ToolInput decode fails for invalid JSON") {
    val ti = summon[ToolInput[SimpleArgs]]
    val json = Json.obj("name" -> Json.fromString("test")) // missing count
    assert(ti.decode(json).isLeft)
  }

  // === Description Annotation Tests ===

  case class DescribedArgs(
      @description("The user's name") name: String,
      @description("Age in years") age: Int
  )
  object DescribedArgs:
    given Decoder[DescribedArgs] = deriveDecoder
    given ToolInput[DescribedArgs] = ToolInput.derived

  test("ToolInput extracts @description annotations") {
    val ti = summon[ToolInput[DescribedArgs]]
    val props = ti.schema.properties.get
    assertEquals(props("name").description, Some("The user's name"))
    assertEquals(props("age").description, Some("Age in years"))
  }

  case class PartiallyDescribedArgs(
      @description("Has description") described: String,
      undescribed: String
  )
  object PartiallyDescribedArgs:
    given Decoder[PartiallyDescribedArgs] = deriveDecoder
    given ToolInput[PartiallyDescribedArgs] = ToolInput.derived

  test("ToolInput handles partial @description annotations") {
    val ti = summon[ToolInput[PartiallyDescribedArgs]]
    val props = ti.schema.properties.get
    assertEquals(props("described").description, Some("Has description"))
    assertEquals(props("undescribed").description, None)
  }

  // === Type Mapping Tests ===

  case class AllTypesArgs(
      s: String,
      i: Int,
      l: Long,
      d: Double,
      f: Float,
      b: Boolean
  )
  object AllTypesArgs:
    given Decoder[AllTypesArgs] = deriveDecoder
    given ToolInput[AllTypesArgs] = ToolInput.derived

  test("ToolInput maps Scala types to JSON schema types") {
    val ti = summon[ToolInput[AllTypesArgs]]
    val props = ti.schema.properties.get
    assertEquals(props("s").`type`.get, "string")
    assertEquals(props("i").`type`.get, "integer")
    assertEquals(props("l").`type`.get, "integer")
    assertEquals(props("d").`type`.get, "number")
    assertEquals(props("f").`type`.get, "number")
    assertEquals(props("b").`type`.get, "boolean")
  }

  case class OptionalArgs(
      required: String,
      optional: Option[String]
  )
  object OptionalArgs:
    given Decoder[OptionalArgs] = deriveDecoder
    given ToolInput[OptionalArgs] = ToolInput.derived

  test("ToolInput handles optional fields") {
    val ti = summon[ToolInput[OptionalArgs]]
    val props = ti.schema.properties.get
    // Both should be present but optional fields map to string type
    assertEquals(props("required").`type`.get, "string")
    assertEquals(props("optional").`type`.get, "string")
  }

  // === Calculator Example Pattern ===

  case class AddArgs(
      @description("First number") a: Double,
      @description("Second number") b: Double
  )
  object AddArgs:
    given Decoder[AddArgs] = deriveDecoder
    given ToolInput[AddArgs] = ToolInput.derived

  test("Calculator AddArgs has correct schema") {
    val ti = summon[ToolInput[AddArgs]]
    assertEquals(ti.schema.`type`, "object")

    val props = ti.schema.properties.get
    assertEquals(props("a").`type`.get, "number")
    assertEquals(props("a").description, Some("First number"))
    assertEquals(props("b").`type`.get, "number")
    assertEquals(props("b").description, Some("Second number"))

    assertEquals(ti.schema.required, Some(List("a", "b")))
  }

  test("Calculator AddArgs decodes correctly") {
    val ti = summon[ToolInput[AddArgs]]
    val json = Json.obj("a" -> Json.fromDouble(1.5).get, "b" -> Json.fromDouble(2.5).get)
    assertEquals(ti.decode(json), Right(AddArgs(1.5, 2.5)))
  }

  // === fieldDescriptions Macro Tests ===

  test("fieldDescriptions extracts all annotations") {
    val descriptions = ToolInput.fieldDescriptions[DescribedArgs]
    assertEquals(descriptions, Map("name" -> "The user's name", "age" -> "Age in years"))
  }

  test("fieldDescriptions returns empty map for unannotated class") {
    val descriptions = ToolInput.fieldDescriptions[SimpleArgs]
    assertEquals(descriptions, Map.empty[String, String])
  }
