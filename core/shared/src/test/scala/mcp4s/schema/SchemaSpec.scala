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

package mcp4s.schema

import io.circe.Json
import io.circe.syntax.*
import mcp4s.protocol.description
import munit.FunSuite

class SchemaSpec extends FunSuite:

  // === Fixtures ===

  case class Simple(name: String, count: Int) derives Schema

  case class Nested(
      @description("Tags to filter by") tags: List[String],
      limit: Option[Int]
  ) derives Schema

  case class Outer(
      @description("Search query") query: String,
      filters: Nested
  ) derives Schema

  enum Temperature derives Schema:
    case Celsius, Fahrenheit

  case class WithDefaults(
      query: String,
      limit: Int = 10,
      unit: Temperature = Temperature.Celsius
  ) derives Schema

  sealed trait Shape derives Schema
  case class Circle(radius: Double)                  extends Shape
  case class Rect(width: Double, height: Double)     extends Shape

  case class WithMap(labels: Map[String, Int]) derives Schema

  case class TreeNode(value: Int, children: List[TreeNode])
  object TreeNode:
    given Schema[TreeNode] = Schema.defer {
      import Schema.*
      Struct[TreeNode](
        "TreeNode",
        Vector(
          Field[TreeNode, Any]("value", Schema.int.asInstanceOf[Schema[Any]], _.value, None),
          Field[TreeNode, Any](
            "children",
            Collection[List, TreeNode](
              Lazily(() => summon[Schema[TreeNode]]),
              identity,
              identity
            ).asInstanceOf[Schema[Any]],
            _.children,
            None
          )
        ),
        values => TreeNode(values(0).asInstanceOf[Int], values(1).asInstanceOf[List[TreeNode]])
      )
    }

  // === Simple products ===

  test("simple product renders object schema with required fields") {
    val schema = Schema[Simple].jsonSchema
    assertEquals(schema.`type`, "object")
    assertEquals(schema.properties.get("name").`type`, Some("string"))
    assertEquals(schema.properties.get("count").`type`, Some("integer"))
    assertEquals(schema.required, Some(List("name", "count")))
  }

  test("simple product round-trips through codecs") {
    val value   = Simple("hello", 42)
    val json    = Schema[Simple].encoder(value)
    val decoded = Schema[Simple].decoder.decodeJson(json)
    assertEquals(decoded, Right(value))
  }

  // === Nested products ===

  test("nested case class renders full nested schema (no bare object collapse)") {
    val schema = Schema[Outer].jsonSchema
    val filters = schema.properties.get("filters")
    assertEquals(filters.`type`, Some("object"))
    val nestedProps = filters.properties.get
    assertEquals(nestedProps("tags").`type`, Some("array"))
    assertEquals(nestedProps("tags").items.get.`type`, Some("string"))
    assertEquals(nestedProps("tags").description, Some("Tags to filter by"))
    assertEquals(filters.required, Some(List("tags")))
  }

  test("nested case class round-trips") {
    val value   = Outer("q", Nested(List("a", "b"), Some(5)))
    val json    = Schema[Outer].encoder(value)
    val decoded = Schema[Outer].decoder.decodeJson(json)
    assertEquals(decoded, Right(value))
  }

  test("None optional fields are omitted when encoding") {
    val json = Schema[Nested].encoder(Nested(Nil, None))
    assertEquals(json.asObject.get.keys.toList, List("tags"))
  }

  test("missing optional fields decode to None") {
    val decoded = Schema[Nested].decoder.decodeJson(Json.obj("tags" -> Json.arr()))
    assertEquals(decoded, Right(Nested(Nil, None)))
  }

  // === Enums ===

  test("Scala 3 enum renders as string enum") {
    val prop = Schema[Temperature].property
    assertEquals(prop.`type`, Some("string"))
    assertEquals(prop.`enum`, Some(List("Celsius", "Fahrenheit")))
  }

  test("enum round-trips") {
    val json = Schema[Temperature].encoder(Temperature.Fahrenheit)
    assertEquals(json, Json.fromString("Fahrenheit"))
    assertEquals(
      Schema[Temperature].decoder.decodeJson(json),
      Right(Temperature.Fahrenheit)
    )
  }

  test("invalid enum value fails with a helpful message") {
    val result = Schema[Temperature].decoder.decodeJson(Json.fromString("Kelvin"))
    assert(result.isLeft)
    assert(result.left.exists(_.getMessage.contains("Celsius")))
  }

  // === Defaults ===

  test("fields with defaults are not required and carry default in schema") {
    val schema = Schema[WithDefaults].jsonSchema
    assertEquals(schema.required, Some(List("query")))
    assertEquals(schema.properties.get("limit").default, Some(Json.fromInt(10)))
    assertEquals(schema.properties.get("unit").default, Some(Json.fromString("Celsius")))
  }

  test("missing fields with defaults decode to the default") {
    val decoded = Schema[WithDefaults].decoder.decodeJson(Json.obj("query" -> "q".asJson))
    assertEquals(decoded, Right(WithDefaults("q", 10, Temperature.Celsius)))
  }

  // === Unions ===

  test("sealed trait with payloads renders oneOf with discriminator") {
    val prop = Schema[Shape].property
    val oneOf = prop.oneOf.get
    assertEquals(oneOf.size, 2)
    val circle = oneOf.head
    assertEquals(
      circle.hcursor.downField("properties").downField("type").downField("enum").as[List[String]],
      Right(List("Circle"))
    )
  }

  test("union round-trips with discriminator") {
    val shape: Shape = Circle(2.0)
    val json         = Schema[Shape].encoder(shape)
    assertEquals(json.hcursor.downField("type").as[String], Right("Circle"))
    assertEquals(json.hcursor.downField("radius").as[Double], Right(2.0))
    assertEquals(Schema[Shape].decoder.decodeJson(json), Right(shape))
  }

  test("unknown union discriminator fails with a helpful message") {
    val bad = Json.obj("type" -> "Triangle".asJson)
    val result = Schema[Shape].decoder.decodeJson(bad)
    assert(result.isLeft)
    assert(result.left.exists(_.getMessage.contains("Circle")))
  }

  // === Maps ===

  test("Map[String, V] renders additionalProperties and round-trips") {
    val prop = Schema[WithMap].jsonSchema.properties.get("labels")
    assertEquals(prop.`type`, Some("object"))
    assertEquals(prop.additionalProperties.get.`type`, Some("integer"))

    val value   = WithMap(Map("a" -> 1, "b" -> 2))
    val json    = Schema[WithMap].encoder(value)
    assertEquals(Schema[WithMap].decoder.decodeJson(json), Right(value))
  }

  // === Recursion ===

  test("recursive schema via defer round-trips") {
    val tree    = TreeNode(1, List(TreeNode(2, Nil), TreeNode(3, List(TreeNode(4, Nil)))))
    val json    = Schema[TreeNode].encoder(tree)
    val decoded = Schema[TreeNode].decoder.decodeJson(json)
    assertEquals(decoded, Right(tree))
  }

  test("recursive schema renders without infinite loop") {
    val schema = Schema[TreeNode].jsonSchema
    assertEquals(schema.`type`, "object")
    // The recursive occurrence is cut off as an untyped object
    val children = schema.properties.get("children")
    assertEquals(children.`type`, Some("array"))
  }

  // === Descriptions ===

  test("@description annotations flow into properties") {
    val schema = Schema[Outer].jsonSchema
    assertEquals(schema.properties.get("query").description, Some("Search query"))
  }

  test("withDescription overrides") {
    val s = Schema.string.withDescription("custom")
    assertEquals(s.property.description, Some("custom"))
  }
