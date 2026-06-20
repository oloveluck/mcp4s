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

import io.circe.{Encoder, Json}
import scala.compiletime.*
import scala.deriving.Mirror

/** Typeclass for tool output types that can be encoded to ToolResult and have a schema.
  *
  * Enables typed tool outputs with automatic `outputSchema` generation and `structuredContent`
  * serialization per MCP spec (2025-11-25).
  *
  * Example:
  * {{{
  * case class CalcResult(result: Double, operation: String) derives ToolOutput
  *
  * // Used with Tool DSL:
  * // import mcp4s.server.mcp.*
  * // Tool.typed[IO, AddArgs, CalcResult]("add", "Add two numbers") { args =>
  * //   IO.pure(CalcResult(args.a + args.b, "add"))
  * // }
  * }}}
  */
trait ToolOutput[A]:
  /** JSON schema for the output type */
  def schema: JsonSchema

  /** Encode a value to a ToolResult with structuredContent */
  def encode(a: A): ToolResult

object ToolOutput:

  def apply[A](using to: ToolOutput[A]): ToolOutput[A] = to

  /** Create a ToolOutput from a schema and encoder function */
  def instance[A](s: JsonSchema, enc: A => ToolResult): ToolOutput[A] =
    new ToolOutput[A]:
      def schema: JsonSchema       = s
      def encode(a: A): ToolResult = enc(a)

  // === Primitive instances ===

  given ToolOutput[String] with
    def schema: JsonSchema = JsonSchema(
      "object",
      Some(Map("result" -> JsonSchemaProperty.make("string"))),
      Some(List("result"))
    )
    def encode(a: String): ToolResult =
      ToolResult(
        List(TextContent(a)),
        structuredContent = Some(Json.obj("result" -> Json.fromString(a)))
      )

  given ToolOutput[Double] with
    def schema: JsonSchema = JsonSchema(
      "object",
      Some(Map("result" -> JsonSchemaProperty.make("number"))),
      Some(List("result"))
    )
    def encode(a: Double): ToolResult =
      ToolResult(
        List(TextContent(a.toString)),
        structuredContent = Some(Json.obj("result" -> Json.fromDoubleOrNull(a)))
      )

  given ToolOutput[Int] with
    def schema: JsonSchema = JsonSchema(
      "object",
      Some(Map("result" -> JsonSchemaProperty.make("integer"))),
      Some(List("result"))
    )
    def encode(a: Int): ToolResult =
      ToolResult(
        List(TextContent(a.toString)),
        structuredContent = Some(Json.obj("result" -> Json.fromInt(a)))
      )

  given ToolOutput[Long] with
    def schema: JsonSchema = JsonSchema(
      "object",
      Some(Map("result" -> JsonSchemaProperty.make("integer"))),
      Some(List("result"))
    )
    def encode(a: Long): ToolResult =
      ToolResult(
        List(TextContent(a.toString)),
        structuredContent = Some(Json.obj("result" -> Json.fromLong(a)))
      )

  given ToolOutput[Boolean] with
    def schema: JsonSchema = JsonSchema(
      "object",
      Some(Map("result" -> JsonSchemaProperty.make("boolean"))),
      Some(List("result"))
    )
    def encode(a: Boolean): ToolResult =
      ToolResult(
        List(TextContent(a.toString)),
        structuredContent = Some(Json.obj("result" -> Json.fromBoolean(a)))
      )

  given ToolOutput[Json] with
    def schema: JsonSchema = JsonSchema("object")
    def encode(a: Json): ToolResult =
      ToolResult(
        List(TextContent(a.noSpaces)),
        structuredContent = Some(a)
      )

  /** Derive ToolOutput for a product type (case class) */
  inline given derived[A <: Product](using m: Mirror.ProductOf[A], e: Encoder[A]): ToolOutput[A] =
    val labels       = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val schemas      = summonSchemas[m.MirroredElemTypes]
    val descriptions = ToolInput.fieldDescriptions[A]

    val properties = labels
      .zip(schemas)
      .map { (label, fs) =>
        val itemsProp = fs.items.map(t => JsonSchemaProperty.make(t))
        label -> JsonSchemaProperty.make(
          fs.typeName,
          descriptions.get(label),
          None,
          None,
          None,
          None,
          itemsProp
        )
      }
      .toMap

    val requiredFields =
      labels.zip(schemas).filter((_, fs) => !fs.isOptional).map((label, _) => label)
    val jsonSchema = JsonSchema(
      "object",
      Some(properties),
      if requiredFields.isEmpty then None else Some(requiredFields)
    )

    instance[A](
      jsonSchema,
      a =>
        val json = e(a)
        ToolResult(
          List(TextContent(json.noSpaces)),
          structuredContent = Some(json)
        )
    )

  /** Schema metadata for a single field, carrying type info, optionality, and array items. */
  final private case class FieldSchema(
      typeName: String,
      isOptional: Boolean,
      items: Option[String]
  )

  // Helper to summon schema info for tuple elements
  private inline def summonSchemas[T <: Tuple]: List[FieldSchema] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => fieldSchemaFor[t] :: summonSchemas[ts]

  // Map Scala types to FieldSchema with type, optionality, and array items info
  private inline def fieldSchemaFor[T]: FieldSchema =
    inline erasedValue[T] match
      case _: Option[t] => FieldSchema(schemaTypeFor[t], true, arrayItemsFor[t])
      case _: List[t]   => FieldSchema("array", false, Some(schemaTypeFor[t]))
      case _: Seq[t]    => FieldSchema("array", false, Some(schemaTypeFor[t]))
      case _            => FieldSchema(schemaTypeFor[T], false, None)

  // Map Scala types to JSON schema type names
  private inline def schemaTypeFor[T]: String =
    inline erasedValue[T] match
      case _: String    => "string"
      case _: Int       => "integer"
      case _: Long      => "integer"
      case _: Double    => "number"
      case _: Float     => "number"
      case _: Boolean   => "boolean"
      case _: List[?]   => "array"
      case _: Seq[?]    => "array"
      case _: Map[?, ?] => "object"
      case _            => "object"

  // Extract array items type for types that are arrays, None otherwise
  private inline def arrayItemsFor[T]: Option[String] =
    inline erasedValue[T] match
      case _: List[t] => Some(schemaTypeFor[t])
      case _: Seq[t]  => Some(schemaTypeFor[t])
      case _          => None
