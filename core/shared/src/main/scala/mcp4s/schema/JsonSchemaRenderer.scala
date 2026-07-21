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
import mcp4s.protocol.{Codecs, JsonSchema, JsonSchemaProperty}

import scala.collection.immutable.VectorMap

/** Renders a [[Schema]] to the MCP wire-format JSON Schema AST. */
private[mcp4s] object JsonSchemaRenderer:
  import Schema.*

  /** Top-level rendering, used for tool `inputSchema`/`outputSchema`. */
  def render[A](schema: Schema[A]): JsonSchema =
    schema match
      case Struct(_, fields, _, _) =>
        val required = requiredLabels(fields)
        JsonSchema(
          "object",
          Some(fieldProperties(fields)),
          if required.isEmpty then None else Some(required)
        )
      case Bijection(underlying, _, _) => render(underlying)
      case lz: Lazily[a]               => render(lz.underlying)
      case other                       =>
        // Non-object top-level schemas (bare enums, unions, collections) keep their
        // full shape. MCP tool schemas are objects in practice (see ToolOutput's
        // `result` wrapping), but prompts and future callers may pass anything.
        val p = renderProperty(other)
        JsonSchema(
          p.`type`.getOrElse("object"),
          properties = p.properties,
          required = p.required,
          description = p.description,
          `enum` = p.`enum`,
          oneOf = p.oneOf,
          items = p.items,
          additionalProperties = p.additionalProperties
        )

  /** Property-level rendering, for embedding in an enclosing object schema. */
  def renderProperty[A](schema: Schema[A]): JsonSchemaProperty =
    schema match
      case Primitive(tag, desc) =>
        JsonSchemaProperty(Some(tag.jsonType), desc)

      case Struct(_, fields, _, desc) =>
        val required = requiredLabels(fields)
        JsonSchemaProperty(
          Some("object"),
          desc,
          properties = Some(fieldProperties(fields)),
          required = if required.isEmpty then None else Some(required)
        )

      case Optional(underlying, desc) =>
        val rendered = renderProperty(underlying)
        desc.fold(rendered)(d => rendered.copy(description = Some(d)))

      case coll: Collection[?, ?] =>
        JsonSchemaProperty(
          Some("array"),
          coll.description,
          items = Some(renderProperty(coll.item))
        )

      case StringMap(value, desc) =>
        JsonSchemaProperty(
          Some("object"),
          desc,
          additionalProperties = Some(renderProperty(value))
        )

      case Enumeration(_, values, desc) =>
        JsonSchemaProperty(
          Some("string"),
          desc,
          `enum` = Some(values.map(_.label).toList)
        )

      case Union(_, alts, _, discriminator, desc) =>
        JsonSchemaProperty(
          None,
          desc,
          oneOf = Some(alts.map(alt => altJson(alt, discriminator)).toList)
        )

      case Bijection(underlying, _, _) => renderProperty(underlying)

      // Recursive reference: cut off with an untyped object (no $defs support yet).
      case Lazily(_) => JsonSchemaProperty(Some("object"))

  // VectorMap keeps field declaration order, so generated schemas are deterministic
  // (a plain HashMap reorders properties once a struct has >4 fields).
  private def fieldProperties[A](fields: Vector[Field[A, ?]]): Map[String, JsonSchemaProperty] =
    fields
      .map { field =>
        val base        = renderProperty(field.schema)
        val withDefault = field.default match
          case Some(d) => base.copy(default = Some(encodeDefault(field, d)))
          case None    => base
        field.label -> withDefault
      }
      .to(VectorMap)

  private def requiredLabels[A](fields: Vector[Field[A, ?]]): List[String] =
    fields.collect {
      case f if !isOptionalField(f) => f.label
    }.toList

  /** A field is not required when it is Option-typed or has a constructor default. */
  private def isOptionalField[A](field: Field[A, ?]): Boolean =
    field.default.isDefined || isOptionalSchema(field.schema)

  private def isOptionalSchema(schema: Schema[?]): Boolean =
    schema match
      case _: Optional[?]              => true
      case Bijection(underlying, _, _) => isOptionalSchema(underlying)
      case lz: Lazily[?]               => isOptionalSchema(lz.underlying)
      case _                           => false

  private def encodeDefault[A, B](field: Field[A, B], default: B): Json =
    field.schema.encoder(default)

  /** Render one union alternative as a full JSON-schema object with the discriminator constrained
    * to the alternative's label.
    */
  private def altJson[A, B](alt: Alt[A, B], discriminator: String): Json =
    import Codecs.given
    val rendered          = renderProperty(alt.schema)
    val withDiscriminator = rendered.copy(
      properties = Some(
        rendered.properties.getOrElse(Map.empty) +
          (discriminator -> JsonSchemaProperty(
            Some("string"),
            `enum` = Some(List(alt.label))
          ))
      ),
      required = Some(discriminator :: rendered.required.getOrElse(Nil))
    )
    withDiscriminator.asJson.dropNullValues
