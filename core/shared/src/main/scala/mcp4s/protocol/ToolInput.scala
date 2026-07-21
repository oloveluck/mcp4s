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

import io.circe.{Decoder, Json}
import mcp4s.schema.{Schema, SchemaMacros}
import scala.deriving.Mirror

/** Typeclass for tool input types that can be decoded from JSON and have a schema.
  *
  * This is a thin view over [[mcp4s.schema.Schema]], which is the single source of truth for
  * derivation. Prefer `derives Schema` on your case classes; `derives ToolInput` continues to work
  * and routes through the same derivation.
  *
  * Example:
  * {{{
  * case class AddArgs(
  *   @description("First number to add") a: Double,
  *   @description("Second number to add") b: Double
  * ) derives Schema
  * }}}
  */
trait ToolInput[A]:
  /** JSON schema for this input type */
  def schema: JsonSchema

  /** Decode JSON to this type */
  def decode(json: Json): Either[String, A]

object ToolInput:

  def apply[A](using ti: ToolInput[A]): ToolInput[A] = ti

  /** Create a ToolInput from an existing Decoder and schema */
  def instance[A](s: JsonSchema, decoder: Decoder[A]): ToolInput[A] =
    new Impl[A](s, decoder)

  /** View a [[Schema]] as a ToolInput. */
  def fromSchema[A](s: Schema[A]): ToolInput[A] =
    instance(s.jsonSchema, s.decoder)

  // Non-inline implementation class to avoid duplication
  final private class Impl[A](val schema: JsonSchema, decoder: Decoder[A]) extends ToolInput[A]:
    def decode(json: Json): Either[String, A] =
      decoder.decodeJson(json).left.map(_.getMessage)

  /** Derive ToolInput for a product type (case class) via [[Schema]] derivation. */
  inline given derived[A <: Product](using m: Mirror.ProductOf[A]): ToolInput[A] =
    fromSchema(Schema.derived[A])

  // === Compile-time metadata (delegates kept for source compatibility) ===

  /** Extract field descriptions from @description annotations at compile time */
  inline def fieldDescriptions[A]: Map[String, String] = SchemaMacros.fieldDescriptions[A]

  /** Extract class-level @description annotation at compile time */
  inline def classDescription[A]: Option[String] = SchemaMacros.classDescription[A]

  /** Get the simple class name at compile time */
  inline def typeName[A]: String = SchemaMacros.typeName[A]

  /** Derive a tool/prompt name from a class name. Strips common suffixes, converts PascalCase to
    * snake_case. Examples: "AddArgs" -> "add", "SmartCalcArgs" -> "smart_calc", "Add" -> "add"
    */
  def deriveName(className: String): String = SchemaMacros.deriveName(className)
