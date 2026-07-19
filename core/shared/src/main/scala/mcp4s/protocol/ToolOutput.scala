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

import io.circe.Json
import mcp4s.schema.Schema
import scala.deriving.Mirror

/** Typeclass for tool output types that can be encoded to ToolResult and have a schema.
  *
  * Enables typed tool outputs with automatic `outputSchema` generation and `structuredContent`
  * serialization per MCP spec (2025-11-25).
  *
  * This is a thin view over [[mcp4s.schema.Schema]]. Prefer `derives Schema` on your case classes;
  * `derives ToolOutput` continues to work and routes through the same derivation (no separate
  * `Encoder` instance is needed).
  *
  * Example:
  * {{{
  * case class CalcResult(result: Double, operation: String) derives Schema
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

  /** View a [[Schema]] as a ToolOutput.
    *
    * MCP `outputSchema` must be a JSON object, so non-struct schemas (primitives, collections) are
    * wrapped as `{"result": <value>}`.
    */
  def fromSchema[A](s: Schema[A]): ToolOutput[A] =
    s match
      case struct: Schema.Struct[A] =>
        instance(
          struct.jsonSchema,
          a =>
            val json = struct.encoder(a)
            ToolResult(List(TextContent(json.noSpaces)), structuredContent = Some(json))
        )
      case other => resultWrapped(other)

  private def resultWrapped[A](s: Schema[A]): ToolOutput[A] =
    val wrappedSchema = JsonSchema(
      "object",
      Some(Map("result" -> s.property)),
      Some(List("result"))
    )
    instance(
      wrappedSchema,
      a =>
        val json = s.encoder(a)
        ToolResult(
          List(TextContent(json.asString.getOrElse(json.noSpaces))),
          structuredContent = Some(Json.obj("result" -> json))
        )
    )

  // === Primitive instances ===

  given ToolOutput[String]  = fromSchema(Schema.string)
  given ToolOutput[Double]  = fromSchema(Schema.double)
  given ToolOutput[Int]     = fromSchema(Schema.int)
  given ToolOutput[Long]    = fromSchema(Schema.long)
  given ToolOutput[Boolean] = fromSchema(Schema.boolean)

  given ToolOutput[Json] with
    def schema: JsonSchema = JsonSchema("object")
    def encode(a: Json): ToolResult =
      ToolResult(
        List(TextContent(a.noSpaces)),
        structuredContent = Some(a)
      )

  /** Derive ToolOutput for a product type (case class) via [[Schema]] derivation. */
  inline given derived[A <: Product](using m: Mirror.ProductOf[A]): ToolOutput[A] =
    fromSchema(Schema.derived[A])
