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

import mcp4s.schema.{PromptCodec, Schema}
import scala.deriving.Mirror

/** Typeclass for prompt inputs with automatic PromptArgument derivation.
  *
  * This is a thin view over [[mcp4s.schema.Schema]]. Prompt arguments arrive as strings on the
  * wire, so each field parses from its string form according to its schema: strings pass through,
  * numbers/booleans/enums parse from literals, `Option[A]` fields are not required, and structured
  * fields (lists, nested objects) parse as JSON strings.
  *
  * Example:
  * {{{
  * case class GreetArgs(
  *   @description("Name to greet") name: String,
  *   @description("Greeting style") style: Option[String]
  * ) derives Schema
  * }}}
  */
trait PromptInput[A]:
  /** List of prompt arguments derived from the type */
  def arguments: List[PromptArgument]

  /** Decode a map of string arguments to this type */
  def decode(args: Map[String, String]): Either[String, A]

object PromptInput:

  def apply[A](using pi: PromptInput[A]): PromptInput[A] = pi

  /** Create a PromptInput from existing arguments and decoder */
  def instance[A](
      args: List[PromptArgument],
      decoder: Map[String, String] => Either[String, A]
  ): PromptInput[A] =
    new Impl[A](args, decoder)

  /** View a [[Schema]] as a PromptInput. */
  def fromSchema[A](s: Schema[A]): PromptInput[A] =
    instance(PromptCodec.arguments(s), args => PromptCodec.decode(s, args))

  // Non-inline implementation class to avoid duplication
  final private class Impl[A](
      val arguments: List[PromptArgument],
      decoder: Map[String, String] => Either[String, A]
  ) extends PromptInput[A]:
    def decode(args: Map[String, String]): Either[String, A] = decoder(args)

  /** Derive PromptInput for a product type (case class) via [[Schema]] derivation. */
  inline given derived[A <: Product](using m: Mirror.ProductOf[A]): PromptInput[A] =
    fromSchema(Schema.derived[A])
