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

import scala.compiletime.*
import scala.deriving.Mirror

/** Typeclass for prompt inputs with automatic PromptArgument derivation.
  *
  * Provides automatic derivation for case classes using `derives PromptInput`. Supports
  * `@description` annotations on fields for argument documentation. Optional fields (Option[A]) are
  * marked as not required.
  *
  * Example:
  * {{{
  * case class GreetArgs(
  *   @description("Name to greet") name: String,
  *   @description("Greeting style") style: Option[String]
  * ) derives PromptInput
  *
  * // Use with ServerBuilder:
  * .prompt[GreetArgs]("greet", "Greet someone") { args =>
  *   IO.pure(GetPromptResult(...))
  * }
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

  // Non-inline implementation class to avoid duplication
  final private class Impl[A](
      val arguments: List[PromptArgument],
      decoder: Map[String, String] => Either[String, A]
  ) extends PromptInput[A]:
    def decode(args: Map[String, String]): Either[String, A] = decoder(args)

  /** Derive PromptInput for a product type (case class) */
  inline given derived[A <: Product](using m: Mirror.ProductOf[A]): PromptInput[A] =
    // Safe: MirroredElemLabels is a tuple of string-literal singleton types.
    val labels        = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val optionalFlags = summonOptionalFlags[m.MirroredElemTypes]
    val descriptions  = ToolInput.fieldDescriptions[A]

    val args = labels.zip(optionalFlags).map { (label, isOptional) =>
      PromptArgument(label, descriptions.get(label), required = !isOptional)
    }

    instance[A](args, map => decodeProduct[A, m.MirroredElemTypes](map, labels)(using m))

  /** Determine which fields are optional (Option[?]) */
  private inline def summonOptionalFlags[T <: Tuple]: List[Boolean] =
    inline erasedValue[T] match
      case _: EmptyTuple        => Nil
      case _: (Option[?] *: ts) => true :: summonOptionalFlags[ts]
      case _: (t *: ts)         => false :: summonOptionalFlags[ts]

  /** Decode a map to a product type */
  private inline def decodeProduct[A <: Product, T <: Tuple](
      map: Map[String, String],
      labels: List[String]
  )(using m: Mirror.ProductOf[A]): Either[String, A] =
    buildTuple[T](map, labels).map { tuple =>
      // Safe: T is m.MirroredElemTypes, so the built tuple has exactly that shape.
      m.fromTuple(tuple.asInstanceOf[m.MirroredElemTypes])
    }

  private def unsupportedType(label: String): Left[String, Nothing] =
    Left(s"Field '$label' has unsupported type; prompt arguments must be String or Option[String]")

  private val labelCountMismatch =
    Left("internal derivation error: field/label count mismatch")

  /** Build a tuple from the map values. `labels` is peeled in lockstep with the tuple type `T`
    * (both originate from the same Mirror), so the `Nil` branches are unreachable in practice —
    * they make the recursion total rather than risking a `.head` on an empty list.
    */
  private inline def buildTuple[T <: Tuple](
      map: Map[String, String],
      labels: List[String]
  ): Either[String, Tuple] =
    inline erasedValue[T] match
      case _: EmptyTuple => Right(EmptyTuple)
      case _: (Option[String] *: ts) =>
        labels match
          case label :: rest => buildTuple[ts](map, rest).map(map.get(label) *: _)
          case Nil           => labelCountMismatch
      case _: (String *: ts) =>
        labels match
          case label :: rest =>
            map.get(label) match
              case Some(value) => buildTuple[ts](map, rest).map(value *: _)
              case None        => Left(s"Missing required argument: $label")
          case Nil => labelCountMismatch
      case _: (Option[t] *: ts) =>
        labels match
          case label :: _ => unsupportedType(label)
          case Nil        => labelCountMismatch
      case _: (t *: ts) =>
        labels match
          case label :: _ => unsupportedType(label)
          case Nil        => labelCountMismatch
