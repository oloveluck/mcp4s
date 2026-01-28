package mcp4s.protocol

import scala.compiletime.*
import scala.deriving.Mirror

/** Typeclass for prompt inputs with automatic PromptArgument derivation.
  *
  * Provides automatic derivation for case classes using `derives PromptInput`.
  * Supports `@description` annotations on fields for argument documentation.
  * Optional fields (Option[A]) are marked as not required.
  *
  * Example:
  * {{{
  * case class GreetArgs(
  *   @description("Name to greet") name: String,
  *   @description("Greeting style") style: Option[String]
  * ) derives PromptInput
  *
  * // Use with McpServerBuilder:
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
  private final class Impl[A](
      val arguments: List[PromptArgument],
      decoder: Map[String, String] => Either[String, A]
  ) extends PromptInput[A]:
    def decode(args: Map[String, String]): Either[String, A] = decoder(args)

  /** Derive PromptInput for a product type (case class) */
  inline given derived[A <: Product](using m: Mirror.ProductOf[A]): PromptInput[A] =
    val labels = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val optionalFlags = summonOptionalFlags[m.MirroredElemTypes]
    val descriptions = ToolInput.fieldDescriptions[A]

    val args = labels.zip(optionalFlags).map { (label, isOptional) =>
      PromptArgument(label, descriptions.get(label), required = !isOptional)
    }

    instance[A](
      args,
      map => decodeProduct[A, m.MirroredElemTypes](map, labels, optionalFlags)(using m)
    )

  /** Determine which fields are optional (Option[?]) */
  private inline def summonOptionalFlags[T <: Tuple]: List[Boolean] =
    inline erasedValue[T] match
      case _: EmptyTuple        => Nil
      case _: (Option[?] *: ts) => true :: summonOptionalFlags[ts]
      case _: (t *: ts)         => false :: summonOptionalFlags[ts]

  /** Decode a map to a product type */
  private inline def decodeProduct[A <: Product, T <: Tuple](
      map: Map[String, String],
      labels: List[String],
      optionalFlags: List[Boolean]
  )(using m: Mirror.ProductOf[A]): Either[String, A] =
    buildTuple[T](map, labels, optionalFlags).map { tuple =>
      m.fromTuple(tuple.asInstanceOf[m.MirroredElemTypes])
    }

  /** Build a tuple from the map values */
  private inline def buildTuple[T <: Tuple](
      map: Map[String, String],
      labels: List[String],
      optionalFlags: List[Boolean]
  ): Either[String, Tuple] =
    inline erasedValue[T] match
      case _: EmptyTuple => Right(EmptyTuple)
      case _: (Option[String] *: ts) =>
        val label = labels.head
        val value = map.get(label)
        buildTuple[ts](map, labels.tail, optionalFlags.tail).map(value *: _)
      case _: (String *: ts) =>
        val label = labels.head
        map.get(label) match
          case Some(value) =>
            buildTuple[ts](map, labels.tail, optionalFlags.tail).map(value *: _)
          case None =>
            Left(s"Missing required argument: $label")
      case _: (t *: ts) =>
        // For non-string types, we still require string input and pass it through
        // This handles edge cases; in practice, prompts use strings
        val label = labels.head
        val isOptional = optionalFlags.head
        if isOptional then
          buildTuple[ts](map, labels.tail, optionalFlags.tail).map(map.get(label) *: _)
        else
          map.get(label) match
            case Some(value) =>
              buildTuple[ts](map, labels.tail, optionalFlags.tail).map(value *: _)
            case None =>
              Left(s"Missing required argument: $label")
