package mcp4s.protocol

import io.circe.{Decoder, Json}
import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.*

/** Typeclass for tool input types that can be decoded from JSON and have a schema.
  *
  * Provides automatic derivation for case classes using `derives ToolInput`.
  * Supports `@description` annotations on fields for JSON schema documentation.
  *
  * Example:
  * {{{
  * case class AddArgs(
  *   @description("First number to add") a: Double,
  *   @description("Second number to add") b: Double
  * ) derives ToolInput
  *
  * // Use with McpServerBuilder:
  * .tool[AddArgs]("add", "Add two numbers") { args =>
  *   IO.pure(ToolResult.text(s"${args.a + args.b}"))
  * }
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

  // Non-inline implementation class to avoid duplication
  private final class Impl[A](val schema: JsonSchema, decoder: Decoder[A]) extends ToolInput[A]:
    def decode(json: Json): Either[String, A] =
      decoder.decodeJson(json).left.map(_.getMessage)

  // === Macro for extracting @description annotations ===

  /** Extract field descriptions from @description annotations at compile time */
  inline def fieldDescriptions[A]: Map[String, String] = ${ fieldDescriptionsMacro[A] }

  private def fieldDescriptionsMacro[A: Type](using Quotes): Expr[Map[String, String]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[A]
    val fields = tpe.typeSymbol.primaryConstructor.paramSymss.flatten
    val descriptions = fields.flatMap { field =>
      field.annotations.collectFirst {
        case term if term.tpe.typeSymbol.fullName == "mcp4s.protocol.description" =>
          term match
            case Apply(_, List(Literal(StringConstant(desc)))) =>
              field.name -> desc
            case _ => null
      }.filter(_ != null)
    }
    Expr(descriptions.toMap)

  // === Derivation Support ===

  /** Derive ToolInput for a product type (case class) */
  inline given derived[A <: Product](using m: Mirror.ProductOf[A], d: Decoder[A]): ToolInput[A] =
    val labels = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val schemas = summonSchemas[m.MirroredElemTypes]
    val descriptions = fieldDescriptions[A]

    val properties = labels.zip(schemas).map { (label, schemaType) =>
      label -> JsonSchemaProperty(schemaType, descriptions.get(label), None)
    }.toMap

    val jsonSchema = JsonSchema("object", Some(properties), Some(labels))

    instance(jsonSchema, d)

  // Helper to summon schema type strings for tuple elements
  private inline def summonSchemas[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => schemaTypeFor[t] :: summonSchemas[ts]

  // Map Scala types to JSON schema types
  private inline def schemaTypeFor[T]: String =
    inline erasedValue[T] match
      case _: String        => "string"
      case _: Int           => "integer"
      case _: Long          => "integer"
      case _: Double        => "number"
      case _: Float         => "number"
      case _: Boolean       => "boolean"
      case _: Option[?]     => "string" // Optional fields - simplified
      case _: List[?]       => "array"
      case _: Seq[?]        => "array"
      case _: Map[?, ?]     => "object"
      case _                => "object"
