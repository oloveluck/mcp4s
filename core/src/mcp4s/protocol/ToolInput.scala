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
  * The Decoder is automatically derived - you don't need a separate `given Decoder[A]`.
  *
  * Example:
  * {{{
  * case class AddArgs(
  *   @description("First number to add") a: Double,
  *   @description("Second number to add") b: Double
  * ) derives ToolInput
  *
  * // Use with ServerBuilder:
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

  /** Extract class-level @description annotation at compile time */
  inline def classDescription[A]: Option[String] = ${ classDescriptionMacro[A] }

  /** Get the simple class name at compile time */
  inline def typeName[A]: String = ${ typeNameMacro[A] }

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

  private def classDescriptionMacro[A: Type](using Quotes): Expr[Option[String]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[A]
    val desc = tpe.typeSymbol.annotations.collectFirst {
      case term if term.tpe.typeSymbol.fullName == "mcp4s.protocol.description" =>
        term match
          case Apply(_, List(Literal(StringConstant(desc)))) => desc
          case _                                             => null
    }.filter(_ != null)
    desc match
      case Some(d) => '{ Some(${ Expr(d) }) }
      case None    => '{ None }

  private def typeNameMacro[A: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    Expr(TypeRepr.of[A].typeSymbol.name)

  /** Derive a tool/prompt name from a class name.
    * Strips common suffixes, converts PascalCase to snake_case.
    * Examples: "AddArgs" -> "add", "SmartCalcArgs" -> "smart_calc", "Add" -> "add"
    */
  def deriveName(className: String): String =
    val stripped = Seq("Args", "Input", "Params", "Request")
      .foldLeft(className) { (name, suffix) =>
        if name.endsWith(suffix) && name.length > suffix.length
        then name.dropRight(suffix.length)
        else name
      }
    stripped
      .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toLowerCase

  // === Derivation Support ===

  /** Schema metadata for a single field, carrying type info, optionality, and array items. */
  final case class FieldSchema(
      typeName: String,
      isOptional: Boolean,
      items: Option[String]
  )

  /** Derive ToolInput for a product type (case class).
    *
    * This automatically derives the Decoder using Circe's generic derivation,
    * so you don't need a separate `given Decoder[A]`.
    */
  inline given derived[A <: Product](using m: Mirror.ProductOf[A]): ToolInput[A] =
    val labels = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val schemas = summonSchemas[m.MirroredElemTypes]
    val descriptions = fieldDescriptions[A]

    val properties = labels.zip(schemas).map { (label, fs) =>
      val itemsProp = fs.items.map(t => JsonSchemaProperty.make(t))
      label -> JsonSchemaProperty.make(fs.typeName, descriptions.get(label), None, None, None, None, itemsProp)
    }.toMap

    val requiredFields = labels.zip(schemas).filter((_, fs) => !fs.isOptional).map((label, _) => label)
    val jsonSchema = JsonSchema("object", Some(properties), if requiredFields.isEmpty then None else Some(requiredFields))

    // Auto-derive the Decoder using Circe's inline derivation
    val decoder: Decoder[A] = Decoder.derived[A]

    instance(jsonSchema, decoder)

  // Helper to summon schema info for tuple elements
  private inline def summonSchemas[T <: Tuple]: List[FieldSchema] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => fieldSchemaFor[t] :: summonSchemas[ts]

  // Map Scala types to FieldSchema with type, optionality, and array items info
  private inline def fieldSchemaFor[T]: FieldSchema =
    inline erasedValue[T] match
      case _: Option[t]     => FieldSchema(schemaTypeFor[t], true, arrayItemsFor[t])
      case _: List[t]       => FieldSchema("array", false, Some(schemaTypeFor[t]))
      case _: Seq[t]        => FieldSchema("array", false, Some(schemaTypeFor[t]))
      case _                => FieldSchema(schemaTypeFor[T], false, None)

  // Map Scala types to JSON schema type names
  private inline def schemaTypeFor[T]: String =
    inline erasedValue[T] match
      case _: String        => "string"
      case _: Int           => "integer"
      case _: Long          => "integer"
      case _: Double        => "number"
      case _: Float         => "number"
      case _: Boolean       => "boolean"
      case _: List[?]       => "array"
      case _: Seq[?]        => "array"
      case _: Map[?, ?]     => "object"
      case _                => "object"

  // Extract array items type for types that are arrays, None otherwise
  private inline def arrayItemsFor[T]: Option[String] =
    inline erasedValue[T] match
      case _: List[t] => Some(schemaTypeFor[t])
      case _: Seq[t]  => Some(schemaTypeFor[t])
      case _          => None
