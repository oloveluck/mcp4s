package mcp4s.protocol

import io.circe.{Encoder, Json}
import scala.compiletime.*
import scala.deriving.Mirror

/** Typeclass for tool output types that can be encoded to ToolResult and have a schema.
  *
  * Enables typed tool outputs with automatic `outputSchema` generation and
  * `structuredContent` serialization per MCP spec (2025-03-26).
  *
  * Example:
  * {{{
  * case class CalcResult(result: Double, operation: String) derives ToolOutput
  *
  * // Used with McpTool:
  * McpTool.typed[IO, (Double, Double), CalcResult]("add", "Add two numbers")(
  *   number("a") *: number("b")
  * ) { case (a, b) => IO.pure(CalcResult(a + b, "add")) }
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
      def schema: JsonSchema = s
      def encode(a: A): ToolResult = enc(a)

  // === Primitive instances ===

  given ToolOutput[String] with
    def schema: JsonSchema = JsonSchema("object",
      Some(Map("result" -> JsonSchemaProperty.make("string"))),
      Some(List("result"))
    )
    def encode(a: String): ToolResult =
      ToolResult(
        List(TextContent(a)),
        structuredContent = Some(Json.obj("result" -> Json.fromString(a)))
      )

  given ToolOutput[Double] with
    def schema: JsonSchema = JsonSchema("object",
      Some(Map("result" -> JsonSchemaProperty.make("number"))),
      Some(List("result"))
    )
    def encode(a: Double): ToolResult =
      ToolResult(
        List(TextContent(a.toString)),
        structuredContent = Some(Json.obj("result" -> Json.fromDoubleOrNull(a)))
      )

  given ToolOutput[Int] with
    def schema: JsonSchema = JsonSchema("object",
      Some(Map("result" -> JsonSchemaProperty.make("integer"))),
      Some(List("result"))
    )
    def encode(a: Int): ToolResult =
      ToolResult(
        List(TextContent(a.toString)),
        structuredContent = Some(Json.obj("result" -> Json.fromInt(a)))
      )

  given ToolOutput[Long] with
    def schema: JsonSchema = JsonSchema("object",
      Some(Map("result" -> JsonSchemaProperty.make("integer"))),
      Some(List("result"))
    )
    def encode(a: Long): ToolResult =
      ToolResult(
        List(TextContent(a.toString)),
        structuredContent = Some(Json.obj("result" -> Json.fromLong(a)))
      )

  given ToolOutput[Boolean] with
    def schema: JsonSchema = JsonSchema("object",
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
    val labels = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val schemas = summonSchemas[m.MirroredElemTypes]
    val descriptions = ToolInput.fieldDescriptions[A]

    val properties = labels.zip(schemas).map { (label, schemaType) =>
      label -> JsonSchemaProperty.make(schemaType, descriptions.get(label), None)
    }.toMap

    val jsonSchema = JsonSchema("object", Some(properties), Some(labels))

    instance[A](
      jsonSchema,
      a =>
        val json = e(a)
        ToolResult(
          List(TextContent(json.noSpaces)),
          structuredContent = Some(json)
        )
    )

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
      case _: Option[?]     => "string"
      case _: List[?]       => "array"
      case _: Seq[?]        => "array"
      case _: Map[?, ?]     => "object"
      case _                => "object"
