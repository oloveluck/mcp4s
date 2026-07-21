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

import io.circe.{Decoder, Encoder, Json}
import mcp4s.protocol.{JsonSchema, JsonSchemaProperty}
import scala.compiletime.*
import scala.deriving.Mirror

/** The single source of truth for how a Scala type maps onto the MCP wire format.
  *
  * A `Schema[A]` is a reified description of `A` from which every protocol artifact is derived: the
  * JSON Schema advertised to clients ([[jsonSchema]]), the circe codecs used to move values on the
  * wire ([[encoder]], [[decoder]]), and prompt-argument metadata (see [[PromptCodec]]). Because all
  * of these are interpreters over the same value, they can never disagree with each other.
  *
  * Case classes, Scala 3 enums, and sealed hierarchies derive automatically:
  * {{{
  * enum Unit derives Schema { case Celsius, Fahrenheit }
  *
  * case class Forecast(
  *   @description("City to look up") city: String,
  *   days: Int = 3,
  *   unit: Unit = Unit.Celsius
  * ) derives Schema
  * }}}
  *
  * Nested case classes, collections, `Map[String, V]`, `Option`, defaults, enums and sealed traits
  * with payloads (rendered as `oneOf` with a `"type"` discriminator) are all supported. Nested
  * types do not need their own `derives Schema` clause — derivation recurses automatically.
  */
sealed trait Schema[A]:
  /** Human-readable description, rendered into the JSON schema. */
  def description: Option[String]

  /** Returns a copy of this schema carrying the given description. */
  def withDescription(d: String): Schema[A]

  /** Full JSON Schema rendering (used for tool input/output schemas). */
  final lazy val jsonSchema: JsonSchema = JsonSchemaRenderer.render(this)

  /** Rendering as a property, for embedding inside an enclosing object schema. */
  final lazy val property: JsonSchemaProperty = JsonSchemaRenderer.renderProperty(this)

  /** circe encoder compiled from this schema. */
  final lazy val encoder: Encoder[A] = SchemaCodec.encoder(this)

  /** circe decoder compiled from this schema. */
  final lazy val decoder: Decoder[A] = SchemaCodec.decoder(this)

object Schema:

  def apply[A](using s: Schema[A]): Schema[A] = s

  /** Primitive scalar (or opaque JSON) schema. */
  final case class Primitive[A](tag: PrimitiveTag[A], description: Option[String] = None)
      extends Schema[A]:
    def withDescription(d: String): Schema[A] = copy(description = Some(d))

  /** A single field of a [[Struct]]. */
  final case class Field[S, A](
      label: String,
      schema: Schema[A],
      get: S => A,
      default: Option[A]
  )

  /** Product type (case class): an object with named, typed fields. */
  final case class Struct[A](
      name: String,
      fields: Vector[Field[A, ?]],
      make: Vector[Any] => A,
      description: Option[String] = None
  ) extends Schema[A]:
    def withDescription(d: String): Schema[A] = copy(description = Some(d))

  /** Optional value; renders as a non-required field and omits `None` when encoding. */
  final case class Optional[A](
      underlying: Schema[A],
      description: Option[String] = None
  ) extends Schema[Option[A]]:
    def withDescription(d: String): Schema[Option[A]] = copy(description = Some(d))

  /** Homogeneous collection; renders as a JSON array. */
  final case class Collection[C[_], A](
      item: Schema[A],
      fromList: List[A] => C[A],
      toList: C[A] => List[A],
      description: Option[String] = None
  ) extends Schema[C[A]]:
    def withDescription(d: String): Schema[C[A]] = copy(description = Some(d))

  /** String-keyed map; renders as an object with `additionalProperties`. */
  final case class StringMap[V](
      value: Schema[V],
      description: Option[String] = None
  ) extends Schema[Map[String, V]]:
    def withDescription(d: String): Schema[Map[String, V]] = copy(description = Some(d))

  /** One named value of an [[Enumeration]]. */
  final case class EnumValue[A](label: String, value: A)

  /** Closed set of singleton values (Scala 3 enum / sealed all-object hierarchy); renders as a JSON
    * string enum.
    */
  final case class Enumeration[A](
      name: String,
      values: Vector[EnumValue[A]],
      description: Option[String] = None
  ) extends Schema[A]:
    def withDescription(d: String): Schema[A] = copy(description = Some(d))

  /** One alternative of a [[Union]]. */
  final case class Alt[A, B](label: String, schema: Schema[B])

  /** Sealed hierarchy with payload-carrying cases; renders as `oneOf` with a string discriminator
    * field (default `"type"`) identifying the case.
    */
  final case class Union[A](
      name: String,
      alts: Vector[Alt[A, ?]],
      ordinal: A => Int,
      discriminator: String = "type",
      description: Option[String] = None
  ) extends Schema[A]:
    def withDescription(d: String): Schema[A] = copy(description = Some(d))

  /** Schema for `B` expressed through an isomorphism with `A`. */
  final case class Bijection[A, B](underlying: Schema[A], to: A => B, from: B => A)
      extends Schema[B]:
    def description: Option[String]           = underlying.description
    def withDescription(d: String): Schema[B] = copy(underlying = underlying.withDescription(d))

  /** Deferred schema, breaking cycles in recursive types. Recursive occurrences render as an
    * untyped `object` in JSON Schema (no `$defs` support yet) but encode/decode fully.
    */
  final case class Lazily[A](thunk: () => Schema[A]) extends Schema[A]:
    lazy val underlying: Schema[A]            = thunk()
    def description: Option[String]           = None
    def withDescription(d: String): Schema[A] = Lazily(() => thunk().withDescription(d))

  /** Tags for primitive schemas, carrying their JSON type name and codecs. */
  enum PrimitiveTag[A](val jsonType: String, val encoder: Encoder[A], val decoder: Decoder[A]):
    case PString extends PrimitiveTag[String]("string", Encoder.encodeString, Decoder.decodeString)
    case PInt    extends PrimitiveTag[Int]("integer", Encoder.encodeInt, Decoder.decodeInt)
    case PLong   extends PrimitiveTag[Long]("integer", Encoder.encodeLong, Decoder.decodeLong)
    case PDouble extends PrimitiveTag[Double]("number", Encoder.encodeDouble, Decoder.decodeDouble)
    case PFloat  extends PrimitiveTag[Float]("number", Encoder.encodeFloat, Decoder.decodeFloat)
    case PBoolean
        extends PrimitiveTag[Boolean]("boolean", Encoder.encodeBoolean, Decoder.decodeBoolean)
    case PJson extends PrimitiveTag[Json]("object", Encoder.encodeJson, Decoder.decodeJson)
    case PUnit
        extends PrimitiveTag[Unit](
          "object",
          Encoder.instance(_ => Json.obj()),
          Decoder.const(())
        )

  import PrimitiveTag.*

  // === Given instances ===

  given string: Schema[String]   = Primitive(PString)
  given int: Schema[Int]         = Primitive(PInt)
  given long: Schema[Long]       = Primitive(PLong)
  given double: Schema[Double]   = Primitive(PDouble)
  given float: Schema[Float]     = Primitive(PFloat)
  given boolean: Schema[Boolean] = Primitive(PBoolean)
  given json: Schema[Json]       = Primitive(PJson)
  given unit: Schema[Unit]       = Primitive(PUnit)

  given option[A](using s: Schema[A]): Schema[Option[A]] = Optional(s)

  given list[A](using s: Schema[A]): Schema[List[A]] = Collection(s, identity, identity)

  given vector[A](using s: Schema[A]): Schema[Vector[A]] =
    Collection(s, _.toVector, _.toList)

  given seq[A](using s: Schema[A]): Schema[Seq[A]] = Collection(s, _.toSeq, _.toList)

  given set[A](using s: Schema[A]): Schema[Set[A]] = Collection(s, _.toSet, _.toList)

  given stringMap[V](using v: Schema[V]): Schema[Map[String, V]] = StringMap(v)

  /** Explicit bijection constructor for wrapper types. */
  def bijection[A, B](using s: Schema[A])(to: A => B, from: B => A): Schema[B] =
    Bijection(s, to, from)

  /** Defer a schema, breaking cycles in recursive types. */
  def defer[A](s: => Schema[A]): Schema[A] = Lazily(() => s)

  // === Derivation ===

  inline def derived[A](using m: Mirror.Of[A]): Schema[A] =
    inline m match
      case p: Mirror.ProductOf[A] => derivedProduct[A](using p)
      case s: Mirror.SumOf[A]     => derivedSum[A](using s)

  private inline def derivedProduct[A](using m: Mirror.ProductOf[A]): Schema[A] =
    val labels       = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val schemas      = summonFieldSchemas[m.MirroredElemTypes]
    val descriptions = SchemaMacros.fieldDescriptions[A]
    val defaults     = SchemaMacros.fieldDefaults[A]
    val fields       = labels
      .zip(schemas)
      .zipWithIndex
      .map { case ((label, fieldSchema), idx) =>
        val described = descriptions.get(label).fold(fieldSchema)(fieldSchema.withDescription)
        Field[A, Any](
          label,
          described.asInstanceOf[Schema[Any]],
          a => a.asInstanceOf[Product].productElement(idx),
          defaults.get(label)
        )
      }
      .toVector
    Struct[A](
      SchemaMacros.typeName[A],
      fields,
      values => m.fromProduct(Tuple.fromArray(values.toArray)),
      SchemaMacros.classDescription[A]
    )

  private inline def derivedSum[A](using m: Mirror.SumOf[A]): Schema[A] =
    val labels = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val name   = SchemaMacros.typeName[A]
    val desc   = SchemaMacros.classDescription[A]
    inline if allSingletons[m.MirroredElemTypes] then
      val values = singletonValues[m.MirroredElemTypes]
      Enumeration[A](
        name,
        labels.zip(values).map((l, v) => EnumValue(l, v.asInstanceOf[A])).toVector,
        desc
      )
    else
      val altSchemas = summonFieldSchemas[m.MirroredElemTypes]
      val alts = labels.zip(altSchemas).map((l, s) => Alt[A, Any](l, s.asInstanceOf[Schema[Any]]))
      Union[A](name, alts.toVector, a => m.ordinal(a), description = desc)

  /** Resolve the schema for one field/alternative type: structural rules for collections and Option
    * (so their element types recurse), then an explicit given, then automatic derivation for nested
    * products/sums.
    */
  private inline def schemaOf[T]: Schema[?] =
    inline erasedValue[T] match
      case _: Option[t] => Optional(schemaOf[t].asInstanceOf[Schema[t]])
      case _: List[t]   =>
        Collection[List, t](schemaOf[t].asInstanceOf[Schema[t]], identity, identity)
      case _: Vector[t] =>
        Collection[Vector, t](schemaOf[t].asInstanceOf[Schema[t]], _.toVector, _.toList)
      case _: Set[t] => Collection[Set, t](schemaOf[t].asInstanceOf[Schema[t]], _.toSet, _.toList)
      case _: Seq[t] => Collection[Seq, t](schemaOf[t].asInstanceOf[Schema[t]], _.toSeq, _.toList)
      case _: Map[String, v] => StringMap(schemaOf[v].asInstanceOf[Schema[v]])
      case _                 =>
        summonFrom {
          case s: Schema[T]    => s
          case m: Mirror.Of[T] => derived[T](using m)
          case _               =>
            error(
              "Cannot derive Schema: no given Schema instance and no Mirror for a field type. " +
                "Add `derives Schema` or provide a `given Schema[...]` for the field's type."
            )
        }

  private inline def summonFieldSchemas[T <: Tuple]: List[Schema[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => schemaOf[t] :: summonFieldSchemas[ts]

  private inline def allSingletons[T <: Tuple]: Boolean =
    inline erasedValue[T] match
      case _: EmptyTuple => true
      case _: (t *: ts)  =>
        summonFrom {
          case _: ValueOf[`t`] => allSingletons[ts]
          case _               => false
        }

  private inline def singletonValues[T <: Tuple]: List[Any] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[ValueOf[t]].value :: singletonValues[ts]
