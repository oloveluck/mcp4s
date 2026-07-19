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

import mcp4s.protocol.PromptArgument

/** Interprets a [[Schema]] as MCP prompt arguments.
  *
  * Prompt arguments arrive as `Map[String, String]` on the wire, so each field is parsed from its
  * string form according to its schema: strings pass through, primitives and enums parse from
  * their literal form, and anything structured (lists, nested objects) parses as a JSON string.
  */
private[mcp4s] object PromptCodec:
  import Schema.*

  private def structOf(schema: Schema[?]): Option[Struct[?]] =
    schema match
      case s: Struct[?]                => Some(s)
      case Bijection(underlying, _, _) => structOf(underlying)
      case lz: Lazily[?]               => structOf(lz.underlying)
      case _                           => None

  private def isOptional(schema: Schema[?]): Boolean =
    schema match
      case _: Optional[?]              => true
      case Bijection(underlying, _, _) => isOptional(underlying)
      case lz: Lazily[?]               => isOptional(lz.underlying)
      case _                           => false

  /** Prompt-argument metadata for a schema. Non-struct schemas expose no arguments. */
  def arguments[A](schema: Schema[A]): List[PromptArgument] =
    structOf(schema) match
      case Some(struct) =>
        struct.fields.map { field =>
          PromptArgument(
            field.label,
            field.schema.description,
            required = field.default.isEmpty && !isOptional(field.schema)
          )
        }.toList
      case None => Nil

  /** Decode prompt arguments (a string map) into `A`. */
  def decode[A](schema: Schema[A], args: Map[String, String]): Either[String, A] =
    schema match
      case struct: Struct[A] =>
        struct.fields
          .foldLeft[Either[String, Vector[Any]]](Right(Vector.empty)) { (acc, field) =>
            acc.flatMap(values => decodeArgument(field, args.get(field.label)).map(values :+ _))
          }
          .map(struct.make)
      case Bijection(underlying, to, _) => decode(underlying, args).map(to)
      case lz: Lazily[A]                => decode(lz.underlying, args)
      case Primitive(PrimitiveTag.PUnit, _) => Right(().asInstanceOf[A])
      case _ =>
        Left(s"Prompt input schemas must be case classes; got ${schema.getClass.getSimpleName}")

  private def decodeArgument[S, B](
      field: Field[S, B],
      raw: Option[String]
  ): Either[String, B] =
    raw match
      case Some(value) => parseString(field.schema, value)
      case None =>
        field.default match
          case Some(default) => Right(default)
          case None =>
            field.schema match
              case _: Optional[?] => Right(None.asInstanceOf[B])
              case _              => Left(s"Missing required argument: ${field.label}")

  /** Parse a single string argument according to its schema. */
  private def parseString[B](schema: Schema[B], value: String): Either[String, B] =
    schema match
      case Primitive(PrimitiveTag.PString, _) => Right(value.asInstanceOf[B])
      case Primitive(tag, _)                  => parseJson(schema, value, tag.jsonType)
      case Optional(underlying, _) =>
        parseString(underlying, value).map(v => Some(v).asInstanceOf[B])
      case Enumeration(name, values, _) =>
        values
          .find(_.label == value)
          .map(_.value)
          .toRight(
            s"'$value' is not a valid $name; expected one of: ${values.map(_.label).mkString(", ")}"
          )
      case Bijection(underlying, to, _) => parseString(underlying, value).map(to)
      case lz: Lazily[B]                => parseString(lz.underlying, value)
      // Structured values (lists, maps, nested objects, unions) parse from a JSON string.
      case other => parseJson(other, value, "JSON")

  private def parseJson[B](schema: Schema[B], value: String, expected: String): Either[String, B] =
    io.circe.parser
      .parse(value)
      .left
      .map(err => s"Expected $expected but could not parse '$value': ${err.message}")
      .flatMap(json => schema.decoder.decodeJson(json).left.map(_.getMessage))
