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

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

/** Compiles a [[Schema]] into circe codecs. Because both directions come from the same schema
  * value, the wire format can never diverge from the advertised JSON Schema.
  *
  * Conventions: `None` optional fields are omitted from encoded objects (MCP wire convention);
  * missing fields with a constructor default decode to that default; unions carry a string
  * discriminator field alongside the case's own fields.
  */
private[mcp4s] object SchemaCodec:
  import Schema.*

  def encoder[A](schema: Schema[A]): Encoder[A] =
    schema match
      case Primitive(tag, _) => tag.encoder

      case Struct(_, fields, _, _) =>
        Encoder.instance { a =>
          Json.fromFields(fields.flatMap(encodeField(_, a)))
        }

      case Optional(underlying, _) =>
        Encoder.instance {
          case Some(value) => underlying.encoder(value)
          case None        => Json.Null
        }

      case coll: Collection[c, a] =>
        Encoder.instance { (values: c[a]) =>
          Json.fromValues(coll.toList(values).map(coll.item.encoder(_)))
        }

      case StringMap(value, _) =>
        Encoder.instance { m =>
          Json.fromFields(m.map((k, v) => k -> value.encoder(v)))
        }

      case Enumeration(name, values, _) =>
        Encoder.instance { a =>
          values.find(_.value == a) match
            case Some(ev) => Json.fromString(ev.label)
            case None     => Json.fromString(a.toString)
        }

      case Union(_, alts, ordinal, discriminator, _) =>
        Encoder.instance { a =>
          val alt  = alts(ordinal(a))
          val body = alt.schema.asInstanceOf[Schema[Any]].encoder(a)
          body.asObject match
            case Some(obj) =>
              Json.fromJsonObject(obj.add(discriminator, Json.fromString(alt.label)))
            case None =>
              Json.obj(discriminator -> Json.fromString(alt.label), "value" -> body)
        }

      case Bijection(underlying, _, from) => underlying.encoder.contramap(from)

      case Lazily(thunk) => Encoder.instance(a => thunk().encoder(a))

  def decoder[A](schema: Schema[A]): Decoder[A] =
    schema match
      case Primitive(tag, _) => tag.decoder

      case Struct(name, fields, make, _) =>
        Decoder.instance { cursor =>
          fields
            .foldLeft[Decoder.Result[Vector[Any]]](Right(Vector.empty)) { (acc, field) =>
              acc.flatMap(values => decodeField(cursor, field).map(values :+ _))
            }
            .map(make)
        }

      case Optional(underlying, _) =>
        Decoder.decodeOption(using underlying.decoder)

      case coll: Collection[c, a] =>
        Decoder.decodeList[a](using coll.item.decoder).map(coll.fromList)

      case StringMap(value, _) =>
        Decoder.decodeMap(using io.circe.KeyDecoder.decodeKeyString, value.decoder)

      case Enumeration(name, values, _) =>
        Decoder.decodeString.emap { label =>
          values
            .find(_.label == label)
            .map(_.value)
            .toRight(
              s"'$label' is not a valid $name; expected one of: ${values.map(_.label).mkString(", ")}"
            )
        }

      case Union(name, alts, _, discriminator, _) =>
        Decoder.instance { cursor =>
          cursor.downField(discriminator).as[String].flatMap { label =>
            alts.find(_.label == label) match
              case Some(alt) =>
                alt.schema.decoder.tryDecode(cursor).map(_.asInstanceOf[A])
              case None =>
                Left(
                  DecodingFailure(
                    s"'$label' is not a valid $name case; expected one of: " +
                      alts.map(_.label).mkString(", "),
                    cursor.history
                  )
                )
          }
        }

      case Bijection(underlying, to, _) => underlying.decoder.map(to)

      case Lazily(thunk) => Decoder.instance(cursor => thunk().decoder(cursor))

  private def encodeField[S](field: Field[S, ?], value: S): Option[(String, Json)] =
    val fieldValue = field.get(value)
    field.schema match
      case _: Optional[?] if fieldValue == None => None
      case schema => Some(field.label -> schema.asInstanceOf[Schema[Any]].encoder(fieldValue))

  private def decodeField[S, B](cursor: HCursor, field: Field[S, B]): Decoder.Result[B] =
    val fieldCursor = cursor.downField(field.label)
    if fieldCursor.failed then
      field.default match
        case Some(default) => Right(default)
        case None          => field.schema.decoder.tryDecode(fieldCursor)
    else field.schema.decoder.tryDecode(fieldCursor)
