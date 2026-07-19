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

package mcp4s.benchmarks

import java.util.concurrent.TimeUnit

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*
import org.openjdk.jmh.annotations.*
import mcp4s.schema.Schema

/** Decode/encode throughput of the Schema-compiled codecs versus circe's generic derivation.
  *
  * The Schema codec is built once (interpreters are cached in lazy vals) and the hot path is a
  * plain circe Decoder/Encoder, so the target is parity with `Decoder.derived`.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(1)
class SchemaDecodeBench:

  import SchemaDecodeBench.*

  private val payload: Json = Json.obj(
    "query"   -> "select * from users".asJson,
    "limit"   -> 25.asJson,
    "verbose" -> true.asJson,
    "tags"    -> List("a", "b", "c").asJson,
    "filters" -> Json.obj(
      "field" -> "age".asJson,
      "op"    -> "gt".asJson,
      "value" -> 21.5.asJson
    )
  )

  private val value: SearchArgs =
    schemaDecoder.decodeJson(payload).fold(throw _, identity)

  @Benchmark
  def decodeViaSchema: Either[io.circe.DecodingFailure, SearchArgs] =
    schemaDecoder.decodeJson(payload)

  @Benchmark
  def decodeViaGenericDerivation: Either[io.circe.DecodingFailure, SearchArgs] =
    genericDecoder.decodeJson(payload)

  @Benchmark
  def encodeViaSchema: Json =
    schemaEncoder(value)

  @Benchmark
  def encodeViaGenericDerivation: Json =
    genericEncoder(value)

object SchemaDecodeBench:

  final case class Filter(field: String, op: String, value: Double)
  object Filter:
    given Schema[Filter] = Schema.derived

  final case class SearchArgs(
      query: String,
      limit: Int,
      verbose: Boolean,
      tags: List[String],
      filters: Filter
  )

  val schema: Schema[SearchArgs]        = Schema.derived
  val schemaDecoder: Decoder[SearchArgs] = schema.decoder
  val schemaEncoder: Encoder[SearchArgs] = schema.encoder

  given Decoder[Filter] = Decoder.derived
  given Encoder[Filter] = Encoder.AsObject.derived

  val genericDecoder: Decoder[SearchArgs] = Decoder.derived
  val genericEncoder: Encoder[SearchArgs] = Encoder.AsObject.derived
