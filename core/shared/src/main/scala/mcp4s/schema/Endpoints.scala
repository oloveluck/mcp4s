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

import mcp4s.protocol.{
  JsonSchema,
  PromptArgument,
  ToolAnnotations,
  ToolOutput,
  ToolResult
}

/** How a tool handler's return value becomes a [[mcp4s.protocol.ToolResult]]. */
sealed trait ToolOutputEncoder[O]:
  /** The advertised `outputSchema`, if this tool has structured output. */
  def outputSchema: Option[JsonSchema]

  /** Encode a handler result to the wire ToolResult. */
  def encode(o: O): ToolResult

object ToolOutputEncoder:
  /** The handler returns a [[ToolResult]] directly; no `outputSchema` is advertised. */
  case object Raw extends ToolOutputEncoder[ToolResult]:
    def outputSchema: Option[JsonSchema] = None
    def encode(o: ToolResult): ToolResult = o

  /** The handler returns a typed value encoded as `structuredContent` via its [[Schema]]. */
  final case class Structured[O](schema: Schema[O]) extends ToolOutputEncoder[O]:
    private val view                      = ToolOutput.fromSchema(schema)
    def outputSchema: Option[JsonSchema]  = Some(view.schema)
    def encode(o: O): ToolResult          = view.encode(o)

/** A tool definition: name, documentation, and typed input/output schemas — with no handler
  * attached yet.
  *
  * A `ToolEndpoint[I, O]` is the shared currency between server and client: on the server, attach
  * a handler with `.handle` / `.handleWith` / `.stream` / `.streamWith` (from
  * `mcp4s.server.dsl`); on the client, call it in a typed way with `connection.call(endpoint)`.
  *
  * {{{
  * case class SearchIn(query: String, limit: Int = 10) derives Schema
  * case class SearchOut(hits: List[String]) derives Schema
  *
  * val search = Tool("search")
  *   .withDescription("Search the index")
  *   .input[SearchIn]
  *   .output[SearchOut]
  * }}}
  */
final case class ToolEndpoint[I, O](
    name: String,
    description: Option[String],
    inputSchema: Schema[I],
    outputEncoder: ToolOutputEncoder[O],
    annotations: Option[ToolAnnotations]
):
  def withName(n: String): ToolEndpoint[I, O]              = copy(name = n)
  def withDescription(d: String): ToolEndpoint[I, O]       = copy(description = Some(d))
  def withAnnotations(a: ToolAnnotations): ToolEndpoint[I, O] = copy(annotations = Some(a))

  /** The wire representation advertised in `tools/list`. */
  def toTool: mcp4s.protocol.Tool =
    mcp4s.protocol.Tool(
      name = name,
      description = description,
      inputSchema = inputSchema.jsonSchema,
      outputSchema = outputEncoder.outputSchema,
      annotations = annotations
    )

object ToolEndpoint:
  extension [I, O](e: ToolEndpoint[I, O])
    /** Set the typed input. If no description was set, the input type's class-level
      * `@description` is used.
      */
    inline def input[I2](using s: Schema[I2]): ToolEndpoint[I2, O] =
      ToolEndpoint(
        e.name,
        e.description.orElse(SchemaMacros.classDescription[I2]),
        s,
        e.outputEncoder,
        e.annotations
      )

    /** Set a typed output, advertised as `outputSchema` and encoded as `structuredContent`. */
    def output[O2](using s: Schema[O2]): ToolEndpoint[I, O2] =
      ToolEndpoint(e.name, e.description, e.inputSchema, ToolOutputEncoder.Structured(s), e.annotations)

/** Constructors for [[ToolEndpoint]]. */
object Tool:

  /** A tool with the given name and no input (yet). Add typed input with `.input[I]`. */
  def apply(name: String): ToolEndpoint[Unit, ToolResult] =
    ToolEndpoint(name, None, Schema.unit, ToolOutputEncoder.Raw, None)

  /** A tool whose name and description derive from the input type: the class name converts to
    * snake_case (with common suffixes like `Args`/`Input` stripped) and the class-level
    * `@description` annotation becomes the description.
    *
    * {{{
    * @description("Add two numbers")
    * case class AddArgs(a: Double, b: Double) derives Schema
    *
    * val add = Tool.from[AddArgs]   // name = "add", description = "Add two numbers"
    * }}}
    */
  inline def from[I](using s: Schema[I]): ToolEndpoint[I, ToolResult] =
    ToolEndpoint(
      SchemaMacros.deriveName(SchemaMacros.typeName[I]),
      SchemaMacros.classDescription[I],
      s,
      ToolOutputEncoder.Raw,
      None
    )

/** A prompt definition: name, documentation, and typed input — with no handler attached yet. */
final case class PromptEndpoint[I](
    name: String,
    description: Option[String],
    inputSchema: Schema[I]
):
  def withName(n: String): PromptEndpoint[I]        = copy(name = n)
  def withDescription(d: String): PromptEndpoint[I] = copy(description = Some(d))

  /** Prompt-argument metadata derived from the input schema. */
  def arguments: List[PromptArgument] = PromptCodec.arguments(inputSchema)

  /** The wire representation advertised in `prompts/list`. */
  def toPrompt: mcp4s.protocol.Prompt =
    mcp4s.protocol.Prompt(name, description, arguments)

object PromptEndpoint:
  extension [I](e: PromptEndpoint[I])
    /** Set the typed input. If no description was set, the input type's class-level
      * `@description` is used.
      */
    inline def input[I2](using s: Schema[I2]): PromptEndpoint[I2] =
      PromptEndpoint(e.name, e.description.orElse(SchemaMacros.classDescription[I2]), s)

/** Constructors for [[PromptEndpoint]]. */
object Prompt:

  /** A prompt with the given name and no input (yet). Add typed input with `.input[I]`. */
  def apply(name: String): PromptEndpoint[Unit] =
    PromptEndpoint(name, None, Schema.unit)

  /** A prompt whose name and description derive from the input type (like [[Tool.from]]). */
  inline def from[I](using s: Schema[I]): PromptEndpoint[I] =
    PromptEndpoint(
      SchemaMacros.deriveName(SchemaMacros.typeName[I]),
      SchemaMacros.classDescription[I],
      s
    )
