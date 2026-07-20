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

package mcp4s.client

import cats.MonadThrow
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.{McpError, TextContent, ToolResult}
import mcp4s.schema.{PromptEndpoint, ToolEndpoint, ToolOutputEncoder}

/** Typed, endpoint-based calls on an [[McpConnection]] — the client half of an
  * [[mcp4s.schema.McpService]].
  *
  * Instead of a stringly-typed tool name and hand-rolled JSON, pass the endpoint definition: the
  * input encodes via its schema and the result decodes via the endpoint's output schema.
  *
  * {{{
  * conn.call(Calculator.add)(AddArgs(1, 2))       // : F[AddResult]
  * conn.call(Calculator.greet)(GreetArgs("Ada"))  // : F[ToolResult] (no .output declared)
  * conn.getPrompt(Prompts.greeting)(GreetArgs("Ada"))
  * }}}
  */
object TypedClient:

  extension [F[_]](conn: McpConnection[F])

    /** Call a tool endpoint with typed input, decoding the typed output.
      *
      * An `isError` result raises [[mcp4s.protocol.McpError.ToolExecutionError]]. For endpoints
      * with a declared `.output[O]`, the result decodes from `structuredContent` (falling back to
      * the first text content parsed as JSON); endpoints without one return the raw
      * [[ToolResult]].
      */
    def call[I, O](endpoint: ToolEndpoint[I, O])(input: I)(using F: MonadThrow[F]): F[O] =
      given io.circe.Encoder[I] = endpoint.inputSchema.encoder
      conn.callTool[I](endpoint.name, input).flatMap { result =>
        if result.isError.getOrElse(false) then
          F.raiseError(McpError.ToolExecutionError(endpoint.name, result.textContent))
        else
          // GADT refinement: Raw witnesses O =:= ToolResult, Structured(schema) binds Schema[O].
          endpoint.outputEncoder match
            case ToolOutputEncoder.Raw =>
              F.pure(result)
            case ToolOutputEncoder.Structured(schema) =>
              decodeStructured(endpoint.name, schema, result)
      }

    /** Get a prompt endpoint with typed input. */
    def getPrompt[I](endpoint: PromptEndpoint[I])(input: I)(using
        F: MonadThrow[F]
    ): F[mcp4s.protocol.GetPromptResult] =
      given io.circe.Encoder[I] = endpoint.inputSchema.encoder
      conn.getPrompt[I](endpoint.name, input)

  private def decodeStructured[F[_], O](
      name: String,
      schema: mcp4s.schema.Schema[O],
      result: ToolResult
  )(using F: MonadThrow[F]): F[O] =
    structuredJson(result) match
      case Some(json) =>
        // Primitive outputs are wrapped as {"result": <value>} on the wire; unwrap when the
        // schema itself is not the object that was advertised.
        val candidate = json.hcursor.downField("result").focus match
          case Some(inner) if !isStruct(schema) => inner
          case _                                => json
        schema.decoder
          .decodeJson(candidate)
          .orElse(schema.decoder.decodeJson(json))
          .leftMap(err =>
            McpError.InternalError(s"Failed to decode result of tool '$name': ${err.getMessage}")
          )
          .liftTo[F]
      case None =>
        F.raiseError(
          McpError.InternalError(
            s"Tool '$name' returned no structuredContent and no parseable text content"
          )
        )

  private def isStruct[O](schema: mcp4s.schema.Schema[O]): Boolean =
    schema match
      case _: mcp4s.schema.Schema.Struct[?] => true
      case _                                => false

  /** Prefer structuredContent; fall back to the first text content parsed as JSON. */
  private def structuredJson(result: ToolResult): Option[Json] =
    result.structuredContent.orElse {
      result.content.collectFirst { case TextContent(text, _, _) =>
        io.circe.parser.parse(text).toOption
      }.flatten
    }
