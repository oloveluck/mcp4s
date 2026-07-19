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

package mcp4s.server

import cats.ApplicativeError
import mcp4s.protocol.*

/** Trait for server-to-client elicitation requests.
  *
  * Allows server tool handlers to request user input from connected clients via the
  * elicitation/create method.
  *
  * @example
  *   {{{
  * ctx.elicitation.elicit(ElicitFormParams(
  *   message = "Please confirm your action",
  *   requestedSchema = JsonSchema.obj(
  *     "confirmed" -> JsonSchema.boolean("Confirm action")
  *   )
  * ))
  *   }}}
  */
trait ElicitationRequester[F[_]]:

  /** Check if the connected client supports elicitation */
  def supportsElicitation: Boolean

  /** Request user input from the client.
    *
    * @param params
    *   The elicitation request parameters
    * @return
    *   The user's response
    * @throws mcp4s.protocol.McpError
    *   `McpError.ElicitationNotSupported` if the client doesn't support elicitation
    */
  def elicit(params: ElicitParams): F[ElicitResult]

object ElicitationRequester:

  /** Create a no-op elicitation requester that always fails. Used when no bidirectional transport
    * is available.
    */
  def unsupported[F[_]](using F: ApplicativeError[F, Throwable]): ElicitationRequester[F] =
    new ElicitationRequester[F]:
      def supportsElicitation: Boolean = false
      def elicit(params: ElicitParams): F[ElicitResult] =
        F.raiseError(McpError.ElicitationNotSupported)
