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

/** Trait for server-to-client sampling requests.
  *
  * Allows server tool handlers to request LLM completions from connected clients via the
  * sampling/createMessage method.
  *
  * @example
  *   {{{
  * ctx.sampling.createMessage(CreateMessageParams(
  *   messages = List(SamplingMessage(Role.User, SamplingTextContent("What is 2+2?"))),
  *   maxTokens = 100
  * ))
  *   }}}
  */
trait SamplingRequester[F[_]]:

  /** Check if the connected client supports sampling */
  def supportsSampling: Boolean

  /** Request an LLM completion from the client.
    *
    * @param params
    *   The sampling request parameters
    * @return
    *   The LLM response from the client
    * @throws mcp4s.protocol.McpError
    *   `McpError.SamplingNotSupported` if the client doesn't support sampling
    */
  def createMessage(params: CreateMessageParams): F[CreateMessageResult]

object SamplingRequester:

  /** Create a no-op sampling requester that always fails. Used when no bidirectional transport is
    * available.
    */
  def unsupported[F[_]](using F: ApplicativeError[F, Throwable]): SamplingRequester[F] =
    new SamplingRequester[F]:
      def supportsSampling: Boolean                                          = false
      def createMessage(params: CreateMessageParams): F[CreateMessageResult] =
        F.raiseError(McpError.SamplingNotSupported)
