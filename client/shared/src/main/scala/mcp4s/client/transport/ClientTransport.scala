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

package mcp4s.client.transport

import cats.effect.Resource
import mcp4s.transport.McpChannel

/** A client-side transport: something that can open an [[mcp4s.transport.McpChannel]] to a
  * server.
  *
  * Implementations handle only connection establishment and message framing (process pipes, HTTP
  * requests, WebSocket frames). The shared [[mcp4s.client.ConnectionRunner]] layers the MCP
  * protocol on top: initialize handshake, request correlation and timeouts, progress routing, and
  * dispatch of server-initiated requests (sampling, elicitation, roots).
  */
trait ClientTransport[F[_]]:
  /** Open a channel to the server. Releasing the resource closes the connection. */
  def open: Resource[F, McpChannel[F]]
