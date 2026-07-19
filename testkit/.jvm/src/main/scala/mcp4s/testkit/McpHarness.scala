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

package mcp4s.testkit

import cats.effect.{IO, Resource}
import com.comcast.ip4s.port
import mcp4s.client.McpConnection
import mcp4s.client.syntax.*
import mcp4s.server.Server
import mcp4s.server.syntax.*

/** Transport a compliance/performance run exercises a server over. */
enum McpTransport:
  case Http, WebSocket

  override def toString: String = this match
    case Http      => "http"
    case WebSocket => "ws"

/** A running server endpoint that can hand out client connections on demand. */
final case class McpEndpoint(connect: Resource[IO, McpConnection[IO]])

/** Starts a [[mcp4s.server.Server]] over a chosen transport on an ephemeral port and exposes a
  * `connect` resource for opening client connections to it. The reusable glue behind both the
  * compliance and performance suites.
  */
object McpHarness:

  /** Serve `server` over `transport` on an ephemeral port; the yielded [[McpEndpoint]] connects
    * clients to that running server. Tracing is disabled (noop).
    */
  def serve(server: Server[IO], transport: McpTransport): Resource[IO, McpEndpoint] =
    transport match
      case McpTransport.Http =>
        server
          .serveHttp(port"0")
          .map: http =>
            val url = s"http://localhost:${http.address.getPort}"
            McpEndpoint(DeterministicClients.simple[IO].connectHttp(url))
      case McpTransport.WebSocket =>
        server
          .serveWebSocket(port"0")
          .map: ws =>
            val url = s"ws://localhost:${ws.address.getPort}"
            McpEndpoint(DeterministicClients.simple[IO].connectWebSocket(url))
