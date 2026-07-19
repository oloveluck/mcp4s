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

import com.comcast.ip4s.Port
import cats.effect.{Async, LiftIO, Resource}
import fs2.io.net.Network
import org.http4s.server.Server as Http4sServer
import org.typelevel.otel4s.trace.Tracer
import mcp4s.server.transport.{
  HttpConfig,
  HttpTransport,
  StdioTransport,
  WebSocketConfig,
  WebSocketTransport
}

/** Convenient extension methods for selecting a transport and running MCP servers.
  *
  * Import this for one-liner server startup — one method per transport:
  *
  * {{{
  * import mcp4s.server.syntax.*
  *
  * // Stdio (for Claude Desktop, etc) — blocks until stdin closes
  * server.runStdio
  *
  * // HTTP, defaults (port 3000, path /mcp)
  * server.serveHttp().useForever
  * server.serveHttp(port"8080").useForever
  * server.serveHttp(HttpConfig(port = port"8080", path = "api")).useForever
  *
  * // WebSocket, defaults (port 3000, path /ws)
  * server.serveWebSocket().useForever
  * server.serveWebSocket(port"3001").useForever
  * }}}
  *
  * For custom http4s routes/middleware (CORS, auth, embedding in an existing app), drop down to
  * `HttpTransport` / `WebSocketTransport` directly.
  */
object syntax:

  extension [F[_]](server: Server[F])

    /** Run the server on stdio transport.
      *
      * Reads JSON-RPC messages from stdin and writes responses to stdout. Use this for local tool
      * servers communicating with Claude Desktop.
      *
      * {{{
      * object MyServer extends IOApp.Simple:
      *   def run = myServer.runStdio
      * }}}
      */
    def runStdio(using Async[F], LiftIO[F])(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): F[Unit] =
      StdioTransport.run[F](server)

    /** Serve the server over HTTP.
      *
      * Defaults: host 0.0.0.0, port 3000, path /mcp, sessions enabled. Tracing defaults to noop;
      * bring a `given Tracer[F]` into scope to enable distributed tracing.
      *
      * {{{
      * server.serveHttp().useForever
      * server.serveHttp(HttpConfig(port = port"8080", path = "api")).useForever
      * }}}
      */
    def serveHttp(config: HttpConfig[F] = HttpConfig[F]())(using Async[F], Network[F])(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): Resource[F, Http4sServer] =
      HttpTransport.serve[F](server, config)

    /** Serve the server over HTTP on the given port (other settings default).
      *
      * For tracing, use the `HttpConfig` overload with a `given Tracer[F]` in scope.
      *
      * {{{
      * server.serveHttp(port"8080").useForever
      * }}}
      */
    def serveHttp(port: Port)(using Async[F], Network[F]): Resource[F, Http4sServer] =
      serveHttp(HttpConfig[F](port = port))

    /** Serve the server over WebSocket.
      *
      * Defaults: host 0.0.0.0, port 3000, path /ws. Tracing defaults to noop; bring a `given
      * Tracer[F]` into scope to enable distributed tracing.
      *
      * {{{
      * server.serveWebSocket().useForever
      * server.serveWebSocket(WebSocketConfig(port = port"3001", path = "socket")).useForever
      * }}}
      */
    def serveWebSocket(config: WebSocketConfig = WebSocketConfig())(using Async[F], Network[F])(
        using tracer: Tracer[F] = Tracer.noop[F]
    ): Resource[F, Http4sServer] =
      WebSocketTransport.serve[F](server, config)

    /** Serve the server over WebSocket on the given port (other settings default).
      *
      * For tracing, use the `WebSocketConfig` overload with a `given Tracer[F]` in scope.
      *
      * {{{
      * server.serveWebSocket(port"3001").useForever
      * }}}
      */
    def serveWebSocket(port: Port)(using Async[F], Network[F]): Resource[F, Http4sServer] =
      serveWebSocket(WebSocketConfig(port = port))
