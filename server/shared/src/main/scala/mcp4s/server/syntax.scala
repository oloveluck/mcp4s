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

import cats.effect.{Async, LiftIO, Resource}
import fs2.io.net.Network
import org.http4s.server.Server as Http4sServer
import org.typelevel.otel4s.trace.Tracer
import mcp4s.server.transport.{HttpConfig, HttpTransport, StdioTransport}

/** Convenient extension methods for running MCP servers.
  *
  * Import this to get simple one-liner server startup:
  *
  * {{{
  * import mcp4s.server.syntax.*
  *
  * // Run on stdio (for Claude Desktop, etc)
  * server.runStdio
  *
  * // Run on HTTP with defaults (port 3000)
  * server.serveHttp.useForever
  *
  * // Run on HTTP with custom port
  * server.serveHttp(port"8080").useForever
  * }}}
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
