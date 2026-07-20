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

import cats.effect.{Async, Concurrent, LiftIO, Resource}
import fs2.io.net.Network
import org.http4s.HttpRoutes
import org.http4s.server.Server as Http4sServer
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.ServerInfo
import mcp4s.server.transport.{
  HttpConfig,
  HttpTransport,
  StdioTransport,
  WebSocketConfig,
  WebSocketTransport
}

/** The one entry point for assembling and running an MCP server.
  *
  * Compose routes with `withTools` / `withResources` / `withPrompts` (each may be called multiple
  * times; routes combine first-match-wins), then bind a transport:
  *
  * {{{
  * import mcp4s.server.*
  * import mcp4s.server.dsl.*
  *
  * val server = McpServer[IO](ServerInfo("calc", "1.0.0"))
  *   .withTools(mathTools)
  *   .withResources(readme)
  *   .withPrompts(greetings)
  *
  * server.stdio.run                                   // stdio (Claude Desktop etc.)
  * server.http().resource.useForever                  // Streamable HTTP
  * server.http(HttpConfig(port = port"8080")).routes  // embed in an existing http4s app
  * server.webSocket().resource.useForever             // WebSocket
  * }}}
  *
  * Capabilities are derived from what is registered: a tools-only server advertises only tools.
  */
final class McpServer[F[_]: Concurrent] private (
    info: ServerInfo,
    tools: Tools[F],
    resources: Resources[F],
    prompts: Prompts[F]
):

  /** Add tool routes (combined with any already added; first match wins). */
  def withTools(t: Tools[F]): McpServer[F] =
    new McpServer(info, Tools.combine(tools, t), resources, prompts)

  /** Add resource routes. */
  def withResources(r: Resources[F]): McpServer[F] =
    new McpServer(info, tools, Resources.combine(resources, r), prompts)

  /** Add prompt routes. */
  def withPrompts(p: Prompts[F]): McpServer[F] =
    new McpServer(info, tools, resources, Prompts.combine(prompts, p))

  /** The assembled [[Server]], for direct use or composition via `|+|`. */
  def toServer: Server[F] = Server.from(info, tools, resources, prompts)

  /** Bind to the stdio transport (newline-delimited JSON-RPC over stdin/stdout). */
  def stdio: McpServer.StdioBinding[F] = McpServer.StdioBinding(toServer)

  /** Bind to the Streamable HTTP transport. */
  def http(config: HttpConfig[F] = HttpConfig[F]()): McpServer.HttpBinding[F] =
    McpServer.HttpBinding(toServer, config)

  /** Bind to the WebSocket transport. */
  def webSocket(config: WebSocketConfig = WebSocketConfig()): McpServer.WebSocketBinding[F] =
    McpServer.WebSocketBinding(toServer, config)

object McpServer:

  /** Start assembling a server with the given identity. */
  def apply[F[_]: Concurrent](info: ServerInfo): McpServer[F] =
    new McpServer(info, Tools.empty[F], Resources.empty[F], Prompts.empty[F])

  /** Start assembling a server with the given name and version. */
  def apply[F[_]: Concurrent](name: String, version: String): McpServer[F] =
    apply(ServerInfo(name, version))

  final class StdioBinding[F[_]] private[server] (server: Server[F]):
    /** Run until stdin closes. */
    def run(using
        Async[F],
        LiftIO[F]
    )(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): F[Unit] =
      StdioTransport.run[F](server)

  final class HttpBinding[F[_]] private[server] (server: Server[F], config: HttpConfig[F]):
    /** The bound HTTP server as a managed resource. */
    def resource(using
        Async[F],
        Network[F]
    )(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): Resource[F, Http4sServer] =
      HttpTransport.serve[F](server, config)

    /** Raw routes for embedding in an existing http4s application (add your own middleware). */
    def routes(using
        Async[F]
    )(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): Resource[F, HttpRoutes[F]] =
      HttpTransport.routes[F](server, config)

    /** Run the server forever. */
    def run(using
        Async[F],
        Network[F]
    )(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): F[Nothing] =
      resource.useForever

  final class WebSocketBinding[F[_]] private[server] (
      server: Server[F],
      config: WebSocketConfig
  ):
    /** The bound WebSocket server as a managed resource. */
    def resource(using
        Async[F],
        Network[F]
    )(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): Resource[F, Http4sServer] =
      WebSocketTransport.serve[F](server, config)

    /** Run the server forever. */
    def run(using
        Async[F],
        Network[F]
    )(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): F[Nothing] =
      resource.useForever
