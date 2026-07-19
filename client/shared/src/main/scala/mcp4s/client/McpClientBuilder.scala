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

import cats.effect.{Async, Concurrent, Resource}
import cats.syntax.all.*
import fs2.io.process.Processes
import org.http4s.client.Client
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.ClientInfo
import mcp4s.client.transport.{
  HttpClientTransport,
  HttpTransportConfig,
  StdioClientTransport,
  StdioTransportConfig
}

/** The one entry point for assembling and connecting an MCP client — the mirror image of
  * `McpServer` on the server side.
  *
  * Compose capabilities with `withRoots` / `withSampling` / `withElicitation` (advertised
  * capabilities are derived from which handlers are present), then connect over a transport:
  *
  * {{{
  * import mcp4s.client.*
  * import mcp4s.client.mcp.*
  *
  * val client = McpClientBuilder[IO](ClientInfo("cli", "1.0.0"))
  *   .withRoots(Roots[IO]("file:///workspace", "Workspace"))
  *   .withSampling(Sampling[IO](params => ...))
  *
  * client.stdio(StdioTransportConfig("node", List("server.js"))).use { conn => ... }
  * client.http(HttpTransportConfig("http://localhost:3000/mcp"), httpClient).use { ... }
  * client.webSocket(WebSocketTransportConfig("ws://localhost:3000/ws")).use { ... }  // JVM
  * }}}
  *
  * Every `connect` method returns a `Resource[F, McpConnection[F]]`; releasing it closes the
  * connection. `webSocket` and the no-`Client` `http` overload are JVM-only extensions (see the
  * JVM `mcp4s.client.syntax`).
  */
final class McpClientBuilder[F[_]: Concurrent] private (
    info: ClientInfo,
    roots: Option[Roots[F]],
    sampling: Option[Samplings[F]],
    elicitation: Option[Elicitations[F]]
):

  /** Add roots (combined with any already added). Advertises the `roots` capability. */
  def withRoots(r: Roots[F]): McpClientBuilder[F] =
    new McpClientBuilder(info, Some(roots.fold(r)(_ |+| r)), sampling, elicitation)

  /** Add a sampling handler (tried in order after any already added). Advertises `sampling`. */
  def withSampling(s: Samplings[F]): McpClientBuilder[F] =
    new McpClientBuilder(info, roots, Some(sampling.fold(s)(_ |+| s)), elicitation)

  /** Add an elicitation handler (tried in order after any already added). Advertises
    * `elicitation`.
    */
  def withElicitation(e: Elicitations[F]): McpClientBuilder[F] =
    new McpClientBuilder(info, roots, sampling, Some(elicitation.fold(e)(_ |+| e)))

  /** The assembled [[McpClient]] (handlers + derived capabilities), for use with a transport's
    * `connect` directly.
    */
  def toClient: McpClient[F] =
    McpClient.from(info, roots, sampling, elicitation)

  /** Connect by spawning a subprocess and speaking JSON-RPC over its stdin/stdout. */
  def stdio(config: StdioTransportConfig)(using Async[F], Processes[F])(using
      tracer: Tracer[F] = Tracer.noop[F]
  ): Resource[F, McpConnection[F]] =
    StdioClientTransport.connect[F](toClient, config)

  /** Connect by spawning `command` with `args` (other settings default). */
  def stdio(command: String, args: String*)(using
      Async[F],
      Processes[F]
  ): Resource[F, McpConnection[F]] =
    stdio(StdioTransportConfig(command, args.toList))

  /** Connect over Streamable HTTP using a caller-provided http4s `Client[F]`.
    *
    * Bring your own backend (e.g. `EmberClientBuilder`) so this works on every platform. On the
    * JVM, see the no-`Client` overload that builds an Ember client for you.
    */
  def http(config: HttpTransportConfig[F], httpClient: Client[F])(using Async[F])(using
      tracer: Tracer[F] = Tracer.noop[F]
  ): Resource[F, McpConnection[F]] =
    HttpClientTransport.connect[F](toClient, config, httpClient)

object McpClientBuilder:

  /** Start assembling a client with the given identity. */
  def apply[F[_]: Concurrent](info: ClientInfo): McpClientBuilder[F] =
    new McpClientBuilder(info, None, None, None)

  /** A builder is usable anywhere an [[McpClient]] is expected (e.g. the JVM-only `webSocket` /
    * auto-Ember `http` extensions in `mcp4s.client.syntax`).
    */
  given toMcpClient[F[_]]: Conversion[McpClientBuilder[F], McpClient[F]] = _.toClient

  /** Start assembling a client with the given name and version. */
  def apply[F[_]: Concurrent](name: String, version: String): McpClientBuilder[F] =
    apply(ClientInfo(name, version))
