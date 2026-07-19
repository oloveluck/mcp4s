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

import cats.effect.{Async, Resource}
import fs2.io.process.Processes
import org.http4s.client.Client
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.transport.{
  HttpClientConfig,
  HttpClientTransport,
  StdioClientConfig,
  StdioClientTransport
}

/** Cross-platform transport-selection extensions for [[McpClient]].
  *
  * Mirrors `mcp4s.server.syntax` — one method per transport, called on the client you already hold.
  * Each returns a `Resource[F, McpConnection[F]]`; `.use` it to talk to the server.
  *
  * Platform note: `connectWebSocket` and the no-`Client` `connectHttp` convenience are JVM-only
  * (see the JVM `mcp4s.client.syntax`). The cross-platform `connectHttp` takes an http4s
  * `Client[F]` so each platform can supply its own backend.
  */
trait ClientSyntax:

  extension [F[_]](client: McpClient[F])

    /** Connect by spawning a subprocess and speaking JSON-RPC over its stdin/stdout.
      *
      * {{{
      * client.connectStdio(StdioClientConfig("node", List("server.js"))).use { conn => ... }
      * }}}
      */
    def connectStdio(config: StdioClientConfig)(using Async[F], Processes[F])(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): Resource[F, McpConnection[F]] =
      StdioClientTransport.connect[F](client, config)

    /** Connect by spawning `command` with `args` (other settings default).
      *
      * For tracing, use the `StdioClientConfig` overload with a `given Tracer[F]` in scope.
      *
      * {{{
      * client.connectStdio("node", "server.js").use { conn => ... }
      * }}}
      */
    def connectStdio(command: String, args: String*)(using
        Async[F],
        Processes[F]
    ): Resource[F, McpConnection[F]] =
      connectStdio(StdioClientConfig(command, args.toList))

    /** Connect over Streamable HTTP using a caller-provided http4s `Client[F]`.
      *
      * Bring your own backend (e.g. `EmberClientBuilder`) so this works on every platform. On the
      * JVM, see the no-`Client` overload that builds an Ember client for you.
      *
      * {{{
      * client.connectHttp(HttpClientConfig("http://localhost:3000"), httpClient).use { ... }
      * }}}
      */
    def connectHttp(config: HttpClientConfig[F], httpClient: Client[F])(using Async[F])(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): Resource[F, McpConnection[F]] =
      HttpClientTransport.connect[F](client, config, httpClient)

    /** Connect over Streamable HTTP at `baseUrl` using a caller-provided http4s `Client[F]`.
      *
      * For tracing, use the `HttpClientConfig` overload with a `given Tracer[F]` in scope.
      *
      * {{{
      * client.connectHttp("http://localhost:3000", httpClient).use { conn => ... }
      * }}}
      */
    def connectHttp(baseUrl: String, httpClient: Client[F])(using
        Async[F]
    ): Resource[F, McpConnection[F]] =
      connectHttp(HttpClientConfig[F](baseUrl), httpClient)
