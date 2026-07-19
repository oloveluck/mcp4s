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
import fs2.io.net.Network
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.transport.{HttpClientConfig, WebSocketClientConfig, WebSocketClientTransport}

/** Transport-selection extensions for [[McpClient]] (`import mcp4s.client.syntax.*`).
  *
  * On top of the cross-platform [[ClientSyntax]] (`connectStdio` / `connectHttp` with a
  * caller-supplied `Client[F]`), the JVM adds:
  *   - `connectWebSocket` — bidirectional WebSocket (JVM-only; http4s `JdkWSClient`).
  *   - `connectHttp` without a `Client[F]` — builds and manages an Ember client for you.
  */
object syntax extends ClientSyntax:

  extension [F[_]](client: McpClient[F])

    /** Connect over WebSocket (JVM-only). Manages the http4s `JdkWSClient` internally.
      *
      * {{{
      * client.connectWebSocket(WebSocketClientConfig("ws://localhost:3000")).use { ... }
      * }}}
      */
    def connectWebSocket(config: WebSocketClientConfig)(using Async[F])(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): Resource[F, McpConnection[F]] =
      WebSocketClientTransport.connect[F](client, config)

    /** Connect over WebSocket at `url` (other settings default; JVM-only).
      *
      * For tracing, use the `WebSocketClientConfig` overload with a `given Tracer[F]` in scope.
      *
      * {{{
      * client.connectWebSocket("ws://localhost:3000").use { conn => ... }
      * }}}
      */
    def connectWebSocket(url: String)(using Async[F]): Resource[F, McpConnection[F]] =
      connectWebSocket(WebSocketClientConfig(url))

    /** Connect over Streamable HTTP, building and managing an Ember client for you (JVM-only).
      *
      * The Ember client's lifecycle is bracketed into the returned `Resource`. For a custom or
      * shared backend (or to enable tracing), use the [[ClientSyntax]] overload that takes a
      * `Client[F]` with a `given Tracer[F]` in scope.
      *
      * {{{
      * client.connectHttp(HttpClientConfig("http://localhost:3000")).use { conn => ... }
      * }}}
      */
    def connectHttp(config: HttpClientConfig[F])(using
        Async[F],
        Network[F]
    ): Resource[F, McpConnection[F]] =
      EmberClientBuilder.default[F].build.flatMap(connectHttp(config, _))

    /** Connect over Streamable HTTP at `baseUrl`, building an Ember client for you (JVM-only).
      *
      * {{{
      * client.connectHttp("http://localhost:3000").use { conn => ... }
      * }}}
      */
    def connectHttp(baseUrl: String)(using Async[F], Network[F]): Resource[F, McpConnection[F]] =
      connectHttp(HttpClientConfig[F](baseUrl))
