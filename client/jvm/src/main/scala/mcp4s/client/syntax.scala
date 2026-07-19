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
import mcp4s.client.transport.{
  HttpTransportConfig,
  WebSocketClientTransport,
  WebSocketTransportConfig
}

/** JVM-only transport bindings (`import mcp4s.client.syntax.*`).
  *
  * The cross-platform bindings (`client.stdio(...)`, `client.http(config, httpClient)`) need no
  * import — they live on [[McpClient]]/[[McpClientBuilder]] directly. The JVM adds:
  *   - `webSocket` — bidirectional WebSocket (http4s `JdkWSClient`).
  *   - `http` without a `Client[F]` — builds and manages an Ember client for you.
  */
object syntax:

  extension [F[_]](client: McpClient[F])

    /** Connect over WebSocket (JVM-only). Manages the http4s `JdkWSClient` internally. */
    def webSocket(config: WebSocketTransportConfig[F])(using Async[F])(using
        tracer: Tracer[F] = Tracer.noop[F]
    ): Resource[F, McpConnection[F]] =
      WebSocketClientTransport.connect[F](client, config)

    /** Connect over WebSocket at `uri` (other settings default; JVM-only). */
    def webSocket(uri: String)(using Async[F]): Resource[F, McpConnection[F]] =
      webSocket(WebSocketTransportConfig[F](uri))

    /** Connect over Streamable HTTP, building and managing an Ember client for you (JVM-only).
      *
      * The Ember client's lifecycle is bracketed into the returned `Resource`. For a custom or
      * shared backend, use `client.http(config, httpClient)`.
      */
    def http(config: HttpTransportConfig[F])(using
        Async[F],
        Network[F]
    ): Resource[F, McpConnection[F]] =
      EmberClientBuilder.default[F].build.flatMap(client.http(config, _))

    /** Connect over Streamable HTTP at `uri`, building an Ember client for you (JVM-only). */
    def http(uri: String)(using Async[F], Network[F]): Resource[F, McpConnection[F]] =
      http(HttpTransportConfig[F](uri))

  extension [F[_]](builder: McpClientBuilder[F])

    /** Connect over WebSocket (JVM-only). Uses the `Tracer[F]` in scope, or noop. */
    inline def webSocket(config: WebSocketTransportConfig[F])(using
        Async[F]
    ): Resource[F, McpConnection[F]] =
      builder.toClient.webSocket(config)(using summon[Async[F]])(using tracerOrNoop[F])

    /** Connect over WebSocket at `uri` (other settings default; JVM-only). */
    inline def webSocket(uri: String)(using Async[F]): Resource[F, McpConnection[F]] =
      builder.webSocket(WebSocketTransportConfig[F](uri))

    /** Connect over Streamable HTTP, building and managing an Ember client for you (JVM-only). */
    inline def http(config: HttpTransportConfig[F])(using
        Async[F],
        Network[F]
    ): Resource[F, McpConnection[F]] =
      EmberClientBuilder
        .default[F]
        .build
        .flatMap(builder.http(config, _)(using summon[Async[F]])(using tracerOrNoop[F]))

    /** Connect over Streamable HTTP at `uri`, building an Ember client for you (JVM-only). */
    inline def http(uri: String)(using Async[F], Network[F]): Resource[F, McpConnection[F]] =
      builder.http(HttpTransportConfig[F](uri))

  /** The `Tracer[F]` in scope at the inline site, or noop when none is given. */
  private inline def tracerOrNoop[F[_]](using Async[F]): Tracer[F] =
    scala.compiletime.summonFrom {
      case t: Tracer[F] => t
      case _            => Tracer.noop[F]
    }
