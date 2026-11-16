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

package mcp4s.server.transport

import cats.effect.{Async, Resource as CatsResource}
import cats.effect.std.{Queue, Supervisor}
import cats.syntax.all.*
import com.comcast.ip4s.{host, port, Host, Port}
import fs2.{Pipe, Stream}
import fs2.io.net.Network
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.{Router, Server as Http4sServer}
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*

import scala.concurrent.duration.*

/** WebSocket transport configuration */
final case class WebSocketConfig(
    host: Host = host"0.0.0.0",
    port: Port = port"3000",
    path: String = "ws",
    /** Maximum queue size for outgoing messages (bounded queue with backpressure) */
    maxQueueSize: Int = 1000,
    /** Request timeout for pending server-to-client operations (e.g. sampling) */
    requestTimeout: FiniteDuration = 5.minutes
)

object WebSocketConfig:
  val default: WebSocketConfig = WebSocketConfig()

/** WebSocket transport for MCP servers.
  *
  * Provides bidirectional JSON-RPC communication over a single WebSocket connection. This is an
  * alternative to HTTP/SSE transport, offering lower latency and simpler connection management for
  * scenarios where both client and server need to send messages asynchronously.
  *
  * Supports server-to-client sampling requests when the client advertises sampling capability.
  *
  * Endpoints:
  *   - GET /ws: WebSocket upgrade endpoint for bidirectional JSON-RPC
  *   - GET /health: Health check endpoint
  */
object WebSocketTransport:

  /** Return the raw MCP WebSocket `HttpRoutes` without any middleware wrapping.
    *
    * Use this to embed MCP WebSocket routes in an existing http4s application and compose standard
    * http4s middleware (CORS, auth, etc.) yourself.
    *
    * @param server
    *   The MCP server to serve
    * @param wsb
    *   WebSocket builder from http4s
    * @param config
    *   WebSocket configuration
    */
  def routes[F[_]: Async](
      server: Server[F],
      wsb: WebSocketBuilder2[F],
      config: WebSocketConfig = WebSocketConfig.default
  )(using Tracer[F]): HttpRoutes[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    HttpRoutes.of[F] {
      case GET -> Root / config.path =>
        createWebSocket(server, wsb, summon[Tracer[F]], config)

      case GET -> Root / "health" =>
        Ok(Json.obj("status" -> Json.fromString("ok")))
    }

  /** Start a WebSocket server for the given MCP server.
    *
    * @param server
    *   The MCP server to serve
    * @param config
    *   WebSocket configuration
    */
  def serve[F[_]: Async: Network](
      server: Server[F],
      config: WebSocketConfig = WebSocketConfig.default
  )(using Tracer[F]): CatsResource[F, Http4sServer] =
    EmberServerBuilder
      .default[F]
      .withHost(config.host)
      .withPort(config.port)
      .withHttpWebSocketApp(wsb => Router("/" -> routes(server, wsb, config)).orNotFound)
      .build

  private def createWebSocket[F[_]: Async](
      server: Server[F],
      wsb: WebSocketBuilder2[F],
      tracer: Tracer[F],
      config: WebSocketConfig
  ): F[Response[F]] =
    for
      // Outgoing frames (bounded, with backpressure). Every server-to-client message — responses,
      // notifications and server-initiated requests — is framed onto this one queue.
      outQueue <- Queue.bounded[F, WebSocketFrame](config.maxQueueSize)
      send = (message: JsonRpcMessage) =>
        outQueue.offer(WebSocketFrame.Text(message.asJson.noSpaces))
      session <- ServerSession
        .create[F](server, Outbound.uniform(send), config.requestTimeout, tracer)
      response <-
        // Incoming frames are handled under a connection-scoped Supervisor: dispatch runs in its
        // own fiber so the receive pipe stays free (a tool may await a sampling/elicitation round
        // trip whose answer is a later frame), and a handler still running when the socket closes
        // is cancelled rather than leaked.
        val receive: Pipe[F, WebSocketFrame, Unit] = in =>
          Stream
            .resource(Supervisor[F])
            .flatMap { supervisor =>
              in.evalMap {
                case WebSocketFrame.Text(text, _) =>
                  tracer
                    .span("ws.message.receive")
                    .surround(handleText(session, text, send, supervisor))
                case WebSocketFrame.Ping(data) => outQueue.offer(WebSocketFrame.Pong(data))
                case _                         => Async[F].unit
              }
            }
            .onFinalize(session.close)

        wsb.build(Stream.fromQueueUnterminated(outQueue), receive)
    yield response

  private def handleText[F[_]: Async](
      session: ServerSession[F],
      text: String,
      send: JsonRpcMessage => F[Unit],
      supervisor: Supervisor[F]
  ): F[Unit] =
    decode[JsonRpcMessage](text) match
      case Right(message) =>
        supervisor.supervise(session.handle(message).flatMap(_.traverse_(send))).void
      case Left(err) =>
        send(JsonRpcErrorResponse(RequestId.NullId, JsonRpcError.parseError(err.getMessage)))
