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

import cats.effect.{Async, Ref, Resource as CatsResource}
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

/** WebSocket transport configuration */
final case class WebSocketConfig(
    host: Host = host"0.0.0.0",
    port: Port = port"3000",
    path: String = "ws",
    /** Maximum queue size for outgoing messages (bounded queue with backpressure) */
    maxQueueSize: Int = 1000,
    /** Request timeout for pending server-to-client operations (e.g. sampling) */
    requestTimeout: scala.concurrent.duration.FiniteDuration =
      scala.concurrent.duration.FiniteDuration(5, "min")
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
    // Create session state for this connection
    WebSocketSession[F](server, tracer, config).flatMap { session =>
      // Queue for outgoing messages (bounded with backpressure)
      Queue.bounded[F, WebSocketFrame](config.maxQueueSize).flatMap { outQueue =>
        // Give session access to the outQueue for sending requests
        session.setOutQueue(outQueue) *> {
          // Process incoming frames and dispatch to handler, with cleanup on disconnect.
          // Dispatch fibers run under a connection-scoped Supervisor so a tool call
          // still running when the socket closes is cancelled rather than leaked.
          val receive: Pipe[F, WebSocketFrame, Unit] = in =>
            Stream
              .resource(Supervisor[F])
              .flatMap { supervisor =>
                in.evalMap {
                  case WebSocketFrame.Text(text, _) =>
                    tracer.span("ws.message.receive").surround {
                      session.handleMessage(text, outQueue, supervisor)
                    }

                  case WebSocketFrame.Close(_) =>
                    Async[F].unit

                  case WebSocketFrame.Ping(data) =>
                    outQueue.offer(WebSocketFrame.Pong(data))

                  case _ =>
                    Async[F].unit
                }
              }
              .onFinalize(session.cleanup)

          // Stream of outgoing frames from the queue
          val send: Stream[F, WebSocketFrame] = Stream.fromQueueUnterminated(outQueue)

          wsb.build(send, receive)
        }
      }
    }

/** Manages state for a single WebSocket connection.
  *
  * The bidirectional machinery (correlation, capability gating, sampling/elicitation requesters)
  * lives in the shared [[ServerSession]]; this class adds WebSocket frame handling and dispatch.
  */
private class WebSocketSession[F[_]: Async](
    dispatcherRef: Ref[F, Option[Dispatcher[F]]],
    session: ServerSession[F],
    outQueueRef: Ref[F, Option[Queue[F, WebSocketFrame]]]
):

  /** Clean up session resources on disconnect. Fails all pending requests with a connection-closed
    * error and clears refs.
    */
  def cleanup: F[Unit] =
    session.cancelPending *> outQueueRef.set(None)

  /** Set the dispatcher (used during initialization) */
  private[transport] def setDispatcher(d: Dispatcher[F]): F[Unit] =
    dispatcherRef.set(Some(d))

  /** Set the output queue for sending messages */
  def setOutQueue(queue: Queue[F, WebSocketFrame]): F[Unit] =
    outQueueRef.set(Some(queue))

  /** Handle an incoming WebSocket message */
  def handleMessage(
      text: String,
      outQueue: Queue[F, WebSocketFrame],
      supervisor: Supervisor[F]
  ): F[Unit] =
    decode[JsonRpcMessage](text) match
      case Right(message) =>
        message match
          case resp: JsonRpcResponse =>
            // Response to a server-initiated request (e.g., sampling)
            session.handleResponse(resp)

          case errResp: JsonRpcErrorResponse =>
            // Error response to a server-initiated request
            session.handleErrorResponse(errResp)

          case req: JsonRpcRequest =>
            // Client request - dispatch and maybe capture client capabilities.
            // Dispatch runs in its own fiber so the receive pipe stays free: a tool
            // handler may await a server-initiated round trip (sampling/elicitation)
            // whose response arrives as a later frame on this same connection.
            dispatcherRef.get.flatMap {
              case Some(dispatcher) =>
                session.maybeExtractClientCaps(req) *>
                  supervisor
                    .supervise(
                      dispatcher
                        .dispatch(req)
                        .flatMap {
                          case Some(response) =>
                            outQueue.offer(WebSocketFrame.Text(response.asJson.noSpaces))
                          case None =>
                            Async[F].unit
                        }
                    )
                    .void
              case None =>
                // Dispatcher not yet initialized - this shouldn't happen
                val error =
                  JsonRpcErrorResponse(req.id, JsonRpcError.internalError("Server not ready"))
                outQueue.offer(WebSocketFrame.Text(error.asJson.noSpaces))
            }

          case notif: JsonRpcNotification =>
            // Client notification
            dispatcherRef.get.flatMap {
              case Some(dispatcher) => dispatcher.dispatch(notif).void
              case None             => Async[F].unit
            }

      case Left(err) =>
        // Send parse error response
        val error = JsonRpcErrorResponse(
          RequestId.NullId,
          JsonRpcError.parseError(err.getMessage)
        )
        outQueue.offer(WebSocketFrame.Text(error.asJson.noSpaces))

private object WebSocketSession:
  def apply[F[_]: Async](
      server: Server[F],
      tracer: Tracer[F],
      config: WebSocketConfig = WebSocketConfig.default
  ): F[WebSocketSession[F]] =
    given Tracer[F] = tracer
    for
      dispatcherRef <- Ref.of[F, Option[Dispatcher[F]]](None)
      outQueueRef   <- Ref.of[F, Option[Queue[F, WebSocketFrame]]](None)

      // Outbound delivery: frame the message and offer it to the connection's queue (if open).
      sendMessage = (message: JsonRpcMessage) =>
        outQueueRef.get.flatMap {
          case Some(queue) => queue.offer(WebSocketFrame.Text(message.asJson.noSpaces))
          case None        => Async[F].unit
        }

      shared <- ServerSession.create[F](sendMessage, config.requestTimeout, tracer)
      session = new WebSocketSession(dispatcherRef, shared, outQueueRef)

      // Create context factory with full capabilities: sampling, elicitation, progress, logging
      contextFactory = (reqId: RequestId, progressToken: Option[RequestId]) =>
        ToolContext[F](
          shared.samplingRequester,
          shared.elicitationRequester,
          reqId,
          progressToken,
          shared.sendProgressNotification,
          shared.sendLoggingNotification
        )
      // Create dispatcher with context factory
      dispatcher <- Dispatcher.withContext[F](server, contextFactory)
      // Set the dispatcher
      _ <- session.setDispatcher(dispatcher)
    yield session
