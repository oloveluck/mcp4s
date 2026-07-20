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

package mcp4s.client.transport

import cats.effect.{Async, Resource as CatsResource}
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Stream
import io.circe.parser.*
import io.circe.syntax.*
import org.http4s.{Headers, Uri}
import org.http4s.client.websocket.{WSClient, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.{ConnectionRunner, McpClient, McpConnection}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.transport.{McpChannel, Timeouts}

/** WebSocket transport configuration for MCP clients. */
final case class WebSocketTransportConfig[F[_]](
    uri: String,
    /** Authentication sent as a Bearer token on the upgrade request. */
    auth: Option[McpAuth[F]] = None,
    /** Maximum queue size for outgoing messages (bounded queue with backpressure) */
    maxQueueSize: Int = 1024,
    timeouts: Timeouts = Timeouts.default
)

/** WebSocket transport for MCP clients.
  *
  * Connects to an MCP server via a single bidirectional WebSocket connection. This provides lower
  * latency than HTTP/SSE and simplifies connection management.
  *
  * JVM-only: built on http4s `JdkWSClient`, which uses `java.net.http` (JDK 11+). The high-level
  * connection handles ping/pong and close frames automatically.
  */
object WebSocketClientTransport:

  /** Build a [[ClientTransport]] for the given WebSocket configuration. */
  def apply[F[_]: Async](config: WebSocketTransportConfig[F]): ClientTransport[F] =
    new ClientTransport[F]:
      def open: CatsResource[F, McpChannel[F]] =
        for
          wsClient <- CatsResource.eval(JdkWSClient.simple[F])
          channel  <- openWith(wsClient, config)
        yield channel

  /** Build a [[ClientTransport]] using a caller-supplied `WSClient` (e.g. for testing). */
  def apply[F[_]: Async](
      wsClient: WSClient[F],
      config: WebSocketTransportConfig[F]
  ): ClientTransport[F] =
    new ClientTransport[F]:
      def open: CatsResource[F, McpChannel[F]] = openWith(wsClient, config)

  private def openWith[F[_]: Async](
      wsClient: WSClient[F],
      config: WebSocketTransportConfig[F]
  ): CatsResource[F, McpChannel[F]] =
    val uri = Uri.unsafeFromString(config.uri)
    for
      headers  <- CatsResource.eval(McpAuth.applyTo(config.auth, Headers.empty))
      wsConn   <- wsClient.connectHighLevel(WSRequest(uri, headers, org.http4s.Method.GET))
      outQueue <- CatsResource.eval(Queue.bounded[F, String](config.maxQueueSize))

      // Pump outgoing frames from the queue to the socket.
      _ <- Stream
        .fromQueueUnterminated(outQueue)
        .evalMap(text => wsConn.send(WSFrame.Text(text)))
        .compile
        .drain
        .background
    yield new McpChannel[F]:
      def send(message: JsonRpcMessage): F[Unit] =
        outQueue.offer(message.asJson.noSpaces)

      def incoming: Stream[F, JsonRpcMessage] =
        wsConn.receiveStream
          .collect { case WSFrame.Text(text, _) => text }
          .evalMapFilter { text =>
            decode[JsonRpcMessage](text) match
              case Right(message) => Async[F].pure(Some(message))
              case Left(err)      =>
                // Don't drop undecodable frames silently: a malformed response would
                // otherwise stall its request until the timeout with no trace.
                Async[F]
                  .delay(
                    System.err.println(s"[MCP WebSocket] undecodable frame: ${err.getMessage}")
                  )
                  .as(None)
          }

  /** Connect to a WebSocket MCP server. */
  def connect[F[_]: Async](
      client: McpClient[F],
      config: WebSocketTransportConfig[F]
  )(using Tracer[F]): CatsResource[F, McpConnection[F]] =
    ConnectionRunner.run(client, apply(config), config.timeouts, summon[Tracer[F]])
