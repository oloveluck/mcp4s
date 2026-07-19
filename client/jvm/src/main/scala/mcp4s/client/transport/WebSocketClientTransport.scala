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

import cats.effect.{Async, Ref, Resource as CatsResource}
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Stream
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import org.http4s.Uri
import org.http4s.client.websocket.{WSClient, WSDataFrame, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient
import org.typelevel.otel4s.trace.Tracer
import mcp4s.RequestCorrelator
import mcp4s.client.{ClientDispatcher, McpClient, McpConnection}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given

/** WebSocket transport configuration for MCP clients */
final case class WebSocketClientConfig(
    url: String,
    path: String = "ws",
    /** Maximum queue size for outgoing messages (bounded queue with backpressure) */
    maxQueueSize: Int = 1000,
    /** Request timeout for pending operations */
    requestTimeout: scala.concurrent.duration.FiniteDuration =
      scala.concurrent.duration.FiniteDuration(5, "min"),
    /** Timeout for the entire initialization sequence (connect + initialize handshake) */
    initTimeout: scala.concurrent.duration.FiniteDuration =
      scala.concurrent.duration.FiniteDuration(30, "s")
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

  /** Connect to a WebSocket MCP server.
    *
    * @param client
    *   The MCP client configuration
    * @param config
    *   WebSocket transport configuration
    * @param tracer
    *   OpenTelemetry tracer for distributed tracing
    */
  def connect[F[_]: Async](
      client: McpClient[F],
      config: WebSocketClientConfig
  )(using Tracer[F]): CatsResource[F, McpConnection[F]] =
    for
      wsClient         <- CatsResource.eval(JdkWSClient.simple[F])
      clientDispatcher <- CatsResource.eval(ClientDispatcher[F](client))
      connection <- establishConnection(
        client,
        wsClient,
        clientDispatcher,
        config,
        summon[Tracer[F]]
      )
    yield connection

  private def establishConnection[F[_]: Async](
      client: McpClient[F],
      wsClient: WSClient[F],
      clientDispatcher: ClientDispatcher[F],
      config: WebSocketClientConfig,
      tracer: Tracer[F]
  ): CatsResource[F, McpConnection[F]] =
    val uri = Uri.unsafeFromString(s"${config.url}/${config.path}")

    for
      // Correlates server responses back to in-flight client requests.
      correlator <- CatsResource.eval(RequestCorrelator[F])
      // Queue for outgoing data frames (bounded, with backpressure).
      outQueue <- CatsResource.eval(Queue.bounded[F, WSDataFrame](config.maxQueueSize))
      // Indirection for progress handlers - set after connection creation.
      progressHandlersRef <- CatsResource.eval(
        Ref.of[F, Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]]](None)
      )

      // Establish the live WebSocket connection (high-level: control frames handled for us).
      wsConn <- wsClient.connectHighLevel(WSRequest(uri))

      // Fail all pending requests when the connection closes.
      cleanupPendingRequests = correlator.cancelAll(JsonRpcError(-32000, "Connection closed", None))

      handleIncoming = (text: String) =>
        decode[JsonRpcMessage](text) match
          case Right(response: JsonRpcResponse) =>
            correlator.complete(response.id, response.result)

          case Right(errorResponse: JsonRpcErrorResponse) =>
            correlator.fail(errorResponse.id, errorResponse.error)

          case Right(request: JsonRpcRequest) =>
            clientDispatcher.dispatch(request).flatMap {
              case Some(response) => outQueue.offer(WSFrame.Text(response.asJson.noSpaces))
              case None           => Async[F].unit
            }

          case Right(notif: JsonRpcNotification) if notif.method == McpMethod.Progress =>
            val pp = notif.params.flatMap(_.as[ProgressParams].toOption)
            pp.traverse_ { p =>
              progressHandlersRef.get.flatMap(_.traverse_ { handlers =>
                handlers.get.flatMap(_.get(p.progressToken).traverse_(_(p)))
              })
            }

          case Right(_: JsonRpcNotification) => Async[F].unit
          case Left(_)                       => Async[F].unit

      sendRequest = (req: JsonRpcRequest) =>
        correlator.request(req.id, config.requestTimeout):
          outQueue.offer(WSFrame.Text(req.asJson.noSpaces))

      sendNotification = (notif: JsonRpcNotification) =>
        outQueue.offer(WSFrame.Text(notif.asJson.noSpaces))

      // Pump outgoing frames from the queue to the socket.
      _ <- Stream.fromQueueUnterminated(outQueue).evalMap(wsConn.send).compile.drain.background

      // Process incoming frames; clean up pending requests when the stream ends (socket closed).
      _ <- wsConn.receiveStream
        .evalMap {
          case WSFrame.Text(text, _) => handleIncoming(text)
          case _                     => Async[F].unit
        }
        .compile
        .drain
        .guarantee(cleanupPendingRequests)
        .background

      initRequest = JsonRpcRequest(
        RequestId.NumberId(1),
        McpMethod.Initialize,
        Some(
          InitializeParams(
            protocolVersion = McpVersion.Current,
            capabilities = client.capabilities,
            clientInfo = client.info
          ).asJson
        )
      )

      doInit = for
        initResult <- sendRequest(initRequest).flatMap(_.as[InitializeResult].liftTo[F])
        _          <- sendNotification(JsonRpcNotification(McpMethod.Initialized, None))
        conn <- McpConnection[F](
          initResult.serverInfo,
          initResult.capabilities,
          sendRequest,
          sendNotification,
          tracer
        )
        _ <- progressHandlersRef.set(Some(conn.progressHandlers))
      yield conn

      // Run the initialize handshake with a timeout; clean up on failure so nothing hangs.
      connection <- CatsResource.eval(
        Async[F]
          .timeoutTo(
            doInit,
            config.initTimeout,
            cleanupPendingRequests *> Async[F].raiseError(
              McpError.InternalError(
                s"WebSocket initialization timed out after ${config.initTimeout}"
              )
            )
          )
          .onError(_ => cleanupPendingRequests)
      )
    yield connection
