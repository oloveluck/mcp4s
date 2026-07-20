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

import cats.effect.{Async, Ref, Resource}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import io.circe.syntax.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.RequestCorrelator
import mcp4s.client.transport.ClientTransport
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.transport.{McpChannel, Timeouts}

/** The single shared engine behind every client transport.
  *
  * Given an open [[McpChannel]], this:
  *   - routes responses to the [[RequestCorrelator]] (with a uniform request timeout),
  *   - dispatches server-initiated requests (sampling, elicitation, roots list, ping) to the
  *     [[ClientDispatcher]] and sends the answers back — on every transport, not just WebSocket,
  *   - routes progress notifications to per-request progress handlers,
  *   - performs the one initialize handshake, and
  *   - fails all in-flight requests when the connection closes.
  */
private[client] object ConnectionRunner:

  def run[F[_]: Async](
      client: McpClient[F],
      transport: ClientTransport[F],
      timeouts: Timeouts,
      tracer: Tracer[F]
  ): Resource[F, McpConnection[F]] =
    for
      channel    <- transport.open
      correlator <- Resource.eval(RequestCorrelator[F])
      dispatcher <- Resource.eval(ClientDispatcher[F](client))
      progressHandlersRef <- Resource.eval(
        Ref.of[F, Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]]](None)
      )

      cleanup = correlator.cancelAll(JsonRpcError(-32000, "Connection closed", None))

      // Drain the channel for the lifetime of the connection.
      _ <- channel.incoming
        .evalMap(route(_, channel, correlator, dispatcher, progressHandlersRef))
        .compile
        .drain
        .guarantee(cleanup)
        .background

      sendRequest = (req: JsonRpcRequest) =>
        correlator.request(req.id, timeouts.request)(channel.send(req))
      sendNotification = (notif: JsonRpcNotification) => channel.send(notif)

      connection <- Resource.eval(
        Async[F]
          .timeoutTo(
            handshake(
              client,
              correlator.nextId,
              sendRequest,
              sendNotification,
              progressHandlersRef,
              tracer
            ),
            timeouts.init,
            cleanup *> Async[F].raiseError(
              McpError.InternalError(s"MCP initialization timed out after ${timeouts.init}")
            )
          )
          .onError(_ => cleanup)
      )
    yield connection

  private def route[F[_]: Async](
      message: JsonRpcMessage,
      channel: McpChannel[F],
      correlator: RequestCorrelator[F],
      dispatcher: ClientDispatcher[F],
      progressHandlersRef: Ref[F, Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]]]
  ): F[Unit] =
    message match
      case JsonRpcResponse(id, result) => correlator.complete(id, result)

      case JsonRpcErrorResponse(id, error) => correlator.fail(id, error)

      case JsonRpcNotification(method, params) if method == McpMethod.Progress =>
        val pp = params.flatMap(_.as[ProgressParams].toOption)
        pp.traverse_ { p =>
          progressHandlersRef.get.flatMap(_.traverse_ { handlers =>
            handlers.get.flatMap(_.get(p.progressToken).traverse_(_(p)))
          })
        }

      case notif: JsonRpcNotification =>
        dispatcher.dispatch(notif).void

      case req: JsonRpcRequest =>
        dispatcher.dispatch(req).flatMap {
          case Some(response) => channel.send(response)
          case None           => Async[F].unit
        }

  private def handshake[F[_]: Async](
      client: McpClient[F],
      nextId: F[RequestId],
      sendRequest: JsonRpcRequest => F[io.circe.Json],
      sendNotification: JsonRpcNotification => F[Unit],
      progressHandlersRef: Ref[F, Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]]],
      tracer: Tracer[F]
  ): F[McpConnection[F]] =
    for
      initId <- nextId
      initRequest = JsonRpcRequest(
        initId,
        McpMethod.Initialize,
        Some(
          InitializeParams(
            protocolVersion = McpVersion.Current,
            capabilities = client.capabilities,
            clientInfo = client.info
          ).asJson
        )
      )
      initJson   <- sendRequest(initRequest)
      initResult <- initJson.as[InitializeResult].liftTo[F]
      _          <- sendNotification(JsonRpcNotification(McpMethod.Initialized, None))
      connection <- McpConnection[F](
        initResult.serverInfo,
        initResult.capabilities,
        nextId,
        sendRequest,
        sendNotification,
        tracer
      )
      _ <- progressHandlersRef.set(Some(connection.progressHandlers))
    yield connection
