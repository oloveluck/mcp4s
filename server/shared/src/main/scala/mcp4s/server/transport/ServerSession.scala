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

import cats.effect.{Async, Deferred, Fiber}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import io.circe.{Decoder, Json}
import io.circe.syntax.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.RequestCorrelator
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.{Dispatcher, ElicitationRequester, SamplingRequester, Server, ToolContext}

import scala.concurrent.duration.FiniteDuration

/** Where a transport delivers server-to-client messages.
  *
  * The Streamable HTTP spec distinguishes messages that relate to a client request (progress,
  * logging, sampling and elicitation requests made while handling it — these ride the SSE stream of
  * that request) from messages unrelated to any request (resource updates, list changes — these go
  * on the standalone GET stream). Duplex transports such as WebSocket and stdio deliver both kinds
  * on their single connection.
  */
private[transport] trait Outbound[F[_]]:
  /** Deliver a message related to the in-flight client request `reqId`. */
  def forRequest(reqId: RequestId): JsonRpcMessage => F[Unit]

  /** Deliver a message unrelated to any client request. */
  def standalone: JsonRpcMessage => F[Unit]

private[transport] object Outbound:
  /** A single channel carries every message (WebSocket, stdio). */
  def uniform[F[_]](send: JsonRpcMessage => F[Unit]): Outbound[F] =
    new Outbound[F]:
      def forRequest(reqId: RequestId): JsonRpcMessage => F[Unit] = send
      def standalone: JsonRpcMessage => F[Unit]                   = send

/** The transport-independent half of a server-side session.
  *
  * Every server transport needs identical machinery around its [[mcp4s.server.Dispatcher]]:
  * correlate server-initiated requests with the client's responses, build per-request
  * [[mcp4s.server.ToolContext]]s whose sampling/elicitation/progress/logging go to the right
  * outbound channel, honour the session's `logging/setLevel`, and forward resource changes the
  * client subscribed to. This class captures that once; transports supply only the [[Outbound]]
  * delivery functions.
  */
final private[transport] class ServerSession[F[_]] private (
    val dispatcher: Dispatcher[F],
    correlator: RequestCorrelator[F],
    changesFiber: Fiber[F, Throwable, Unit]
)(using F: Async[F]):

  /** Route one inbound message: client responses complete pending server-initiated requests;
    * requests and notifications go to the dispatcher.
    */
  def handle(message: JsonRpcMessage): F[Option[JsonRpcMessage]] =
    message match
      case resp: JsonRpcResponse         => correlator.complete(resp.id, resp.result).as(None)
      case errResp: JsonRpcErrorResponse => correlator.fail(errResp.id, errResp.error).as(None)
      case other                         => dispatcher.dispatch(other)

  /** Number of server-initiated requests awaiting a client response. */
  def pendingRequestCount: F[Int] = correlator.inFlightCount

  /** Fail all pending server-initiated requests (e.g. on disconnect or session expiry). */
  def cancelPending: F[Unit] =
    correlator.cancelAll(JsonRpcError(-32000, "Connection closed", None))

  /** Tear the session down: fail pending requests, stop forwarding resource changes, and cancel
    * running tasks. Idempotent.
    */
  def close: F[Unit] =
    cancelPending *> changesFiber.cancel *> dispatcher.close

private[transport] object ServerSession:

  def create[F[_]: Async](
      server: Server[F],
      outbound: Outbound[F],
      requestTimeout: FiniteDuration,
      tracer: Tracer[F],
      config: Dispatcher.Config = Dispatcher.Config.default
  ): F[ServerSession[F]] =
    for
      correlator    <- RequestCorrelator[F]
      dispatcherDef <- Deferred[F, Dispatcher[F]]
      contextFactory = (reqId: RequestId, progressToken: Option[RequestId]) =>
        dispatcherDef.get.flatMap { dispatcher =>
          dispatcher.clientCapabilities.map { caps =>
            toolContext(
              dispatcher,
              correlator,
              outbound.forRequest(reqId),
              requestTimeout,
              tracer,
              caps.getOrElse(ClientCapabilities()),
              reqId,
              progressToken
            )
          }
        }
      dispatcher <-
        given Tracer[F] = tracer
        Dispatcher.withContext[F](server, contextFactory, config)
      _ <- dispatcherDef.complete(dispatcher)
      // Resource changes the client subscribed to become notifications/resources/updated on
      // the standalone channel. Runs for the session's lifetime.
      changes <- server.resourceChanges
        .evalMap { uri =>
          dispatcher.subscriptions.flatMap { subscribed =>
            if subscribed.contains(uri) then
              outbound.standalone(
                JsonRpcNotification(
                  McpMethod.ResourcesUpdated,
                  Some(Json.obj("uri" -> Json.fromString(uri)))
                )
              )
            else Async[F].unit
          }
        }
        .compile
        .drain
        .start
    yield new ServerSession(dispatcher, correlator, changes)

  /** A tool context bound to one outbound channel. */
  private def toolContext[F[_]](
      dispatcher: Dispatcher[F],
      correlator: RequestCorrelator[F],
      send: JsonRpcMessage => F[Unit],
      requestTimeout: FiniteDuration,
      tracer: Tracer[F],
      clientCaps: ClientCapabilities,
      reqId: RequestId,
      progressToken: Option[RequestId]
  )(using F: Async[F]): ToolContext[F] =

    def request[A: Decoder](method: String, params: Json): F[A] =
      for
        id   <- correlator.nextId
        json <- correlator.request(id, requestTimeout):
          send(JsonRpcRequest(id, method, Some(params)))
        decoded <- json.as[A].liftTo[F]
      yield decoded

    val sampling = new SamplingRequester[F]:
      val supportsSampling: Boolean = clientCaps.sampling.isDefined

      def createMessage(params: CreateMessageParams): F[CreateMessageResult] =
        tracer.span("mcp.sampling.createMessage").surround {
          if supportsSampling then
            request[CreateMessageResult](McpMethod.SamplingCreateMessage, params.asJson)
          else F.raiseError(McpError.SamplingNotSupported)
        }

    val elicitation = new ElicitationRequester[F]:
      val supportsElicitation: Boolean = clientCaps.elicitation.isDefined

      def elicit(params: ElicitParams): F[ElicitResult] =
        tracer.span("mcp.elicitation.create").surround {
          if supportsElicitation then
            request[ElicitResult](McpMethod.ElicitationCreate, params.asJson)
          else F.raiseError(McpError.ElicitationNotSupported)
        }

    def progress(token: RequestId, value: Double, total: Option[Double]): F[Unit] =
      send(
        JsonRpcNotification(McpMethod.Progress, Some(ProgressParams(token, value, total).asJson))
      )

    // Honour logging/setLevel: messages below the client's chosen level are not sent.
    def log(level: LogLevel, message: String, data: Option[Json]): F[Unit] =
      dispatcher.logLevel.flatMap {
        case Some(min) if level.ordinal < min.ordinal => F.unit
        case _                                        =>
          send(
            JsonRpcNotification(
              McpMethod.LoggingMessage,
              Some(LogMessage(level, None, logPayload(message, data)).asJson)
            )
          )
      }

    ToolContext[F](sampling, elicitation, reqId, progressToken, progress, log)

  /** When both a message and structured data are given, the message is folded into the data payload
    * rather than dropped.
    */
  private def logPayload(message: String, data: Option[Json]): Json =
    data match
      case None                       => Json.fromString(message)
      case Some(d) if message.isEmpty => d
      case Some(d)                    =>
        d.asObject match
          case Some(obj) if !obj.contains("message") =>
            Json.fromJsonObject(("message" -> Json.fromString(message)) +: obj)
          case Some(_) => d
          case None    => Json.obj("message" -> Json.fromString(message), "data" -> d)
