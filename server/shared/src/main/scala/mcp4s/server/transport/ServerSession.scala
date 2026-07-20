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

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import io.circe.{Decoder, Json}
import io.circe.syntax.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.RequestCorrelator
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.{ElicitationRequester, SamplingRequester}

import scala.concurrent.duration.FiniteDuration

/** The transport-independent half of a server-side session.
  *
  * Both duplex server transports (HTTP + SSE, WebSocket) need identical machinery for
  * server-initiated requests: correlate request/response pairs, track the client's advertised
  * capabilities, and expose [[SamplingRequester]]/[[ElicitationRequester]] that gate on those
  * capabilities. This class captures that once; transports supply only the outbound delivery
  * function (SSE queue offer vs. WebSocket frame offer).
  */
final private[transport] class ServerSession[F[_]](
    correlator: RequestCorrelator[F],
    clientCapsRef: Ref[F, Option[ClientCapabilities]],
    sendMessage: JsonRpcMessage => F[Unit],
    requestTimeout: FiniteDuration,
    tracer: Tracer[F]
)(using F: Async[F]):

  /** Fail all pending server-initiated requests (e.g. on disconnect or session expiry). */
  def cancelPending: F[Unit] =
    correlator.cancelAll(JsonRpcError(-32000, "Connection closed", None))

  /** Number of server-initiated requests awaiting a client response. */
  def pendingRequestCount: F[Int] = correlator.inFlightCount

  /** Record the client capabilities if `message` is an initialize request. */
  def maybeExtractClientCaps(message: JsonRpcMessage): F[Unit] =
    message match
      case req: JsonRpcRequest if req.method == McpMethod.Initialize =>
        req.params.flatMap(_.as[InitializeParams].toOption) match
          case Some(params) => clientCapsRef.set(Some(params.capabilities))
          case None         => F.unit
      case _ => F.unit

  /** Route a client response to the matching pending server-initiated request. */
  def handleResponse(resp: JsonRpcResponse): F[Unit] =
    correlator.complete(resp.id, resp.result)

  /** Route a client error response to the matching pending server-initiated request. */
  def handleErrorResponse(resp: JsonRpcErrorResponse): F[Unit] =
    correlator.fail(resp.id, resp.error)

  /** Send a request to the client and await its response (with the configured timeout). */
  def sendRequest[A: Decoder](method: String, params: Json): F[A] =
    for
      reqId <- correlator.nextId
      json <- correlator.request(reqId, requestTimeout):
        sendMessage(JsonRpcRequest(reqId, method, Some(params)))
      decoded <- json.as[A].liftTo[F]
    yield decoded

  /** Send a notification to the client. */
  def sendNotification(notification: JsonRpcNotification): F[Unit] =
    sendMessage(notification)

  /** Send a progress notification. */
  def sendProgressNotification(token: RequestId, prog: Double, total: Option[Double]): F[Unit] =
    sendNotification(
      JsonRpcNotification(McpMethod.Progress, Some(ProgressParams(token, prog, total).asJson))
    )

  /** Send a logging notification. When both a message and structured data are given, the message is
    * folded into the data payload rather than dropped.
    */
  def sendLoggingNotification(level: LogLevel, message: String, data: Option[Json]): F[Unit] =
    val payload = data match
      case None                       => Json.fromString(message)
      case Some(d) if message.isEmpty => d
      case Some(d) =>
        d.asObject match
          case Some(obj) if !obj.contains("message") =>
            Json.fromJsonObject(("message" -> Json.fromString(message)) +: obj)
          case Some(_) => d
          case None    => Json.obj("message" -> Json.fromString(message), "data" -> d)
    sendNotification(
      JsonRpcNotification(
        McpMethod.LoggingMessage,
        Some(LogMessage(level, None, payload).asJson)
      )
    )

  /** SamplingRequester for this session; verifies the client advertised sampling. */
  val samplingRequester: SamplingRequester[F] =
    new SamplingRequester[F]:
      def supportsSampling: Boolean = true // Actual check happens in createMessage

      def createMessage(params: CreateMessageParams): F[CreateMessageResult] =
        tracer.span("mcp.sampling.createMessage").surround {
          clientCapsRef.get.flatMap {
            case Some(caps) if caps.sampling.isDefined =>
              sendRequest[CreateMessageResult](McpMethod.SamplingCreateMessage, params.asJson)
            case _ =>
              F.raiseError(McpError.SamplingNotSupported)
          }
        }

  /** ElicitationRequester for this session; verifies the client advertised elicitation. */
  val elicitationRequester: ElicitationRequester[F] =
    new ElicitationRequester[F]:
      def supportsElicitation: Boolean = true // Actual check happens in elicit

      def elicit(params: ElicitParams): F[ElicitResult] =
        tracer.span("mcp.elicitation.create").surround {
          clientCapsRef.get.flatMap {
            case Some(caps) if caps.elicitation.isDefined =>
              sendRequest[ElicitResult](McpMethod.ElicitationCreate, params.asJson)
            case _ =>
              F.raiseError(McpError.ElicitationNotSupported)
          }
        }

private[transport] object ServerSession:
  def create[F[_]: Async](
      sendMessage: JsonRpcMessage => F[Unit],
      requestTimeout: FiniteDuration,
      tracer: Tracer[F]
  ): F[ServerSession[F]] =
    for
      correlator    <- RequestCorrelator[F]
      clientCapsRef <- Ref.of[F, Option[ClientCapabilities]](None)
    yield new ServerSession(correlator, clientCapsRef, sendMessage, requestTimeout, tracer)
