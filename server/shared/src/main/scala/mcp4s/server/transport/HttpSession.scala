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
import cats.effect.std.{Queue, SecureRandom, UUIDGen}
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.RequestCorrelator
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*

import scala.concurrent.duration.FiniteDuration

/** Represents an HTTP session for MCP Streamable HTTP transport.
  *
  * Each session has its own Dispatcher instance, providing isolated state for initialization and
  * request handling per client.
  *
  * Supports bidirectional communication via SSE streams:
  *   - Server can send progress/logging notifications during tool execution
  *   - Server can send sampling/elicitation requests and await client responses
  */
final class HttpSession[F[_]] private (
    val id: String,
    val dispatcher: Dispatcher[F],
    val createdAt: FiniteDuration,
    lastAccessedRef: Ref[F, FiniteDuration],
    val outQueue: Queue[F, JsonRpcMessage],
    correlator: RequestCorrelator[F],
    val clientCapsRef: Ref[F, Option[ClientCapabilities]],
    val config: SessionConfig,
    tracer: Tracer[F]
)(using F: Async[F]):

  /** Get the last access time for this session, as a duration since the epoch */
  def lastAccessed: F[FiniteDuration] = lastAccessedRef.get

  /** Update the last access time to now */
  def touch: F[Unit] = F.realTime.flatMap(lastAccessedRef.set)

  /** Check if this session has expired based on the configured timeout */
  def isExpired: F[Boolean] =
    for
      now  <- F.realTime
      last <- lastAccessedRef.get
    yield (now - last) > config.timeout

  /** Shutdown this session, completing any pending requests with an error */
  def shutdown: F[Unit] =
    correlator.cancelAll(JsonRpcError(-32000, "Session closed", None))

  /** Get the number of pending requests awaiting responses */
  def pendingRequestCount: F[Int] = correlator.inFlightCount

  /** Get the current queue size */
  def queueSize: F[Int] = outQueue.size

  /** Extract client capabilities from initialize request */
  def maybeExtractClientCaps(message: JsonRpcMessage): F[Unit] =
    message match
      case req: JsonRpcRequest if req.method == McpMethod.Initialize =>
        req.params.flatMap(_.as[InitializeParams].toOption) match
          case Some(params) => clientCapsRef.set(Some(params.capabilities))
          case None         => F.unit
      case _ => F.unit

  /** Handle a response from the client to a server-initiated request */
  def handleResponse(resp: JsonRpcResponse): F[Unit] =
    correlator.complete(resp.id, resp.result)

  /** Handle an error response from the client to a server-initiated request */
  def handleErrorResponse(resp: JsonRpcErrorResponse): F[Unit] =
    correlator.fail(resp.id, resp.error)

  /** Send a request to the client and await the response. The request will timeout after the
    * configured requestTimeout duration.
    */
  private[transport] def sendRequest[A: Decoder](method: String, params: Json): F[A] =
    for
      reqId <- correlator.nextId
      json <- correlator.request(reqId, config.requestTimeout):
        outQueue.offer(JsonRpcRequest(reqId, method, Some(params)))
      decoded <- json.as[A].liftTo[F]
    yield decoded

  /** Send a notification to the client */
  private[transport] def sendNotification(notification: JsonRpcNotification): F[Unit] =
    outQueue.offer(notification)

  /** Send a progress notification */
  private[transport] def sendProgressNotification(
      token: RequestId,
      prog: Double,
      total: Option[Double]
  ): F[Unit] =
    val notification = JsonRpcNotification(
      McpMethod.Progress,
      Some(ProgressParams(token, prog, total).asJson)
    )
    sendNotification(notification)

  /** Send a logging notification */
  private[transport] def sendLoggingNotification(
      level: LogLevel,
      message: String,
      data: Option[Json]
  ): F[Unit] =
    val notification = JsonRpcNotification(
      McpMethod.LoggingMessage,
      Some(LogMessage(level, None, data.getOrElse(Json.fromString(message))).asJson)
    )
    sendNotification(notification)

  /** SamplingRequester for this session */
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

  /** ElicitationRequester for this session */
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

object HttpSession:

  /** Create a new HTTP session with a fresh Dispatcher and full context support.
    *
    * @param server
    *   The MCP server to create a dispatcher for
    * @param config
    *   Session configuration (optional, defaults to SessionConfig.default)
    * @param tracer
    *   OpenTelemetry tracer for distributed tracing
    */
  def create[F[_]: Async](
      server: Server[F],
      config: SessionConfig = SessionConfig.default
  )(using tracer: Tracer[F]): F[HttpSession[F]] =
    for
      id <- SecureRandom
        .javaSecuritySecureRandom[F]
        .flatMap { implicit sr =>
          UUIDGen[F].randomUUID
        }
        .map(_.toString)
      // Use bounded queue with configurable max size for backpressure
      outQueue        <- Queue.bounded[F, JsonRpcMessage](config.maxQueueSize)
      correlator      <- RequestCorrelator[F]
      clientCapsRef   <- Ref.of[F, Option[ClientCapabilities]](None)
      now             <- Async[F].realTime
      lastAccessedRef <- Ref.of[F, FiniteDuration](now)

      // Create a partial session with a placeholder dispatcher
      // We need the session reference to create the context factory
      placeholderDispatcher <- Dispatcher[F](server)
      session = new HttpSession[F](
        id,
        placeholderDispatcher,
        now,
        lastAccessedRef,
        outQueue,
        correlator,
        clientCapsRef,
        config,
        tracer
      )

      // Create context factory with full capabilities
      // progressToken from _meta is used for progress notifications when provided
      contextFactory = (reqId: RequestId, progressToken: Option[RequestId]) =>
        ToolContext[F](
          session.samplingRequester,
          session.elicitationRequester,
          reqId,
          progressToken,
          session.sendProgressNotification,
          session.sendLoggingNotification
        )

      // Create the real dispatcher with context factory
      realDispatcher <- Dispatcher.withContext[F](server, contextFactory)
    yield new HttpSession[F](
      id,
      realDispatcher,
      now,
      lastAccessedRef,
      outQueue,
      correlator,
      clientCapsRef,
      config,
      tracer
    )
