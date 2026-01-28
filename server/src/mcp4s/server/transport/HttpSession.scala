package mcp4s.server.transport

import cats.effect.{Async, Deferred, Ref}
import cats.effect.std.Queue
import cats.effect.syntax.monadCancel.*
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*

import java.time.Instant
import java.util.UUID

/** Represents an HTTP session for MCP Streamable HTTP transport.
  *
  * Each session has its own Dispatcher instance, providing isolated state
  * for initialization and request handling per client.
  *
  * Supports bidirectional communication via SSE streams:
  * - Server can send progress/logging notifications during tool execution
  * - Server can send sampling/elicitation requests and await client responses
  */
final class HttpSession[F[_]] private (
    val id: String,
    val dispatcher: Dispatcher[F],
    val createdAt: Instant,
    lastAccessedRef: Ref[F, Instant],
    val outQueue: Queue[F, JsonRpcMessage],
    val pendingRequests: Ref[F, Map[RequestId, Deferred[F, Either[JsonRpcError, Json]]]],
    val clientCapsRef: Ref[F, Option[ClientCapabilities]],
    val requestIdGen: Ref[F, Long],
    tracer: Tracer[F]
)(using F: Async[F]):

  /** Get the last access time for this session */
  def lastAccessed: F[Instant] = lastAccessedRef.get

  /** Update the last access time to now */
  def touch: F[Unit] = F.realTimeInstant.flatMap(lastAccessedRef.set)

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
    pendingRequests.modify { map =>
      map.get(resp.id) match
        case Some(deferred) => (map - resp.id, deferred.complete(Right(resp.result)).void)
        case None           => (map, F.unit)
    }.flatten

  /** Handle an error response from the client to a server-initiated request */
  def handleErrorResponse(resp: JsonRpcErrorResponse): F[Unit] =
    pendingRequests.modify { map =>
      map.get(resp.id) match
        case Some(deferred) => (map - resp.id, deferred.complete(Left(resp.error)).void)
        case None           => (map, F.unit)
    }.flatten

  /** Send a request to the client and await the response */
  private[transport] def sendRequest[A: Decoder](method: String, params: Json): F[A] =
    for
      reqId <- nextRequestId
      deferred <- Deferred[F, Either[JsonRpcError, Json]]
      _ <- pendingRequests.update(_ + (reqId -> deferred))
      request = JsonRpcRequest(reqId, method, Some(params))
      _ <- outQueue.offer(request)
      result <- deferred.get.guarantee(pendingRequests.update(_ - reqId))
      decoded <- result match
        case Right(json) => json.as[A].liftTo[F]
        case Left(err)   => F.raiseError(McpError.fromJsonRpcError(err))
    yield decoded

  /** Send a notification to the client */
  private[transport] def sendNotification(notification: JsonRpcNotification): F[Unit] =
    outQueue.offer(notification)

  /** Send a progress notification */
  private[transport] def sendProgressNotification(token: RequestId, prog: Double, total: Option[Double]): F[Unit] =
    val notification = JsonRpcNotification(
      McpMethod.Progress,
      Some(ProgressParams(token, prog, total).asJson)
    )
    sendNotification(notification)

  /** Send a logging notification */
  private[transport] def sendLoggingNotification(level: LogLevel, message: String, data: Option[Json]): F[Unit] =
    val notification = JsonRpcNotification(
      McpMethod.LoggingMessage,
      Some(LogMessage(level, None, data.getOrElse(Json.fromString(message))).asJson)
    )
    sendNotification(notification)

  private def nextRequestId: F[RequestId] =
    requestIdGen.getAndUpdate(_ + 1).map(n => RequestId.NumberId(n + 1))

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
              F.raiseError(McpError.SamplingNotSupported())
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
              F.raiseError(McpError.ElicitationNotSupported())
          }
        }

object HttpSession:

  /** Create a new HTTP session with a fresh Dispatcher and full context support.
    *
    * @param server The MCP server to create a dispatcher for
    * @param tracer OpenTelemetry tracer for distributed tracing
    */
  def create[F[_]: Async](server: McpServer[F])(using tracer: Tracer[F]): F[HttpSession[F]] =
    for
      id <- Async[F].delay(UUID.randomUUID().toString)
      outQueue <- Queue.unbounded[F, JsonRpcMessage]
      pendingRequests <- Ref.of[F, Map[RequestId, Deferred[F, Either[JsonRpcError, Json]]]](Map.empty)
      clientCapsRef <- Ref.of[F, Option[ClientCapabilities]](None)
      requestIdGen <- Ref.of[F, Long](0L)
      now <- Async[F].realTimeInstant
      lastAccessedRef <- Ref.of[F, Instant](now)

      // Create a partial session with a placeholder dispatcher
      // We need the session reference to create the context factory
      placeholderDispatcher <- Dispatcher[F](server)
      session = new HttpSession[F](
        id,
        placeholderDispatcher,
        now,
        lastAccessedRef,
        outQueue,
        pendingRequests,
        clientCapsRef,
        requestIdGen,
        tracer
      )

      // Create context factory with full capabilities
      contextFactory = (reqId: RequestId) => ToolContext[F](
        session.samplingRequester,
        session.elicitationRequester,
        reqId,
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
      pendingRequests,
      clientCapsRef,
      requestIdGen,
      tracer
    )
