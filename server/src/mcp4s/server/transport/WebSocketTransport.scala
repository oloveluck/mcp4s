package mcp4s.server.transport

import cats.effect.{Async, Deferred, Ref, Resource as CatsResource}
import cats.effect.std.Queue
import cats.effect.syntax.monadCancel.*
import cats.syntax.all.*
import com.comcast.ip4s.{Host, Port, host, port}
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
    requestTimeout: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.FiniteDuration(5, "min")
)

object WebSocketConfig:
  val default: WebSocketConfig = WebSocketConfig()

/** WebSocket transport for MCP servers.
  *
  * Provides bidirectional JSON-RPC communication over a single WebSocket connection.
  * This is an alternative to HTTP/SSE transport, offering lower latency and simpler
  * connection management for scenarios where both client and server need to send
  * messages asynchronously.
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
    * Use this to embed MCP WebSocket routes in an existing http4s application and
    * compose standard http4s middleware (CORS, auth, etc.) yourself.
    *
    * @param server The MCP server to serve
    * @param wsb WebSocket builder from http4s
    * @param config WebSocket configuration
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
    * @param server The MCP server to serve
    * @param config WebSocket configuration
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
      config: WebSocketConfig = WebSocketConfig.default
  ): F[Response[F]] =
    // Create session state for this connection
    WebSocketSession[F](server, tracer, config).flatMap { session =>
      // Queue for outgoing messages (bounded with backpressure)
      Queue.bounded[F, WebSocketFrame](config.maxQueueSize).flatMap { outQueue =>
        // Give session access to the outQueue for sending requests
        session.setOutQueue(outQueue) *> {
          // Process incoming frames and dispatch to handler, with cleanup on disconnect
          val receive: Pipe[F, WebSocketFrame, Unit] = _.evalMap {
            case WebSocketFrame.Text(text, _) =>
              tracer.span("ws.message.receive").surround {
                session.handleMessage(text, outQueue)
              }

            case WebSocketFrame.Close(_) =>
              Async[F].unit

            case WebSocketFrame.Ping(data) =>
              outQueue.offer(WebSocketFrame.Pong(data))

            case _ =>
              Async[F].unit
          }.onFinalize(session.cleanup)

          // Stream of outgoing frames from the queue
          val send: Stream[F, WebSocketFrame] = Stream.fromQueueUnterminated(outQueue)

          wsb.build(send, receive)
        }
      }
    }

/** Manages state for a single WebSocket connection.
  *
  * Handles:
  * - Request dispatching to the MCP server
  * - Server-to-client request correlation (for sampling)
  * - Client capability tracking
  */
private class WebSocketSession[F[_]: Async](
    dispatcherRef: Ref[F, Option[Dispatcher[F]]],
    requestIdGen: Ref[F, Long],
    pendingRequests: Ref[F, Map[RequestId, Deferred[F, Either[JsonRpcError, Json]]]],
    clientCapsRef: Ref[F, Option[ClientCapabilities]],
    outQueueRef: Ref[F, Option[Queue[F, WebSocketFrame]]],
    tracer: Tracer[F],
    config: WebSocketConfig
):

  /** Clean up session resources on disconnect.
    * Completes all pending deferreds with connection-closed errors and clears refs.
    */
  def cleanup: F[Unit] =
    val connectionClosedError = Left(JsonRpcError(-32000, "Connection closed", None))
    pendingRequests.getAndSet(Map.empty).flatMap { pending =>
      pending.values.toList.traverse_ { deferred =>
        deferred.complete(connectionClosedError).void
          .handleErrorWith(_ => Async[F].unit) // Ignore if already completed
      }
    } *> outQueueRef.set(None) *> clientCapsRef.set(None)

  /** Set the dispatcher (used during initialization) */
  private[transport] def setDispatcher(d: Dispatcher[F]): F[Unit] =
    dispatcherRef.set(Some(d))

  /** Set the output queue for sending messages */
  def setOutQueue(queue: Queue[F, WebSocketFrame]): F[Unit] =
    outQueueRef.set(Some(queue))

  /** Handle an incoming WebSocket message */
  def handleMessage(text: String, outQueue: Queue[F, WebSocketFrame]): F[Unit] =
    decode[JsonRpcMessage](text) match
      case Right(message) => message match
        case resp: JsonRpcResponse =>
          // Response to a server-initiated request (e.g., sampling)
          handleResponse(resp)

        case errResp: JsonRpcErrorResponse =>
          // Error response to a server-initiated request
          handleErrorResponse(errResp)

        case req: JsonRpcRequest =>
          // Client request - dispatch and maybe capture client capabilities
          dispatcherRef.get.flatMap {
            case Some(dispatcher) =>
              maybeExtractClientCaps(req) *>
                dispatcher.dispatch(req).flatMap {
                  case Some(response) =>
                    outQueue.offer(WebSocketFrame.Text(response.asJson.noSpaces))
                  case None =>
                    Async[F].unit
                }
            case None =>
              // Dispatcher not yet initialized - this shouldn't happen
              val error = JsonRpcErrorResponse(req.id, JsonRpcError.internalError("Server not ready"))
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

  /** SamplingRequester for this session (cached instance) */
  val samplingRequester: SamplingRequester[F] =
    new SamplingRequester[F]:
      def supportsSampling: Boolean = true  // Actual check happens in createMessage

      def createMessage(params: CreateMessageParams): F[CreateMessageResult] =
        tracer.span("mcp.sampling.createMessage").surround {
          clientCapsRef.get.flatMap {
            case Some(caps) if caps.sampling.isDefined =>
              sendRequest[CreateMessageResult](McpMethod.SamplingCreateMessage, params.asJson)
            case _ =>
              Async[F].raiseError(McpError.SamplingNotSupported())
          }
        }

  /** ElicitationRequester for this session (cached instance) */
  val elicitationRequester: ElicitationRequester[F] =
    new ElicitationRequester[F]:
      def supportsElicitation: Boolean = true  // Actual check happens in elicit

      def elicit(params: ElicitParams): F[ElicitResult] =
        tracer.span("mcp.elicitation.create").surround {
          clientCapsRef.get.flatMap {
            case Some(caps) if caps.elicitation.isDefined =>
              sendRequest[ElicitResult](McpMethod.ElicitationCreate, params.asJson)
            case _ =>
              Async[F].raiseError(McpError.ElicitationNotSupported())
          }
        }

  /** Send a progress notification */
  private def sendProgressNotification(token: RequestId, prog: Double, total: Option[Double]): F[Unit] =
    val notification = JsonRpcNotification(
      McpMethod.Progress,
      Some(ProgressParams(token, prog, total).asJson)
    )
    sendNotification(notification)

  /** Send a logging notification */
  private def sendLoggingNotification(level: LogLevel, message: String, data: Option[Json]): F[Unit] =
    val notification = JsonRpcNotification(
      McpMethod.LoggingMessage,
      Some(LogMessage(level, None, data.getOrElse(Json.fromString(message))).asJson)
    )
    sendNotification(notification)

  /** Send a notification through the WebSocket */
  private def sendNotification(notification: JsonRpcNotification): F[Unit] =
    outQueueRef.get.flatMap {
      case Some(queue) => queue.offer(WebSocketFrame.Text(notification.asJson.noSpaces))
      case None => Async[F].unit
    }

  private def sendRequest[A: Decoder](method: String, params: Json): F[A] =
    for
      reqId <- nextRequestId
      deferred <- Deferred[F, Either[JsonRpcError, Json]]
      _ <- pendingRequests.update(_ + (reqId -> deferred))
      request = JsonRpcRequest(reqId, method, Some(params))
      queueOpt <- outQueueRef.get
      _ <- queueOpt match
        case Some(queue) =>
          queue.offer(WebSocketFrame.Text(request.asJson.noSpaces))
        case None =>
          Async[F].raiseError(new RuntimeException("WebSocket not connected"))
      // Apply configured request timeout
      result <- Async[F].race(
        Async[F].sleep(config.requestTimeout),
        deferred.get
      ).guarantee(pendingRequests.update(_ - reqId))
      decoded <- result match
        case Left(_) =>
          Async[F].raiseError(McpError.InternalError(s"Request timed out after ${config.requestTimeout}"))
        case Right(Right(json)) => json.as[A].liftTo[F]
        case Right(Left(err))   => Async[F].raiseError(McpError.fromJsonRpcError(err))
    yield decoded

  private def nextRequestId: F[RequestId] =
    requestIdGen.getAndUpdate(_ + 1).map(n => RequestId.NumberId(n + 1))

  private def handleResponse(resp: JsonRpcResponse): F[Unit] =
    pendingRequests.modify { map =>
      map.get(resp.id) match
        case Some(deferred) => (map - resp.id, deferred.complete(Right(resp.result)).void)
        case None           => (map, Async[F].unit)
    }.flatten

  private def handleErrorResponse(resp: JsonRpcErrorResponse): F[Unit] =
    pendingRequests.modify { map =>
      map.get(resp.id) match
        case Some(deferred) => (map - resp.id, deferred.complete(Left(resp.error)).void)
        case None           => (map, Async[F].unit)
    }.flatten

  /** Extract client capabilities from initialize request */
  private def maybeExtractClientCaps(req: JsonRpcRequest): F[Unit] =
    if req.method == McpMethod.Initialize then
      req.params.flatMap(_.as[InitializeParams].toOption) match
        case Some(params) => clientCapsRef.set(Some(params.capabilities))
        case None         => Async[F].unit
    else
      Async[F].unit

private object WebSocketSession:
  def apply[F[_]: Async](server: Server[F], tracer: Tracer[F], config: WebSocketConfig = WebSocketConfig.default): F[WebSocketSession[F]] =
    given Tracer[F] = tracer
    for
      dispatcherRef <- Ref.of[F, Option[Dispatcher[F]]](None)
      requestIdGen <- Ref.of[F, Long](0L)
      pendingRequests <- Ref.of[F, Map[RequestId, Deferred[F, Either[JsonRpcError, Json]]]](Map.empty)
      clientCapsRef <- Ref.of[F, Option[ClientCapabilities]](None)
      outQueueRef <- Ref.of[F, Option[Queue[F, WebSocketFrame]]](None)
      // Create session
      session = new WebSocketSession(dispatcherRef, requestIdGen, pendingRequests, clientCapsRef, outQueueRef, tracer, config)
      // Create context factory with full capabilities: sampling, elicitation, progress, logging
      contextFactory = (reqId: RequestId, progressToken: Option[RequestId]) => ToolContext[F](
        session.samplingRequester,
        session.elicitationRequester,
        reqId,
        progressToken,
        session.sendProgressNotification,
        session.sendLoggingNotification
      )
      // Create dispatcher with context factory
      dispatcher <- Dispatcher.withContext[F](server, contextFactory)
      // Set the dispatcher
      _ <- session.setDispatcher(dispatcher)
    yield session
