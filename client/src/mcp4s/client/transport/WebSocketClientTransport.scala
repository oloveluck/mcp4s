package mcp4s.client.transport

import cats.effect.{Async, Deferred, Ref, Resource as CatsResource}
import cats.effect.std.Queue
import cats.effect.syntax.monadCancel.*
import cats.syntax.all.*
import fs2.{Pipe, Stream}
import fs2.concurrent.SignallingRef
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.httpclient.fs2.HttpClientFs2Backend
import sttp.client4.ws.stream.*
import sttp.ws.WebSocketFrame
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.{ClientDispatcher, McpClient, McpConnection, McpConnectionImpl}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given

/** WebSocket transport configuration for MCP clients */
final case class WebSocketClientConfig(
    url: String,
    path: String = "ws"
)

/** WebSocket transport for MCP clients.
  *
  * Connects to an MCP server via a single bidirectional WebSocket connection.
  * This provides lower latency than HTTP/SSE and simplifies connection management.
  */
object WebSocketClientTransport:

  /** Connect to a WebSocket MCP server.
    *
    * @param client The MCP client configuration
    * @param config WebSocket transport configuration
    * @param tracer OpenTelemetry tracer for distributed tracing
    */
  def connect[F[_]: Async](
      client: McpClient[F],
      config: WebSocketClientConfig
  )(using Tracer[F]): CatsResource[F, McpConnection[F]] =
    for
      backend <- HttpClientFs2Backend.resource[F]()
      clientDispatcher <- CatsResource.eval(ClientDispatcher[F](client))
      connection <- establishConnection(client, backend, clientDispatcher, config, summon[Tracer[F]])
    yield connection

  private def establishConnection[F[_]: Async](
      client: McpClient[F],
      backend: WebSocketStreamBackend[F, Fs2Streams[F]],
      clientDispatcher: ClientDispatcher[F],
      config: WebSocketClientConfig,
      tracer: Tracer[F]
  ): CatsResource[F, McpConnection[F]] =
    val wsUrl = s"${config.url}/${config.path}"

    for
      // Pending requests awaiting responses: RequestId -> Deferred for result
      pendingRequests <- CatsResource.eval(
        Ref.of[F, Map[RequestId, Deferred[F, Either[JsonRpcError, Json]]]](Map.empty)
      )
      // Queue for outgoing messages
      outQueue <- CatsResource.eval(Queue.unbounded[F, WebSocketFrame])
      // Request ID generator
      requestIdGen <- CatsResource.eval(Ref.of[F, Long](0L))
      // In-flight requests for cancellation support
      inFlightRef <- CatsResource.eval(Ref.of[F, Map[RequestId, Deferred[F, Unit]]](Map.empty))
      // Deferred to signal initialization complete and pass the connection
      connectionDeferred <- CatsResource.eval(Deferred[F, McpConnection[F]])
      // Signal to trigger initialization after streams are running
      initSignal <- CatsResource.eval(SignallingRef.of[F, Boolean](false))

      // Handle incoming messages
      handleIncoming = (text: String) =>
        decode[JsonRpcMessage](text) match
          case Right(response: JsonRpcResponse) =>
            pendingRequests.get.flatMap(_.get(response.id) match
              case Some(deferred) => deferred.complete(Right(response.result)).void
              case None           => Async[F].unit
            )

          case Right(errorResponse: JsonRpcErrorResponse) =>
            pendingRequests.get.flatMap(_.get(errorResponse.id) match
              case Some(deferred) => deferred.complete(Left(errorResponse.error)).void
              case None           => Async[F].unit
            )

          case Right(request: JsonRpcRequest) =>
            clientDispatcher.dispatch(request).flatMap {
              case Some(response) =>
                outQueue.offer(WebSocketFrame.text(response.asJson.noSpaces))
              case None => Async[F].unit
            }

          case Right(_: JsonRpcNotification) =>
            Async[F].unit

          case Left(_) =>
            Async[F].unit

      // Create the sendRequest function
      sendRequest = (req: JsonRpcRequest) =>
        for
          deferred <- Deferred[F, Either[JsonRpcError, Json]]
          _ <- pendingRequests.update(_ + (req.id -> deferred))
          _ <- outQueue.offer(WebSocketFrame.text(req.asJson.noSpaces))
          result <- deferred.get.guarantee(pendingRequests.update(_ - req.id))
          json <- result match
            case Right(json) => Async[F].pure(json)
            case Left(error) => Async[F].raiseError(McpError.fromJsonRpcError(error))
        yield json

      // Create the sendNotification function
      sendNotification = (notif: JsonRpcNotification) =>
        outQueue.offer(WebSocketFrame.text(notif.asJson.noSpaces))

      // Cleanup function - completes all pending requests with error when connection closes
      cleanupPendingRequests = pendingRequests.get.flatMap { pending =>
        val connectionClosedError = Left(JsonRpcError(-32000, "Connection closed", None))
        pending.values.toList.traverse_ { deferred =>
          deferred.complete(connectionClosedError).void
            .handleErrorWith(_ => Async[F].unit) // Ignore if already completed
        }
      }

      // Initialization logic - runs after streams are active
      doInit = for
        initId <- requestIdGen.getAndUpdate(_ + 1).map(n => RequestId.NumberId(n + 1))
        initRequest = JsonRpcRequest(
          initId,
          McpMethod.Initialize,
          Some(InitializeParams(
            protocolVersion = McpVersion.Current,
            capabilities = client.capabilities,
            clientInfo = client.info
          ).asJson)
        )
        initResult <- sendRequest(initRequest).flatMap { result =>
          result.as[InitializeResult].liftTo[F]
        }
        _ <- sendNotification(JsonRpcNotification(McpMethod.Initialized, None))
        conn = new McpConnectionImpl[F](
          initResult.serverInfo,
          initResult.capabilities,
          sendRequest,
          sendNotification,
          requestIdGen,
          inFlightRef,
          tracer
        )
        _ <- connectionDeferred.complete(conn)
      yield ()

      // WebSocket frame processing pipe
      wsPipe: Pipe[F, WebSocketFrame.Data[?], WebSocketFrame] = { incoming =>
        // Outgoing frames from queue
        val outgoing = Stream.fromQueueUnterminated(outQueue)

        // Process incoming frames
        val incomingProcessed = incoming.evalMap {
          case WebSocketFrame.Text(text, _, _) => handleIncoming(text)
          case _                               => Async[F].unit
        }.drain

        // Signal that streams are ready, then keep running
        val signalReady = Stream.eval(initSignal.set(true)).drain

        // Merge all streams: signal ready, then process in/out concurrently
        signalReady ++ outgoing.mergeHaltBoth(incomingProcessed)
      }

      // Start WebSocket connection in background with cleanup on termination
      _ <- CatsResource.make(
        Async[F].start {
          basicRequest
            .get(uri"$wsUrl")
            .response(asWebSocketStream(Fs2Streams[F])(wsPipe))
            .send(backend)
            .void
            .guarantee(cleanupPendingRequests) // Clean up pending requests on any termination
        }
      )(fiber => fiber.cancel)

      // Wait for streams to be ready, then initialize (with error handling)
      _ <- CatsResource.eval {
        initSignal.waitUntil(identity) *> doInit.onError { _ =>
          // On init failure, cleanup so pending requests don't hang
          cleanupPendingRequests
        }
      }

      // Get the connection
      connection <- CatsResource.eval(connectionDeferred.get)
    yield connection
