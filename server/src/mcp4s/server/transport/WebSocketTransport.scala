package mcp4s.server.transport

import cats.effect.{Async, Resource as CatsResource}
import cats.effect.std.Queue
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
import org.http4s.server.{Router, Server}
import org.http4s.server.middleware.CORS
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*

/** WebSocket transport configuration */
final case class WebSocketConfig(
    host: Host = host"0.0.0.0",
    port: Port = port"3000",
    enableCors: Boolean = true,
    path: String = "ws"
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
  * Endpoints:
  *   - GET /ws: WebSocket upgrade endpoint for bidirectional JSON-RPC
  *   - GET /health: Health check endpoint
  */
object WebSocketTransport:

  /** Start a WebSocket server for the given MCP server.
    *
    * @param server The MCP server to serve
    * @param config WebSocket configuration
    * @param tracer OpenTelemetry tracer for distributed tracing
    */
  def serve[F[_]: Async: Network](
      server: McpServer[F],
      config: WebSocketConfig = WebSocketConfig.default
  )(using Tracer[F]): CatsResource[F, Server] =
    EmberServerBuilder
      .default[F]
      .withHost(config.host)
      .withPort(config.port)
      .withHttpWebSocketApp(wsb => createApp(server, wsb, config, summon[Tracer[F]]))
      .build

  private def createApp[F[_]: Async](
      server: McpServer[F],
      wsb: WebSocketBuilder2[F],
      config: WebSocketConfig,
      tracer: Tracer[F]
  ): HttpApp[F] =
    val routes = createRoutes(server, wsb, config, tracer)
    val corsRoutes = if config.enableCors then CORS.policy.withAllowOriginAll(routes) else routes
    Router("/" -> corsRoutes).orNotFound

  private def createRoutes[F[_]: Async](
      server: McpServer[F],
      wsb: WebSocketBuilder2[F],
      config: WebSocketConfig,
      tracer: Tracer[F]
  ): HttpRoutes[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    HttpRoutes.of[F] {
      case GET -> Root / config.path =>
        createWebSocket(server, wsb, tracer)

      case GET -> Root / "health" =>
        Ok(Json.obj("status" -> Json.fromString("ok")))
    }

  private def createWebSocket[F[_]: Async](
      server: McpServer[F],
      wsb: WebSocketBuilder2[F],
      tracer: Tracer[F]
  ): F[Response[F]] =
    // Create a new Dispatcher for this connection (isolated state per connection)
    given Tracer[F] = tracer
    mcp4s.server.Dispatcher[F](server).flatMap { dispatcher =>
      // Queue for outgoing messages (server-initiated notifications)
      Queue.unbounded[F, WebSocketFrame].flatMap { outQueue =>
        // Process incoming frames and dispatch to handler
        val receive: Pipe[F, WebSocketFrame, Unit] = _.evalMap {
          case WebSocketFrame.Text(text, _) =>
            tracer.span("ws.message.receive").surround {
              parseAndDispatch(text, dispatcher, outQueue)
            }

          case WebSocketFrame.Close(_) =>
            Async[F].unit

          case WebSocketFrame.Ping(data) =>
            outQueue.offer(WebSocketFrame.Pong(data))

          case _ =>
            Async[F].unit
        }

        // Stream of outgoing frames from the queue
        val send: Stream[F, WebSocketFrame] = Stream.fromQueueUnterminated(outQueue)

        wsb.build(send, receive)
      }
    }

  private def parseAndDispatch[F[_]: Async](
      text: String,
      dispatcher: mcp4s.server.Dispatcher[F],
      outQueue: Queue[F, WebSocketFrame]
  ): F[Unit] =
    decode[JsonRpcMessage](text) match
      case Right(message) =>
        dispatcher.dispatch(message).flatMap {
          case Some(response) =>
            val frame = WebSocketFrame.Text(response.asJson.noSpaces)
            outQueue.offer(frame)
          case None =>
            Async[F].unit // Notifications don't require response
        }

      case Left(err) =>
        // Send parse error response
        val error = JsonRpcErrorResponse(
          RequestId.NullId,
          JsonRpcError.parseError(err.getMessage)
        )
        val frame = WebSocketFrame.Text(error.asJson.noSpaces)
        outQueue.offer(frame)
