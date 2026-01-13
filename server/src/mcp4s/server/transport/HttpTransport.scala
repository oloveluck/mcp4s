package mcp4s.server.transport

import cats.effect.{Async, Resource as CatsResource}
import fs2.io.net.Network
import cats.syntax.all.*
import com.comcast.ip4s.{Host, Port, host, port}
import fs2.Stream
import fs2.concurrent.Topic
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.`Content-Type`
import org.http4s.server.{Router, Server}
import org.http4s.server.middleware.CORS
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*

/** HTTP/SSE transport configuration */
final case class HttpConfig(
    host: Host = host"0.0.0.0",
    port: Port = port"3000",
    enableCors: Boolean = true
)

object HttpConfig:
  val default: HttpConfig = HttpConfig()

/** HTTP/SSE transport for MCP servers.
  *
  * Provides two endpoints:
  *   - POST /message: Receives JSON-RPC requests, returns JSON-RPC responses
  *   - GET /sse: Server-Sent Events stream for server-to-client notifications
  */
object HttpTransport:

  /** Start an HTTP server for the given MCP server */
  def serve[F[_]: Async: Network](
      server: McpServer[F],
      config: HttpConfig = HttpConfig.default
  ): CatsResource[F, Server] =
    for
      dispatcher <- CatsResource.eval(mcp4s.server.Dispatcher[F](server))
      sseTopic <- CatsResource.eval(Topic[F, JsonRpcMessage])
      routes = createRoutes(dispatcher, sseTopic)
      corsRoutes = if config.enableCors then CORS.policy.withAllowOriginAll(routes) else routes
      httpServer <- EmberServerBuilder
        .default[F]
        .withHost(config.host)
        .withPort(config.port)
        .withHttpApp(Router("/" -> corsRoutes).orNotFound)
        .build
    yield httpServer

  private def createRoutes[F[_]: Async](
      dispatcher: mcp4s.server.Dispatcher[F],
      sseTopic: Topic[F, JsonRpcMessage]
  ): HttpRoutes[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    HttpRoutes.of[F] {
      case req @ POST -> Root / "message" =>
        req.as[Json].flatMap { json =>
          decode[JsonRpcMessage](json.noSpaces) match
            case Right(message) =>
              dispatcher.dispatch(message).flatMap {
                case Some(response) =>
                  Ok(response.asJson, `Content-Type`(MediaType.application.json))
                case None =>
                  NoContent()
              }
            case Left(err) =>
              val error = JsonRpcErrorResponse(
                RequestId.NullId,
                JsonRpcError.parseError(err.getMessage)
              )
              Ok(error.asJson, `Content-Type`(MediaType.application.json))
        }

      case GET -> Root / "sse" =>
        val sseStream: Stream[F, ServerSentEvent] =
          sseTopic.subscribe(100).map { message =>
            ServerSentEvent(data = Some(message.asJson.noSpaces))
          }

        Ok(sseStream)

      case GET -> Root / "health" =>
        Ok(Json.obj("status" -> Json.fromString("ok")))
    }
