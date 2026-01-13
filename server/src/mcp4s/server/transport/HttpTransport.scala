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
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.context.propagation.TextMapGetter
import org.typelevel.otel4s.trace.Tracer
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

  /** TextMapGetter for extracting trace context from HTTP headers */
  private given TextMapGetter[Headers] with
    def get(carrier: Headers, key: String): Option[String] =
      carrier.get(CIString(key)).map(_.head.value)
    def keys(carrier: Headers): Iterable[String] =
      carrier.headers.map(_.name.toString)

  /** Start an HTTP server for the given MCP server.
    *
    * @param server The MCP server to serve
    * @param config HTTP configuration
    * @param tracer Optional OpenTelemetry tracer for distributed tracing (defaults to noop)
    */
  def serve[F[_]: Async: Network](
      server: McpServer[F],
      config: HttpConfig = HttpConfig.default
  )(using Tracer[F]): CatsResource[F, Server] =
    for
      dispatcher <- CatsResource.eval(mcp4s.server.Dispatcher[F](server))
      sseTopic <- CatsResource.eval(Topic[F, JsonRpcMessage])
      routes = createRoutes(dispatcher, sseTopic, summon[Tracer[F]])
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
      sseTopic: Topic[F, JsonRpcMessage],
      tracer: Tracer[F]
  ): HttpRoutes[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    HttpRoutes.of[F] {
      case req @ POST -> Root / "message" =>
        // Extract trace context from incoming request headers for distributed tracing
        tracer.joinOrRoot(req.headers) {
          tracer.span("http.server.request", Attribute("http.method", "POST"), Attribute("http.route", "/message"))
            .use { span =>
              req.as[Json].flatMap { json =>
                decode[JsonRpcMessage](json.noSpaces) match
                  case Right(message) =>
                    dispatcher.dispatch(message).flatMap {
                      case Some(response) =>
                        span.addAttribute(Attribute("http.status_code", 200L)) *>
                          Ok(response.asJson, `Content-Type`(MediaType.application.json))
                      case None =>
                        span.addAttribute(Attribute("http.status_code", 204L)) *>
                          NoContent()
                    }
                  case Left(err) =>
                    val error = JsonRpcErrorResponse(
                      RequestId.NullId,
                      JsonRpcError.parseError(err.getMessage)
                    )
                    span.addAttribute(Attribute("http.status_code", 200L)) *>
                      Ok(error.asJson, `Content-Type`(MediaType.application.json))
              }
            }
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
