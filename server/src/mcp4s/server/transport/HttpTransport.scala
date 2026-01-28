package mcp4s.server.transport

import cats.effect.{Async, Resource as CatsResource}
import fs2.io.net.Network
import cats.syntax.all.*
import com.comcast.ip4s.{Host, Port, host, port}
import io.circe.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.{Allow, `Content-Type`}
import org.http4s.server.{Router, Server}
import org.http4s.server.middleware.CORS
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.context.propagation.TextMapGetter
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.vault.Key
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*
import mcp4s.server.auth.{AuthConfig, AuthMiddleware}

/** Streamable HTTP transport configuration */
final case class HttpConfig[F[_]](
    host: Host = host"0.0.0.0",
    port: Port = port"3000",
    path: String = "mcp",
    enableCors: Boolean = true,
    auth: Option[AuthConfig[F]] = None
)

object HttpConfig:
  def default[F[_]]: HttpConfig[F] = HttpConfig[F]()

/** Streamable HTTP transport for MCP servers.
  *
  * Implements the MCP Streamable HTTP transport (spec 2025-03-26):
  *   - POST /{path}: Receives JSON-RPC requests, returns JSON-RPC responses
  *   - GET /{path}: Returns 405 (server-initiated notifications not supported)
  *   - GET /health: Health check endpoint
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
      config: HttpConfig[F] = HttpConfig.default[F]
  )(using Tracer[F]): CatsResource[F, Server] =
    for
      dispatcher <- CatsResource.eval(mcp4s.server.Dispatcher[F](server))
      tokenKey <- CatsResource.eval(AuthMiddleware.tokenInfoKey[F])
      given Key[TokenInfo] = tokenKey
      baseRoutes = createRoutes(dispatcher, config.path, summon[Tracer[F]])

      // Apply auth middleware if configured
      protectedRoutes = config.auth match
        case Some(authConfig) => AuthMiddleware[F](authConfig, baseRoutes)
        case None => baseRoutes

      // Add metadata endpoint if auth configured (must be outside auth middleware)
      allRoutes = config.auth match
        case Some(authConfig) =>
          AuthMiddleware.metadataRoutes[F](authConfig) <+> protectedRoutes
        case None => protectedRoutes

      corsRoutes = if config.enableCors then CORS.policy.withAllowOriginAll(allRoutes) else allRoutes
      httpServer <- EmberServerBuilder
        .default[F]
        .withHost(config.host)
        .withPort(config.port)
        .withHttpApp(Router("/" -> corsRoutes).orNotFound)
        .build
    yield httpServer

  private def createRoutes[F[_]: Async](
      dispatcher: mcp4s.server.Dispatcher[F],
      path: String,
      tracer: Tracer[F]
  ): HttpRoutes[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    HttpRoutes.of[F] {
      case req @ POST -> Root / `path` =>
        // Extract trace context from incoming request headers for distributed tracing
        tracer.joinOrRoot(req.headers) {
          tracer.span("http.server.request", Attribute("http.method", "POST"), Attribute("http.route", s"/$path"))
            .use { span =>
              req.as[Json].flatMap { json =>
                json.as[JsonRpcMessage] match
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

      case GET -> Root / `path` =>
        // Server-initiated notifications not supported yet - return 405 per spec
        MethodNotAllowed(Allow(Method.POST))

      case GET -> Root / "health" =>
        Ok(Json.obj("status" -> Json.fromString("ok")))
    }
