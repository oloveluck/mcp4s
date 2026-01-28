package mcp4s.server.transport

import cats.effect.{Async, Resource as CatsResource}
import fs2.Stream
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

import scala.concurrent.duration.FiniteDuration

/** Streamable HTTP transport configuration */
final case class HttpConfig[F[_]](
    host: Host = host"0.0.0.0",
    port: Port = port"3000",
    path: String = "mcp",
    enableCors: Boolean = true,
    auth: Option[AuthConfig[F]] = None,
    enableSessions: Boolean = true,
    sessionTimeout: Option[FiniteDuration] = None
)

object HttpConfig:
  def default[F[_]]: HttpConfig[F] = HttpConfig[F]()

/** Streamable HTTP transport for MCP servers.
  *
  * Implements the MCP Streamable HTTP transport (spec 2025-03-26):
  *   - POST /{path}: Receives JSON-RPC requests, returns JSON-RPC responses
  *     - When client accepts SSE (text/event-stream): Returns SSE stream with
  *       notifications, server-initiated requests, and final response
  *     - Otherwise: Returns single JSON response
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

  /** Session header name (case-insensitive per MCP spec) */
  private val SessionHeaderName = CIString("Mcp-Session-Id")

  /** JSON-RPC error code for invalid session */
  private val InvalidSessionErrorCode = -32000

  /** Valid localhost patterns for DNS rebinding protection */
  private val LocalhostPatterns = Set("localhost", "127.0.0.1", "[::1]", "::1")

  /** SSE media type */
  private val SseMediaType = MediaType.`text/event-stream`

  /** Check if a host configuration indicates localhost binding */
  private def isLocalhostBinding(host: Host): Boolean =
    val hostStr = host.toString
    hostStr == "localhost" || hostStr == "127.0.0.1" || hostStr == "::1" || hostStr == "[::1]" || hostStr == "0.0.0.0"

  /** Validate Host header for DNS rebinding protection.
    * When server is bound to localhost, reject requests with non-localhost Host headers.
    */
  private def isValidHostHeader[F[_]](req: Request[F], configHost: Host): Boolean =
    if !isLocalhostBinding(configHost) then true
    else
      req.headers.get(CIString("Host")).map(_.head.value) match
        case None => true // No host header, allow (unusual but not a rebinding attack)
        case Some(hostHeader) =>
          // Extract hostname without port
          val hostname = hostHeader.split(":").head.toLowerCase
          LocalhostPatterns.contains(hostname)

  /** Check if request accepts SSE responses */
  private def acceptsSSE[F[_]](req: Request[F]): Boolean =
    req.headers.get(CIString("Accept")).exists { nel =>
      nel.toList.exists(_.value.contains("text/event-stream"))
    }

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
      tokenKey <- CatsResource.eval(AuthMiddleware.tokenInfoKey[F])
      given Key[TokenInfo] = tokenKey
      baseRoutes <- if config.enableSessions then
        CatsResource.eval(SessionManager[F](server)).map { sessionManager =>
          createSessionRoutes(sessionManager, config.path, config.host, summon[Tracer[F]])
        }
      else
        // Legacy mode: single shared dispatcher (not MCP compliant)
        CatsResource.eval(mcp4s.server.Dispatcher[F](server)).map { dispatcher =>
          createLegacyRoutes(dispatcher, config.path, config.host, summon[Tracer[F]])
        }

      // Apply auth middleware if configured
      protectedRoutes = config.auth match
        case Some(authConfig) => AuthMiddleware[F](authConfig, baseRoutes)
        case None => baseRoutes

      // Add metadata endpoint if auth configured (must be outside auth middleware)
      allRoutes = config.auth match
        case Some(authConfig) =>
          AuthMiddleware.metadataRoutes[F](authConfig) <+> protectedRoutes
        case None => protectedRoutes

      corsRoutes = if config.enableCors then
        CORS.policy
          .withAllowOriginAll
          .withExposeHeadersAll  // Expose Mcp-Session-Id for browser clients
          (allRoutes)
      else allRoutes
      httpServer <- EmberServerBuilder
        .default[F]
        .withHost(config.host)
        .withPort(config.port)
        .withHttpApp(Router("/" -> corsRoutes).orNotFound)
        .build
    yield httpServer

  /** Create routes with session management (MCP compliant) */
  private def createSessionRoutes[F[_]: Async](
      sessionManager: SessionManager[F],
      path: String,
      configHost: Host,
      tracer: Tracer[F]
  ): HttpRoutes[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    HttpRoutes.of[F] {
      case req @ POST -> Root / `path` =>
        // DNS rebinding protection
        if !isValidHostHeader(req, configHost) then
          Forbidden("DNS rebinding protection: invalid Host header")
        else
          // Extract trace context from incoming request headers for distributed tracing
          tracer.joinOrRoot(req.headers) {
            tracer.span("http.server.request", Attribute("http.method", "POST"), Attribute("http.route", s"/$path"))
              .use { span =>
                req.as[Json].flatMap { json =>
                  json.as[JsonRpcMessage] match
                    case Right(message) =>
                      handleSessionRequest(req, message, sessionManager, span, acceptsSSE(req))
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

  /** Handle a request with session management */
  private def handleSessionRequest[F[_]: Async](
      req: Request[F],
      message: JsonRpcMessage,
      sessionManager: SessionManager[F],
      span: org.typelevel.otel4s.trace.Span[F],
      useSSE: Boolean
  ): F[Response[F]] =
    val sessionIdOpt = req.headers.get(SessionHeaderName).map(_.head.value)
    val isInitialize = message match
      case r: JsonRpcRequest => r.method == McpMethod.Initialize
      case _ => false

    (sessionIdOpt, isInitialize) match
      // Case 1: No session + initialize → create new session
      case (None, true) =>
        sessionManager.create.flatMap { session =>
          dispatchWithSession(session, message, span, useSSE).map { response =>
            response.putHeaders(Header.Raw(SessionHeaderName, session.id))
          }
        }

      // Case 2: Has session ID → lookup and dispatch
      case (Some(sessionId), _) =>
        sessionManager.get(sessionId).flatMap {
          case Some(session) =>
            dispatchWithSession(session, message, span, useSSE).map { response =>
              response.putHeaders(Header.Raw(SessionHeaderName, session.id))
            }
          case None =>
            // Invalid session ID
            invalidSessionResponse(message)
        }

      // Case 3: No session + not initialize → error
      case (None, false) =>
        invalidSessionResponse(message)

  /** Dispatch a message using the session's dispatcher */
  private def dispatchWithSession[F[_]: Async](
      session: HttpSession[F],
      message: JsonRpcMessage,
      span: org.typelevel.otel4s.trace.Span[F],
      useSSE: Boolean
  ): F[Response[F]] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    // Check if this is a response to a server-initiated request
    message match
      case resp: JsonRpcResponse =>
        // Client is responding to our sampling/elicitation request
        session.handleResponse(resp) *>
          span.addAttribute(Attribute("http.status_code", 204L)) *>
          NoContent()

      case errResp: JsonRpcErrorResponse =>
        // Client is sending an error response to our request
        session.handleErrorResponse(errResp) *>
          span.addAttribute(Attribute("http.status_code", 204L)) *>
          NoContent()

      case _ =>
        // Extract client capabilities from initialize request
        session.maybeExtractClientCaps(message) *> {
          if useSSE then
            handleStreamingRequest(session, message, span)
          else
            handleSimpleRequest(session, message, span)
        }

  /** Handle a request with SSE streaming response.
    *
    * Returns an SSE stream that:
    * 1. Runs dispatch and collects any notifications from outQueue
    * 2. Emits collected notifications followed by final response
    *
    * For bidirectional communication (sampling/elicitation), we need a different
    * approach where the stream stays open while awaiting client responses.
    */
  private def handleStreamingRequest[F[_]: Async](
      session: HttpSession[F],
      message: JsonRpcMessage,
      span: org.typelevel.otel4s.trace.Span[F]
  ): F[Response[F]] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    import fs2.concurrent.SignallingRef

    SignallingRef[F, Boolean](false).flatMap { dispatchDone =>
      // The SSE stream that:
      // 1. Runs dispatch in the background
      // 2. Emits queue messages while dispatch runs
      // 3. Emits final response when dispatch completes
      val sseStream: Stream[F, ServerSentEvent] = {
        // Start dispatch as a background effect, storing result
        import cats.effect.Ref
        Stream.eval(Ref.of[F, Option[JsonRpcMessage]](None)).flatMap { resultRef =>
          // The dispatch effect runs concurrently
          val dispatchStream = Stream.eval(
            session.dispatcher.dispatch(message).flatMap { result =>
              resultRef.set(result) *> dispatchDone.set(true)
            }
          ).drain

          // Queue drain stream - polls the queue until dispatch is done
          // Uses a small delay to avoid busy-waiting when queue is empty
          val queueStream = Stream.repeatEval(
            session.outQueue.tryTake.flatMap {
              case Some(msg) => Async[F].pure(Some(msg))
              case None => Async[F].sleep(scala.concurrent.duration.Duration(10, "ms")).as(None)
            }
          ).flatMap {
            case Some(msg) => Stream.emit(ServerSentEvent(data = Some(msg.asJson.noSpaces)))
            case None => Stream.empty
          }.interruptWhen(dispatchDone.discrete)

          // Run queue draining concurrently with dispatch
          val mainStream = queueStream.concurrently(dispatchStream)

          // After main stream ends, drain any remaining queue messages and emit final response
          mainStream ++ Stream.eval(drainQueue(session)).flatMap { remaining =>
            val remainingEvents = remaining.map(msg => ServerSentEvent(data = Some(msg.asJson.noSpaces)))
            Stream.emits(remainingEvents)
          } ++ Stream.eval(resultRef.get).flatMap {
            case Some(response) =>
              Stream.emit(ServerSentEvent(data = Some(response.asJson.noSpaces)))
            case None =>
              Stream.empty // Notification, no response needed
          }
        }
      }

      span.addAttribute(Attribute("http.status_code", 200L)) *>
        Ok(sseStream, `Content-Type`(SseMediaType))
    }

  /** Drain all remaining messages from the queue */
  private def drainQueue[F[_]: Async](session: HttpSession[F]): F[List[JsonRpcMessage]] =
    def loop(acc: List[JsonRpcMessage]): F[List[JsonRpcMessage]] =
      session.outQueue.tryTake.flatMap {
        case Some(msg) => loop(acc :+ msg)
        case None => Async[F].pure(acc)
      }
    loop(Nil)

  /** Handle a simple (non-streaming) request */
  private def handleSimpleRequest[F[_]: Async](
      session: HttpSession[F],
      message: JsonRpcMessage,
      span: org.typelevel.otel4s.trace.Span[F]
  ): F[Response[F]] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    session.dispatcher.dispatch(message).flatMap {
      case Some(response) =>
        span.addAttribute(Attribute("http.status_code", 200L)) *>
          Ok(response.asJson, `Content-Type`(MediaType.application.json))
      case None =>
        span.addAttribute(Attribute("http.status_code", 204L)) *>
          NoContent()
    }

  /** Return HTTP 400 with JSON-RPC error for invalid/missing session */
  private def invalidSessionResponse[F[_]: Async](message: JsonRpcMessage): F[Response[F]] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    val requestId = message match
      case r: JsonRpcRequest => r.id
      case _ => RequestId.NullId

    val error = JsonRpcErrorResponse(
      requestId,
      JsonRpcError(InvalidSessionErrorCode, "Invalid or missing session", None)
    )
    BadRequest(error.asJson, `Content-Type`(MediaType.application.json))

  /** Create legacy routes without session management (not MCP compliant) */
  private def createLegacyRoutes[F[_]: Async](
      dispatcher: mcp4s.server.Dispatcher[F],
      path: String,
      configHost: Host,
      tracer: Tracer[F]
  ): HttpRoutes[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    HttpRoutes.of[F] {
      case req @ POST -> Root / `path` =>
        // DNS rebinding protection
        if !isValidHostHeader(req, configHost) then
          Forbidden("DNS rebinding protection: invalid Host header")
        else
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
