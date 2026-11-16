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

import cats.data.EitherT
import cats.effect.{Async, Resource as CatsResource}
import cats.effect.syntax.spawn.*
import cats.syntax.all.*
import fs2.Stream
import fs2.io.net.Network
import com.comcast.ip4s.{host, port, Host, Port}
import io.circe.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.{`Content-Type`, Allow}
import org.http4s.server.{Router, Server as Http4sServer}
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.context.propagation.TextMapGetter
import org.typelevel.otel4s.trace.{Span, Tracer}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*

import scala.concurrent.duration.*

/** Streamable HTTP transport configuration.
  *
  * @param enableSessions
  *   issue an `Mcp-Session-Id` per client and keep per-client protocol state. When false the
  *   transport is stateless: one dispatcher serves every client and no session header is used.
  * @param allowedOrigins
  *   `Origin` values accepted in addition to the localhost origins that are always accepted on a
  *   localhost binding. Requests with any other `Origin` are refused with 403.
  * @param keepAliveInterval
  *   how often an SSE comment is written to idle streams so intermediaries keep them open
  */
final case class HttpConfig(
    host: Host = host"0.0.0.0",
    port: Port = port"3000",
    path: String = "mcp",
    enableSessions: Boolean = true,
    /** Session configuration for timeout, queue sizes, etc. */
    sessionConfig: SessionConfig = SessionConfig.default,
    allowedOrigins: Set[String] = Set.empty,
    keepAliveInterval: FiniteDuration = 30.seconds
)

object HttpConfig:
  val default: HttpConfig = HttpConfig()

/** Streamable HTTP transport for MCP servers (spec 2025-11-25).
  *
  *   - `POST /{path}`: one JSON-RPC message per request. Requests are answered with an SSE stream
  *     when the client accepts `text/event-stream` (carrying related notifications and
  *     server-initiated requests before the response) and with a single JSON object otherwise.
  *     Notifications and responses are acknowledged with `202 Accepted`.
  *   - `GET /{path}`: opens the session's standalone SSE stream for server-to-client messages
  *     unrelated to any request (subscribed resource updates, task-related messages after their
  *     request completed). One per session; a second concurrent GET is refused with 409.
  *   - `DELETE /{path}`: terminates the session.
  *   - `GET /health`: health check.
  *
  * Requests with an unknown `Mcp-Session-Id` get 404 (clients then re-initialize); requests other
  * than `initialize` without a session id get 400. An unsupported `MCP-Protocol-Version` header is
  * refused with 400, and `Origin`/`Host` headers are validated against the binding to defeat DNS
  * rebinding (403).
  */
object HttpTransport:

  /** TextMapGetter for extracting trace context from HTTP headers */
  private given TextMapGetter[Headers] with
    def get(carrier: Headers, key: String): Option[String] =
      carrier.get(CIString(key)).map(_.head.value)
    def keys(carrier: Headers): Iterable[String] =
      carrier.headers.map(_.name.toString)

  private val SessionHeaderName         = CIString("Mcp-Session-Id")
  private val ProtocolVersionHeaderName = CIString("MCP-Protocol-Version")

  /** JSON-RPC error code for invalid session */
  private val InvalidSessionErrorCode = -32000

  /** Valid localhost patterns for DNS rebinding protection */
  private val LocalhostPatterns = Set("localhost", "127.0.0.1", "[::1]", "::1")

  private val SseMediaType = MediaType.`text/event-stream`

  /** Check if a host configuration indicates localhost binding */
  private def isLocalhostBinding(host: Host): Boolean =
    val hostStr = host.toString
    hostStr == "localhost" || hostStr == "127.0.0.1" || hostStr == "::1" || hostStr == "[::1]" || hostStr == "0.0.0.0"

  private def hostnameOf(hostHeader: String): String =
    // "[::1]:3000" keeps its brackets; "localhost:3000" drops the port.
    if hostHeader.startsWith("[") then hostHeader.takeWhile(_ != ']') + "]"
    else hostHeader.split(":").head.toLowerCase

  /** DNS rebinding protection: on a localhost binding the `Host` header must name localhost. */
  private def isValidHostHeader[F[_]](req: Request[F], config: HttpConfig): Boolean =
    !isLocalhostBinding(config.host) ||
      req.headers.get(CIString("Host")).map(_.head.value).forall { h =>
        LocalhostPatterns.contains(hostnameOf(h))
      }

  /** Origin validation (spec: MUST validate; invalid Origin → 403). Explicitly allowed origins
    * always pass; on a localhost binding, localhost origins pass too; elsewhere, without an
    * allowlist, any Origin passes (the deployment is expected to front this with its own policy).
    */
  private def isValidOrigin[F[_]](req: Request[F], config: HttpConfig): Boolean =
    req.headers.get(CIString("Origin")).map(_.head.value).forall { origin =>
      config.allowedOrigins.contains(origin) || {
        val originHost = Uri
          .fromString(origin)
          .toOption
          .flatMap(_.host)
          .map(h => hostnameOf(h.value.toLowerCase))
        if isLocalhostBinding(config.host) then originHost.exists(LocalhostPatterns.contains)
        else config.allowedOrigins.isEmpty
      }
    }

  /** Check if request accepts SSE responses */
  private def acceptsSSE[F[_]](req: Request[F]): Boolean =
    req.headers.get(CIString("Accept")).exists { nel =>
      nel.toList.exists(_.value.contains("text/event-stream"))
    }

  /** Return the raw MCP `HttpRoutes` without CORS or auth wrapping.
    *
    * Use this to embed MCP routes in an existing http4s application and compose standard http4s
    * middleware (CORS, auth, etc.) yourself.
    *
    * @param server
    *   The MCP server to serve
    * @param config
    *   HTTP configuration
    */
  def routes[F[_]: Async](
      server: Server[F],
      config: HttpConfig = HttpConfig.default
  )(using Tracer[F]): CatsResource[F, HttpRoutes[F]] =
    val sessions: CatsResource[F, Sessions[F]] =
      if config.enableSessions then
        SessionManager.withCleanup[F](server, config.sessionConfig).map(Sessions.stateful(_))
      else
        CatsResource
          .make(
            HttpSession.create[F](
              server,
              config.sessionConfig,
              Dispatcher.Config(allowReinitialize = true)
            )
          )(_.shutdown)
          .map(Sessions.stateless(_))
    sessions.map(new Routes[F](_, config, summon[Tracer[F]]).routes)

  /** Start an HTTP server for the given MCP server.
    *
    * @param server
    *   The MCP server to serve
    * @param config
    *   HTTP configuration
    */
  def serve[F[_]: Async: Network](
      server: Server[F],
      config: HttpConfig = HttpConfig.default
  )(using Tracer[F]): CatsResource[F, Http4sServer] =
    for
      mcpRoutes  <- routes(server, config)
      httpServer <- EmberServerBuilder
        .default[F]
        .withHost(config.host)
        .withPort(config.port)
        .withHttpApp(Router("/" -> mcpRoutes).orNotFound)
        .withIdleTimeout(10.minutes) // SSE streams need long idle timeout
        .build
    yield httpServer

  /** How requests map onto sessions: per-client sessions keyed by `Mcp-Session-Id`, or one shared
    * session for a stateless deployment.
    */
  private trait Sessions[F[_]]:
    def stateful: Boolean
    def create: F[HttpSession[F]]
    def get(id: SessionId): F[Option[HttpSession[F]]]
    def remove(id: SessionId): F[Unit]

  private object Sessions:
    def stateful[F[_]](manager: SessionManager[F]): Sessions[F] =
      new Sessions[F]:
        val stateful: Boolean                             = true
        def create: F[HttpSession[F]]                     = manager.create
        def get(id: SessionId): F[Option[HttpSession[F]]] = manager.get(id)
        def remove(id: SessionId): F[Unit]                = manager.remove(id)

    def stateless[F[_]: Async](single: HttpSession[F]): Sessions[F] =
      new Sessions[F]:
        val stateful: Boolean                             = false
        def create: F[HttpSession[F]]                     = Async[F].pure(single)
        def get(id: SessionId): F[Option[HttpSession[F]]] = Async[F].pure(Some(single))
        def remove(id: SessionId): F[Unit]                = Async[F].unit

  private class Routes[F[_]](sessions: Sessions[F], config: HttpConfig, tracer: Tracer[F])(using
      F: Async[F]
  ):
    private val dsl = new Http4sDsl[F] {}
    import dsl.*

    private val path = config.path

    def routes: HttpRoutes[F] =
      HttpRoutes.of[F] {
        case req @ POST -> Root / `path` =>
          guarded(req, "POST")(span => handlePost(req, span))

        case req @ GET -> Root / `path` =>
          guarded(req, "GET")(_ => handleGet(req))

        case req @ DELETE -> Root / `path` =>
          guarded(req, "DELETE")(_ => handleDelete(req))

        case GET -> Root / "health" =>
          Ok(Json.obj("status" -> Json.fromString("ok")))
      }

    /** Shared per-request checks: DNS rebinding (Host/Origin), the protocol version header, and a
      * traced span joined to any incoming trace context.
      */
    private def guarded(req: Request[F], method: String)(
        handle: Span[F] => F[Response[F]]
    ): F[Response[F]] =
      if !isValidHostHeader(req, config) || !isValidOrigin(req, config) then
        Forbidden(
          JsonRpcErrorResponse(
            RequestId.NullId,
            JsonRpcError.invalidRequest("DNS rebinding protection: invalid Host or Origin header")
          ).asJson
        )
      else
        req.headers.get(ProtocolVersionHeaderName).map(_.head.value) match
          case Some(version) if !McpVersion.isSupported(version) =>
            BadRequest(
              JsonRpcErrorResponse(
                RequestId.NullId,
                JsonRpcError.invalidRequest(
                  s"Unsupported MCP-Protocol-Version: $version (supported: " +
                    s"${McpVersion.Supported.mkString(", ")})"
                )
              ).asJson
            )
          case _ =>
            tracer.joinOrRoot(req.headers) {
              tracer
                .span(
                  "http.server.request",
                  Attribute("http.method", method),
                  Attribute("http.route", s"/$path")
                )
                .use(handle)
            }

    // === POST ===

    private def handlePost(req: Request[F], span: Span[F]): F[Response[F]] =
      req.as[Json].attempt.flatMap {
        case Left(err) =>
          BadRequest(errorBody(RequestId.NullId, JsonRpcError.parseError(err.getMessage)))
        case Right(json) =>
          json.as[JsonRpcMessage] match
            case Left(err) =>
              BadRequest(errorBody(RequestId.NullId, JsonRpcError.invalidRequest(err.getMessage)))
            case Right(message) =>
              resolveSession(req, message).flatMap {
                case Left(response) => response.pure[F]
                case Right(session) =>
                  dispatch(req, session, message, span).map(withSessionHeader(session))
              }
      }

    /** Find (or, for `initialize`, create) the session a POST belongs to. */
    private def resolveSession(
        req: Request[F],
        message: JsonRpcMessage
    ): F[Either[Response[F], HttpSession[F]]] =
      val isInitialize = message match
        case r: JsonRpcRequest => r.method == McpMethod.Initialize
        case _                 => false
      if !sessions.stateful then sessions.create.map(Right(_))
      else
        (sessionId(req), isInitialize) match
          case (None, true)  => sessions.create.map(Right(_))
          case (Some(id), _) => lookupSession(id)
          case (None, false) =>
            BadRequest(
              errorBody(requestIdOf(message), invalidSession("Missing Mcp-Session-Id header"))
            ).map(Left(_))

    private def dispatch(
        req: Request[F],
        session: HttpSession[F],
        message: JsonRpcMessage,
        span: Span[F]
    ): F[Response[F]] =
      message match
        case request: JsonRpcRequest if acceptsSSE(req) =>
          span.addAttribute(Attribute("http.status_code", 200L)) *>
            streamingResponse(session, request)
        case request: JsonRpcRequest =>
          session.handle(request).flatMap {
            case Some(response) =>
              span.addAttribute(Attribute("http.status_code", 200L)) *>
                Ok(response.asJson, `Content-Type`(MediaType.application.json))
            case None =>
              span.addAttribute(Attribute("http.status_code", 202L)) *> Accepted()
          }
        case other =>
          // Notifications and client responses carry no reply: 202 Accepted, no body.
          session.handle(other) *>
            span.addAttribute(Attribute("http.status_code", 202L)) *> Accepted()

    /** Answer a request with an SSE stream: a priming event, then every message the handler emits
      * for this request, then the response, after which the stream ends. Dispatch runs in a fiber
      * tied to the stream, so a client that disconnects cancels the handler.
      */
    private def streamingResponse(
        session: HttpSession[F],
        request: JsonRpcRequest
    ): F[Response[F]] =
      val body: Stream[F, ServerSentEvent] =
        Stream.resource(session.requestStream(request.id)).flatMap { stream =>
          val run =
            session
              .handle(request)
              .attempt
              .flatMap {
                case Right(Some(response)) => stream.send(response)
                case Right(None)           => F.unit
                case Left(err)             =>
                  stream.send(
                    JsonRpcErrorResponse(request.id, JsonRpcError.internalError(err.getMessage))
                  )
              } *> stream.close
          Stream.bracket(run.start)(_.cancel) >> events(stream)
        }
      Ok(body, `Content-Type`(SseMediaType))

    // === GET (standalone stream) ===

    private def handleGet(req: Request[F]): F[Response[F]] =
      if !acceptsSSE(req) then
        NotAcceptable(
          errorBody(
            RequestId.NullId,
            JsonRpcError.invalidRequest("GET requires Accept: text/event-stream")
          )
        )
      else
        withSession(req) { session =>
          // Resumption via Last-Event-ID is not supported: the stream always starts fresh.
          session.standaloneStream.allocated.flatMap {
            case (None, release) =>
              release *> Conflict(
                errorBody(
                  RequestId.NullId,
                  JsonRpcError.invalidRequest("Only one standalone SSE stream per session")
                )
              )
            case (Some(stream), release) =>
              val body = Stream.bracket(F.unit)(_ => release) >> events(stream)
              Ok(body, `Content-Type`(SseMediaType)).map(withSessionHeader(session))
          }
        }

    // === DELETE (session termination) ===

    private def handleDelete(req: Request[F]): F[Response[F]] =
      if !sessions.stateful then MethodNotAllowed(Allow(Method.POST, Method.GET))
      else withSession(req)(session => sessions.remove(session.id) *> Ok())

    // === Helpers ===

    /** Render one stream as SSE: a priming event (empty data, carrying the stream's first event id
      * so clients can learn the id scheme), then one `message` event per JSON-RPC message, with
      * periodic keep-alive comments while idle.
      */
    private def events(stream: HttpSession.Stream[F]): Stream[F, ServerSentEvent] =
      val eventId  = (n: Long) => ServerSentEvent.EventId(s"${stream.id}-$n")
      val priming  = ServerSentEvent(data = Some(""), id = Some(eventId(0)))
      val messages = Stream
        .fromQueueNoneTerminated(stream.queue)
        .zipWithIndex
        .map { (msg, i) =>
          ServerSentEvent(
            data = Some(msg.asJson.noSpaces),
            eventType = Some("message"),
            id = Some(eventId(i + 1))
          )
        }
      val keepAlive = Stream
        .awakeEvery[F](config.keepAliveInterval)
        .as(ServerSentEvent(comment = Some("keep-alive")))
      Stream.emit(priming) ++ messages.mergeHaltL(keepAlive)

    private def sessionId(req: Request[F]): Option[SessionId] =
      req.headers.get(SessionHeaderName).map(h => SessionId(h.head.value))

    private def lookupSession(id: SessionId): F[Either[Response[F], HttpSession[F]]] =
      sessions.get(id).flatMap {
        case Some(session) => session.asRight[Response[F]].pure[F]
        case None          =>
          NotFound(errorBody(RequestId.NullId, invalidSession("Session not found")))
            .map(Left(_))
      }

    /** Run `f` with the request's session; in stateless mode that is the shared session. */
    private def withSession(req: Request[F])(f: HttpSession[F] => F[Response[F]]): F[Response[F]] =
      EitherT(
        if !sessions.stateful then sessions.create.map(_.asRight[Response[F]])
        else
          sessionId(req) match
            case Some(id) => lookupSession(id)
            case None     =>
              BadRequest(
                errorBody(RequestId.NullId, invalidSession("Missing Mcp-Session-Id header"))
              )
                .map(Left(_))
      ).semiflatMap(f).merge

    private def withSessionHeader(session: HttpSession[F])(response: Response[F]): Response[F] =
      if sessions.stateful then response.putHeaders(Header.Raw(SessionHeaderName, session.id.value))
      else response

    private def requestIdOf(message: JsonRpcMessage): RequestId =
      message match
        case r: JsonRpcRequest => r.id
        case _                 => RequestId.NullId

    private def invalidSession(detail: String): JsonRpcError =
      JsonRpcError(InvalidSessionErrorCode, detail, None)

    private def errorBody(id: RequestId, error: JsonRpcError): Json =
      JsonRpcErrorResponse(id, error).asJson
