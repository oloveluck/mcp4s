package mcp4s.client.transport

import cats.effect.{Async, Ref, Resource as CatsResource, Temporal}
import cats.syntax.all.*
import fs2.io.net.Network
import io.circe.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.headers.`Content-Type`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.context.propagation.TextMapUpdater
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.{McpClient, McpConnection, ResilienceConfig}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given

/** Authentication method for the HTTP transport. */
enum HttpAuth[F[_]]:
  /** Static bearer token. */
  case Bearer(token: String) extends HttpAuth[F]
  /** Dynamic token provider called before each request. Use for token refresh, Ref-based tokens, etc. */
  case TokenProvider(provide: F[String]) extends HttpAuth[F]

/** Streamable HTTP transport configuration for MCP clients */
final case class HttpClientConfig[F[_]](
    baseUrl: String,
    endpoint: String = "/mcp",
    /** Authentication method. `None` means no auth header is sent. */
    auth: Option[HttpAuth[F]] = None
)

/** Streamable HTTP transport for MCP clients.
  *
  * Implements the MCP Streamable HTTP transport (spec 2025-11-25):
  *   - POST /{endpoint}: Send JSON-RPC requests
  */
object HttpClientTransport:

  /** TextMapUpdater for injecting trace context into HTTP headers */
  private given TextMapUpdater[Headers] with
    def updated(carrier: Headers, key: String, value: String): Headers =
      carrier.put(Header.Raw(CIString(key), value))

  /** Connect to an HTTP MCP server.
    *
    * @param client The MCP client configuration
    * @param config HTTP transport configuration
    * @param tracer Optional OpenTelemetry tracer for distributed tracing (defaults to noop)
    */
  def connect[F[_]: Async: Network](
      client: McpClient[F],
      config: HttpClientConfig[F],
      httpClient: Client[F],
      resilience: Option[ResilienceConfig] = None
  )(using Tracer[F]): CatsResource[F, McpConnection[F]] =
    for
      connection <- CatsResource.eval(establishConnection(client, httpClient, config, summon[Tracer[F]], resilience))
    yield connection

  /** Session header name (case-insensitive per MCP spec) */
  private val SessionHeaderName = CIString("Mcp-Session-Id")

  private def establishConnection[F[_]: Async](
      client: McpClient[F],
      httpClient: Client[F],
      config: HttpClientConfig[F],
      tracer: Tracer[F],
      resilience: Option[ResilienceConfig]
  ): F[McpConnection[F]] =
    val endpointUri = Uri.unsafeFromString(s"${config.baseUrl}${config.endpoint}")

    for
      // Create session ID ref - will be populated after initialize
      sessionIdRef <- Ref.of[F, Option[String]](None)

      // Indirection for progress handlers - set after connection creation
      progressHandlersRef <- Ref.of[F, Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]]](None)

      // Create the request sender function with session and trace context
      sendRequest = createRequestSender(httpClient, endpointUri, sessionIdRef, progressHandlersRef, config, tracer)

      // Create the notification sender function with session and trace context
      sendNotification = createNotificationSender(httpClient, endpointUri, sessionIdRef, config, tracer)

      // Send initialize request and capture session ID from response
      initRequest = JsonRpcRequest(
        RequestId.NumberId(1),
        McpMethod.Initialize,
        Some(InitializeParams(
          protocolVersion = McpVersion.Current,
          capabilities = client.capabilities,
          clientInfo = client.info
        ).asJson)
      )
      initResult <- sendInitRequest(httpClient, endpointUri, initRequest, sessionIdRef, config, tracer)

      // Send initialized notification (now with session ID)
      _ <- sendNotification(JsonRpcNotification(McpMethod.Initialized, None))

      // Wrap sendRequest with resilience if configured
      wrappedSendRequest = resilience match
        case Some(config) => ResilienceConfig.wrapSendRequest(sendRequest, config)(using Temporal[F])
        case None         => sendRequest

      // Create connection via factory
      conn <- McpConnection[F](
        initResult.serverInfo, initResult.capabilities,
        wrappedSendRequest, sendNotification, tracer
      )

      // Wire up progress handlers so SSE routing can find them
      _ <- progressHandlersRef.set(Some(conn.progressHandlers))
    yield conn

  /** Resolve the auth token and add Authorization: Bearer header if configured */
  private def withAuth[F[_]: Async](headers: Headers, config: HttpClientConfig[F]): F[Headers] =
    config.auth match
      case Some(HttpAuth.Bearer(token)) =>
        Async[F].pure(headers.put(Header.Raw(CIString("Authorization"), s"Bearer $token")))
      case Some(HttpAuth.TokenProvider(provide)) =>
        provide.map(token => headers.put(Header.Raw(CIString("Authorization"), s"Bearer $token")))
      case None =>
        Async[F].pure(headers)

  /** Send initialize request and capture session ID from response header */
  private def sendInitRequest[F[_]: Async](
      httpClient: Client[F],
      endpointUri: Uri,
      initRequest: JsonRpcRequest,
      sessionIdRef: Ref[F, Option[String]],
      config: HttpClientConfig[F],
      tracer: Tracer[F]
  ): F[InitializeResult] =
    tracer.propagate(Headers.empty).flatMap { traceHeaders =>
      withAuth(
        traceHeaders.put(Header.Raw(CIString("Accept"), "application/json, text/event-stream")),
        config
      ).flatMap { authedHeaders =>
        val request = Request[F](
          method = Method.POST,
          uri = endpointUri,
          headers = authedHeaders
        ).withEntity(initRequest.asJson)
          .withContentType(`Content-Type`(MediaType.application.json))

        runRequestToJson(httpClient, request).flatMap { case (headers, responseJson) =>
          // Extract session ID from response header
          val sessionIdOpt = headers.get(SessionHeaderName).map(_.head.value)
          sessionIdRef.set(sessionIdOpt).flatMap { _ =>
            responseJson.as[JsonRpcMessage] match
              case Right(JsonRpcResponse(_, result)) =>
                result.as[InitializeResult].liftTo[F]
              case Right(JsonRpcErrorResponse(_, error)) =>
                Async[F].raiseError(McpError.fromJsonRpcError(error))
              case Right(_) =>
                Async[F].raiseError(McpError.InternalError("Unexpected response type"))
              case Left(err) =>
                Async[F]
                  .raiseError(McpError.InternalError(s"Failed to parse response: ${err.getMessage}"))
          }

        }
      }
    }

  private def runRequestToJson[F[_]: Async](
      httpClient: Client[F],
      request: Request[F],
      progressHandlers: Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]] = None
  ) =
    httpClient.run(request).use { response =>
      val sse = response.headers
        .get(CIString("Content-Type"))
        .exists(_.exists(_.value == "text/event-stream"))
      val parsedJson = if (!sse) {
        response.asJson
      } else {
        response.body.through(org.http4s.ServerSentEvent.decoder[F])
          .evalMap {
            case ServerSentEvent(data, eventType, id, retry, comment) =>
              data
                .map(utf8String =>
                  io.circe.parser.parse(utf8String) match
                    case Left(value) =>
                      Async[F].raiseError[Option[Json]](
                        McpError
                          .InternalError(s"Parse error of SSE data (expected json) ${value}")
                      )
                    case Right(json) =>
                      // Check if this is a progress notification
                      json.as[JsonRpcMessage] match
                        case Right(notif: JsonRpcNotification) if notif.method == McpMethod.Progress =>
                          val routed = for
                            handlers <- progressHandlers.traverse(_.get)
                            pp = notif.params.flatMap(_.as[ProgressParams].toOption)
                            _ <- pp.traverse_ { p =>
                              handlers.flatMap(_.get(p.progressToken)).traverse_(_(p))
                            }
                          yield ()
                          routed.as(Option.empty[Json])
                        case _ =>
                          Async[F].pure(Some(json))
                )
                .getOrElse(Async[F].pure(Option.empty[Json]))
          }
          .collect { case Some(json) => json }
          .take(1)
          .compile
          .lastOrError
      }
      parsedJson.map(js => response.headers -> js)
    }

  /** Create a request sender function that includes session ID */
  private def createRequestSender[F[_]: Async](
      httpClient: Client[F],
      endpointUri: Uri,
      sessionIdRef: Ref[F, Option[String]],
      progressHandlersRef: Ref[F, Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]]],
      config: HttpClientConfig[F],
      tracer: Tracer[F]
  ): JsonRpcRequest => F[Json] = { req =>
    for
      sessionIdOpt <- sessionIdRef.get
      traceHeaders <- tracer.propagate(Headers.empty)
      baseHeaders <- withAuth(
        traceHeaders.put(Header.Raw(CIString("Accept"), "application/json, text/event-stream")),
        config
      )
      headersWithSession = sessionIdOpt match
        case Some(sessionId) =>
          baseHeaders.put(Header.Raw(SessionHeaderName, sessionId))
        case None =>
          baseHeaders
      request = Request[F](
        method = Method.POST,
        uri = endpointUri,
        headers = headersWithSession
      ).withEntity(req.asJson)
        .withContentType(`Content-Type`(MediaType.application.json))

      progressHandlers <- progressHandlersRef.get
      responseJsonAndHeader <- runRequestToJson(httpClient, request, progressHandlers)
      responseJson = responseJsonAndHeader._2

      result <-
        responseJson.as[JsonRpcMessage] match
          case Right(JsonRpcResponse(_, result)) =>
            Async[F].pure(result)
          case Right(JsonRpcErrorResponse(_, error)) =>
            Async[F].raiseError(McpError.fromJsonRpcError(error))
          case Right(_) =>
            Async[F].raiseError(McpError.InternalError("Unexpected response type"))
          case Left(err) =>
            Async[F].raiseError(
              McpError.InternalError(s"Failed to parse response: ${err.getMessage}")
            )
    yield result
  }

  /** Create a notification sender function that includes session ID */
  private def createNotificationSender[F[_]: Async](
      httpClient: Client[F],
      endpointUri: Uri,
      sessionIdRef: Ref[F, Option[String]],
      config: HttpClientConfig[F],
      tracer: Tracer[F]
  ): JsonRpcNotification => F[Unit] = { notif =>
    for
      sessionIdOpt <- sessionIdRef.get
      traceHeaders <- tracer.propagate(Headers.empty)
      baseHeaders <- withAuth(
        traceHeaders.put(Header.Raw(CIString("Accept"), "application/json, text/event-stream")),
        config
      )
      headersWithSession = sessionIdOpt match
        case Some(sessionId) =>
          baseHeaders.put(Header.Raw(SessionHeaderName, sessionId))
        case None =>
          baseHeaders
      request = Request[F](
        method = Method.POST,
        uri = endpointUri,
        headers = headersWithSession
      ).withEntity(notif.asJson)
        .withContentType(`Content-Type`(MediaType.application.json))
      _ <- httpClient.status(request)
    yield ()
  }
