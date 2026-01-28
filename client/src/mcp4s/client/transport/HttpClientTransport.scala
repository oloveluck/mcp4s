package mcp4s.client.transport

import cats.effect.{Async, Deferred, Ref, Resource as CatsResource}
import cats.syntax.all.*
import fs2.io.net.Network
import io.circe.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.`Content-Type`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.context.propagation.TextMapUpdater
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.{McpClient, McpConnection, McpConnectionImpl}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given

/** Streamable HTTP transport configuration for MCP clients */
final case class HttpClientConfig(
    baseUrl: String,
    endpoint: String = "/mcp"
)

/** Streamable HTTP transport for MCP clients.
  *
  * Implements the MCP Streamable HTTP transport (spec 2025-03-26):
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
      config: HttpClientConfig
  )(using Tracer[F]): CatsResource[F, McpConnection[F]] =
    for
      httpClient <- EmberClientBuilder.default[F].build
      connection <- CatsResource.eval(establishConnection(client, httpClient, config, summon[Tracer[F]]))
    yield connection

  /** Session header name (case-insensitive per MCP spec) */
  private val SessionHeaderName = CIString("Mcp-Session-Id")

  private def establishConnection[F[_]: Async](
      client: McpClient[F],
      httpClient: Client[F],
      config: HttpClientConfig,
      tracer: Tracer[F]
  ): F[McpConnection[F]] =
    val endpointUri = Uri.unsafeFromString(s"${config.baseUrl}${config.endpoint}")

    for
      // Create session ID ref - will be populated after initialize
      sessionIdRef <- Ref.of[F, Option[String]](None)

      // Create request ID generator (starts at 0, first ID will be 1)
      requestIdGen <- Ref.of[F, Long](0L)

      // Create the request sender function with session and trace context
      sendRequest = createRequestSender(httpClient, endpointUri, sessionIdRef, tracer)

      // Create the notification sender function with session and trace context
      sendNotification = createNotificationSender(httpClient, endpointUri, sessionIdRef, tracer)

      // Send initialize request and capture session ID from response
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
      initResult <- sendInitRequest(httpClient, endpointUri, initRequest, sessionIdRef, tracer)

      // Send initialized notification (now with session ID)
      _ <- sendNotification(JsonRpcNotification(McpMethod.Initialized, None))

      // Create in-flight request registry for cancellation support
      inFlightRef <- Ref.of[F, Map[RequestId, Deferred[F, Unit]]](Map.empty)
    yield new McpConnectionImpl[F](
      initResult.serverInfo,
      initResult.capabilities,
      sendRequest,
      sendNotification,
      requestIdGen,
      inFlightRef,
      tracer
    )

  /** Send initialize request and capture session ID from response header */
  private def sendInitRequest[F[_]: Async](
      httpClient: Client[F],
      endpointUri: Uri,
      initRequest: JsonRpcRequest,
      sessionIdRef: Ref[F, Option[String]],
      tracer: Tracer[F]
  ): F[InitializeResult] =
    tracer.propagate(Headers.empty).flatMap { traceHeaders =>
      val request = Request[F](
        method = Method.POST,
        uri = endpointUri,
        headers = traceHeaders.put(
          Header.Raw(CIString("Accept"), "application/json")
        )
      ).withEntity(initRequest.asJson)
        .withContentType(`Content-Type`(MediaType.application.json))

      httpClient.run(request).use { response =>
        // Extract session ID from response header
        val sessionIdOpt = response.headers.get(SessionHeaderName).map(_.head.value)
        sessionIdRef.set(sessionIdOpt) *>
          response.as[Json].flatMap { responseJson =>
            responseJson.as[JsonRpcMessage] match
              case Right(JsonRpcResponse(_, result)) =>
                result.as[InitializeResult].liftTo[F]
              case Right(JsonRpcErrorResponse(_, error)) =>
                Async[F].raiseError(McpError.fromJsonRpcError(error))
              case Right(_) =>
                Async[F].raiseError(McpError.InternalError("Unexpected response type"))
              case Left(err) =>
                Async[F].raiseError(McpError.InternalError(s"Failed to parse response: ${err.getMessage}"))
          }
      }
    }

  /** Create a request sender function that includes session ID */
  private def createRequestSender[F[_]: Async](
      httpClient: Client[F],
      endpointUri: Uri,
      sessionIdRef: Ref[F, Option[String]],
      tracer: Tracer[F]
  ): JsonRpcRequest => F[Json] = { req =>
    for
      sessionIdOpt <- sessionIdRef.get
      traceHeaders <- tracer.propagate(Headers.empty)
      headersWithSession = sessionIdOpt match
        case Some(sessionId) =>
          traceHeaders
            .put(Header.Raw(CIString("Accept"), "application/json"))
            .put(Header.Raw(SessionHeaderName, sessionId))
        case None =>
          traceHeaders.put(Header.Raw(CIString("Accept"), "application/json"))
      request = Request[F](
        method = Method.POST,
        uri = endpointUri,
        headers = headersWithSession
      ).withEntity(req.asJson)
        .withContentType(`Content-Type`(MediaType.application.json))
      result <- httpClient.expect[Json](request).flatMap { responseJson =>
        responseJson.as[JsonRpcMessage] match
          case Right(JsonRpcResponse(_, result)) =>
            Async[F].pure(result)
          case Right(JsonRpcErrorResponse(_, error)) =>
            Async[F].raiseError(McpError.fromJsonRpcError(error))
          case Right(_) =>
            Async[F].raiseError(McpError.InternalError("Unexpected response type"))
          case Left(err) =>
            Async[F].raiseError(McpError.InternalError(s"Failed to parse response: ${err.getMessage}"))
      }
    yield result
  }

  /** Create a notification sender function that includes session ID */
  private def createNotificationSender[F[_]: Async](
      httpClient: Client[F],
      endpointUri: Uri,
      sessionIdRef: Ref[F, Option[String]],
      tracer: Tracer[F]
  ): JsonRpcNotification => F[Unit] = { notif =>
    for
      sessionIdOpt <- sessionIdRef.get
      traceHeaders <- tracer.propagate(Headers.empty)
      headersWithSession = sessionIdOpt match
        case Some(sessionId) =>
          traceHeaders
            .put(Header.Raw(CIString("Accept"), "application/json"))
            .put(Header.Raw(SessionHeaderName, sessionId))
        case None =>
          traceHeaders.put(Header.Raw(CIString("Accept"), "application/json"))
      request = Request[F](
        method = Method.POST,
        uri = endpointUri,
        headers = headersWithSession
      ).withEntity(notif.asJson)
        .withContentType(`Content-Type`(MediaType.application.json))
      _ <- httpClient.status(request)
    yield ()
  }
