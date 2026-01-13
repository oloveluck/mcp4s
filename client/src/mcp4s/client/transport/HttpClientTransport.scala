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
import mcp4s.client.{McpClient, McpConnection, McpConnectionImpl}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given

/** HTTP transport configuration for MCP clients */
final case class HttpClientConfig(
    baseUrl: String,
    messageEndpoint: String = "/message",
    sseEndpoint: String = "/sse"
)

/** HTTP transport for MCP clients.
  *
  * Connects to an MCP server via HTTP endpoints:
  *   - POST /message: Send JSON-RPC requests
  *   - GET /sse: Receive server-initiated requests (optional)
  */
object HttpClientTransport:

  /** Connect to an HTTP MCP server */
  def connect[F[_]: Async: Network](
      client: McpClient[F],
      config: HttpClientConfig
  ): CatsResource[F, McpConnection[F]] =
    for
      httpClient <- EmberClientBuilder.default[F].build
      connection <- CatsResource.eval(establishConnection(client, httpClient, config))
    yield connection

  private def establishConnection[F[_]: Async](
      client: McpClient[F],
      httpClient: Client[F],
      config: HttpClientConfig
  ): F[McpConnection[F]] =
    val messageUri = Uri.unsafeFromString(s"${config.baseUrl}${config.messageEndpoint}")

    // Create the request sender function
    val sendRequest: JsonRpcRequest => F[Json] = { req =>
      val request = Request[F](
        method = Method.POST,
        uri = messageUri
      ).withEntity(req.asJson)
        .withContentType(`Content-Type`(MediaType.application.json))

      httpClient.expect[Json](request).flatMap { responseJson =>
        // Parse the response
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
    }

    // Create the notification sender function
    val sendNotification: JsonRpcNotification => F[Unit] = { notif =>
      val request = Request[F](
        method = Method.POST,
        uri = messageUri
      ).withEntity(notif.asJson)
        .withContentType(`Content-Type`(MediaType.application.json))
      httpClient.status(request).void
    }

    for
      // Create request ID generator (starts at 0, first ID will be 1)
      requestIdGen <- Ref.of[F, Long](0L)
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
      // Send initialized notification
      _ <- sendNotification(JsonRpcNotification(McpMethod.Initialized, None))
      // Create in-flight request registry for cancellation support
      inFlightRef <- Ref.of[F, Map[RequestId, Deferred[F, Unit]]](Map.empty)
    yield new McpConnectionImpl[F](
      initResult.serverInfo,
      initResult.capabilities,
      sendRequest,
      sendNotification,
      requestIdGen,
      inFlightRef
    )
