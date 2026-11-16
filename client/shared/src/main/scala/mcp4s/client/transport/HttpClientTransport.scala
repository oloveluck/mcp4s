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

package mcp4s.client.transport

import cats.effect.{Async, Ref, Resource as CatsResource}
import cats.effect.std.{Queue, Supervisor}
import cats.syntax.all.*
import fs2.Stream
import io.circe.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.headers.`Content-Type`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.context.propagation.TextMapUpdater
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.{ConnectionRunner, McpClient, McpConnection}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.transport.{McpChannel, Timeouts}

import scala.concurrent.duration.*

/** Streamable HTTP transport configuration for MCP clients.
  *
  * @param listen
  *   open the server's standalone SSE stream (HTTP GET) once initialized, so server-pushed
  *   notifications such as subscribed resource updates arrive between requests. The stream is
  *   reopened if it drops; servers that answer 405 are left alone.
  */
final case class HttpTransportConfig[F[_]](
    uri: String,
    /** Authentication method. `None` means no auth header is sent. */
    auth: Option[McpAuth[F]] = None,
    /** Maximum queue size for incoming messages (bounded queue with backpressure) */
    maxQueueSize: Int = 1024,
    timeouts: Timeouts = Timeouts.default,
    listen: Boolean = true
)

/** Streamable HTTP transport for MCP clients (spec 2025-11-25).
  *
  * Each outbound request is a POST to the MCP endpoint. When the server responds with an SSE
  * stream, every event on it — progress notifications, server-initiated requests such as
  * sampling/elicitation, and the final response — is surfaced through the channel, so bidirectional
  * flows work exactly as they do over WebSocket. After initialization the client sends the
  * negotiated `MCP-Protocol-Version` on every request, listens on the standalone GET stream, and
  * terminates the session with DELETE when the connection is released.
  */
object HttpClientTransport:

  /** TextMapUpdater for injecting trace context into HTTP headers */
  private given TextMapUpdater[Headers] with
    def updated(carrier: Headers, key: String, value: String): Headers =
      carrier.put(Header.Raw(CIString(key), value))

  private val SessionHeaderName         = CIString("Mcp-Session-Id")
  private val ProtocolVersionHeaderName = CIString("MCP-Protocol-Version")
  private val AcceptHeaderName          = CIString("Accept")

  /** Build a [[ClientTransport]] over a caller-supplied http4s `Client[F]`. */
  def apply[F[_]](
      httpClient: Client[F],
      config: HttpTransportConfig[F]
  )(using Async[F])(using tracer: Tracer[F] = Tracer.noop[F]): ClientTransport[F] =
    new ClientTransport[F]:
      def open: CatsResource[F, McpChannel[F]] =
        for
          supervisor <- Supervisor[F]
          channel    <- CatsResource.make(
            for
              sessionIdRef <- Ref.of[F, Option[String]](None)
              versionRef   <- Ref.of[F, Option[String]](None)
              inbox        <- Queue.bounded[F, JsonRpcMessage](config.maxQueueSize)
            yield new HttpChannel[F](
              httpClient,
              config,
              sessionIdRef,
              versionRef,
              inbox,
              supervisor,
              tracer
            )
          )(_.terminateSession)
        yield channel

  /** Connect to an HTTP MCP server using a caller-supplied http4s `Client[F]`. */
  def connect[F[_]: Async](
      client: McpClient[F],
      config: HttpTransportConfig[F],
      httpClient: Client[F]
  )(using Tracer[F]): CatsResource[F, McpConnection[F]] =
    ConnectionRunner.run(client, apply(httpClient, config), config.timeouts, summon[Tracer[F]])

  /** One MCP channel over Streamable HTTP.
    *
    * `send` POSTs the message. JSON responses are enqueued into the inbox; SSE responses are
    * drained event-by-event into the inbox until the terminal response for the posted request
    * arrives. The inbox is what [[incoming]] surfaces to the [[ConnectionRunner]].
    */
  final private class HttpChannel[F[_]](
      httpClient: Client[F],
      config: HttpTransportConfig[F],
      sessionIdRef: Ref[F, Option[String]],
      versionRef: Ref[F, Option[String]],
      inbox: Queue[F, JsonRpcMessage],
      supervisor: Supervisor[F],
      tracer: Tracer[F]
  )(using F: Async[F])
      extends McpChannel[F]:

    private val endpointUri = Uri.unsafeFromString(config.uri)

    def incoming: Stream[F, JsonRpcMessage] =
      Stream.fromQueueUnterminated(inbox)

    def send(message: JsonRpcMessage): F[Unit] =
      message match
        case req: JsonRpcRequest =>
          post(req.asJson).flatMap(drainResponse(req.id, _))
        case other =>
          // Notifications and responses to server-initiated requests expect no correlated
          // reply; POST them and ignore the (202 Accepted) response body.
          post(other.asJson).flatMap(_.use(checkStatus))

    def negotiated(protocolVersion: String): F[Unit] =
      versionRef.set(Some(protocolVersion)) *>
        (if config.listen then supervisor.supervise(listen).void else F.unit)

    /** Explicitly end the session (HTTP DELETE); best effort, errors are ignored. */
    def terminateSession: F[Unit] =
      sessionIdRef.get.flatMap {
        case None    => F.unit
        case Some(_) =>
          headers
            .flatMap { hs =>
              httpClient
                .run(Request[F](method = Method.DELETE, uri = endpointUri, headers = hs))
                .use_
            }
            .attempt
            .void *> sessionIdRef.set(None)
      }

    private def headers: F[Headers] =
      for
        traceHeaders <- tracer.propagate(Headers.empty)
        authed       <- McpAuth.applyTo(
          config.auth,
          traceHeaders.put(Header.Raw(AcceptHeaderName, "application/json, text/event-stream"))
        )
        sessionIdOpt <- sessionIdRef.get
        versionOpt   <- versionRef.get
      yield
        val withSession =
          sessionIdOpt.fold(authed)(id => authed.put(Header.Raw(SessionHeaderName, id)))
        versionOpt.fold(withSession)(v => withSession.put(Header.Raw(ProtocolVersionHeaderName, v)))

    private def post(body: Json): F[CatsResource[F, Response[F]]] =
      headers.map { hs =>
        httpClient.run(
          Request[F](method = Method.POST, uri = endpointUri, headers = hs)
            .withEntity(body)
            .withContentType(`Content-Type`(MediaType.application.json))
        )
      }

    /** Fail on non-success statuses, surfacing the JSON-RPC error body when the server sent one. A
      * 404 means the server dropped the session: forget the id so a reconnect re-initializes.
      */
    private def checkStatus(resp: Response[F]): F[Unit] =
      if resp.status.isSuccess then F.unit
      else
        val forgetSession =
          if resp.status == Status.NotFound then sessionIdRef.set(None) else F.unit
        forgetSession *> resp.bodyText.compile.string.attempt.flatMap { body =>
          val detail = body.toOption
            .flatMap(b => io.circe.parser.parse(b).flatMap(_.as[JsonRpcErrorResponse]).toOption)
            .map(_.error.message)
            .getOrElse(resp.status.reason)
          F.raiseError(McpError.InternalError(s"HTTP ${resp.status.code}: $detail"))
        }

    /** Consume the response to a posted request: capture the session id, then enqueue every
      * JSON-RPC message it carries until the terminal response for `requestId` arrives.
      */
    private def drainResponse(
        requestId: RequestId,
        response: CatsResource[F, Response[F]]
    ): F[Unit] =
      response.use { resp =>
        val captureSession =
          resp.headers.get(SessionHeaderName) match
            case Some(values) => sessionIdRef.update(_.orElse(Some(values.head.value)))
            case None         => F.unit

        val consume =
          if isSse(resp) then
            sseMessages(resp)
              .evalTap(inbox.offer)
              .takeThrough {
                case JsonRpcResponse(id, _)      => id != requestId
                case JsonRpcErrorResponse(id, _) => id != requestId
                case _                           => true
              }
              .compile
              .drain
          else
            resp.asJson.flatMap { json =>
              json.as[JsonRpcMessage] match
                case Right(message) => inbox.offer(message)
                case Left(err)      =>
                  F.raiseError[Unit](
                    McpError.InternalError(s"Failed to parse response: ${err.getMessage}")
                  )
            }

        checkStatus(resp) *> captureSession *> consume
      }

    private def isSse(resp: Response[F]): Boolean =
      resp.headers
        .get(CIString("Content-Type"))
        .exists(_.exists(_.value.startsWith("text/event-stream")))

    /** The JSON-RPC messages on an SSE body. Priming events (empty data) and comments carry no
      * message and are skipped.
      */
    private def sseMessages(resp: Response[F]): Stream[F, JsonRpcMessage] =
      resp.body
        .through(ServerSentEvent.decoder[F])
        .evalMapFilter { event =>
          event.data.map(_.trim).filter(_.nonEmpty) match
            case None       => F.pure(Option.empty[JsonRpcMessage])
            case Some(data) =>
              io.circe.parser.parse(data).flatMap(_.as[JsonRpcMessage]) match
                case Right(message) => F.pure(Some(message))
                case Left(err)      =>
                  F.raiseError[Option[JsonRpcMessage]](
                    McpError.InternalError(s"Failed to parse SSE event: $err")
                  )
        }

    /** Keep the standalone GET stream open for the channel's lifetime, reconnecting with a short
      * backoff when it ends. Stops for good when the server declines (405, or any 4xx).
      */
    private def listen: F[Unit] =
      def once: F[Boolean] =
        headers.flatMap { hs =>
          httpClient
            .run(
              Request[F](method = Method.GET, uri = endpointUri, headers = hs)
                .putHeaders(Header.Raw(AcceptHeaderName, "text/event-stream"))
            )
            .use { resp =>
              if resp.status.isSuccess && isSse(resp) then
                sseMessages(resp).evalMap(inbox.offer).compile.drain.as(true)
              else F.pure(resp.status.responseClass == Status.ServerError)
            }
            .handleError(_ => true)
        }
      once.flatMap(retry => if retry then F.sleep(1.second) *> listen else F.unit)
