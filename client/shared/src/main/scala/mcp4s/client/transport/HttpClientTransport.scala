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
import cats.effect.std.Queue
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

/** Streamable HTTP transport configuration for MCP clients. */
final case class HttpTransportConfig[F[_]](
    uri: String,
    /** Authentication method. `None` means no auth header is sent. */
    auth: Option[McpAuth[F]] = None,
    /** Maximum queue size for incoming messages (bounded queue with backpressure) */
    maxQueueSize: Int = 1024,
    timeouts: Timeouts = Timeouts.default
)

/** Streamable HTTP transport for MCP clients (spec 2025-11-25).
  *
  * Each outbound request is a POST to the MCP endpoint. When the server responds with an SSE
  * stream, every event on it — progress notifications, server-initiated requests such as
  * sampling/elicitation, and the final response — is surfaced through the channel, so bidirectional
  * flows work exactly as they do over WebSocket.
  */
object HttpClientTransport:

  /** TextMapUpdater for injecting trace context into HTTP headers */
  private given TextMapUpdater[Headers] with
    def updated(carrier: Headers, key: String, value: String): Headers =
      carrier.put(Header.Raw(CIString(key), value))

  /** Session header name (case-insensitive per MCP spec) */
  private val SessionHeaderName = CIString("Mcp-Session-Id")

  /** Build a [[ClientTransport]] over a caller-supplied http4s `Client[F]`. */
  def apply[F[_]](
      httpClient: Client[F],
      config: HttpTransportConfig[F]
  )(using Async[F])(using tracer: Tracer[F] = Tracer.noop[F]): ClientTransport[F] =
    new ClientTransport[F]:
      def open: CatsResource[F, McpChannel[F]] =
        CatsResource.eval {
          for
            sessionIdRef <- Ref.of[F, Option[String]](None)
            inbox        <- Queue.bounded[F, JsonRpcMessage](config.maxQueueSize)
          yield new HttpChannel[F](httpClient, config, sessionIdRef, inbox, tracer)
        }

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
  final private class HttpChannel[F[_]: Async](
      httpClient: Client[F],
      config: HttpTransportConfig[F],
      sessionIdRef: Ref[F, Option[String]],
      inbox: Queue[F, JsonRpcMessage],
      tracer: Tracer[F]
  ) extends McpChannel[F]:

    private val endpointUri = Uri.unsafeFromString(config.uri)

    def incoming: Stream[F, JsonRpcMessage] =
      Stream.fromQueueUnterminated(inbox)

    def send(message: JsonRpcMessage): F[Unit] =
      message match
        case req: JsonRpcRequest =>
          post(req.asJson).flatMap(drainResponse(req.id, _))
        case other =>
          // Notifications and responses to server-initiated requests expect no correlated
          // reply; POST them and ignore the (usually 202 Accepted) response body.
          post(other.asJson).flatMap(_.use_)

    private def headers: F[Headers] =
      for
        traceHeaders <- tracer.propagate(Headers.empty)
        authed       <- McpAuth.applyTo(
          config.auth,
          traceHeaders.put(
            Header.Raw(CIString("Accept"), "application/json, text/event-stream")
          )
        )
        sessionIdOpt <- sessionIdRef.get
      yield sessionIdOpt.fold(authed)(id => authed.put(Header.Raw(SessionHeaderName, id)))

    private def post(body: Json): F[CatsResource[F, Response[F]]] =
      headers.map { hs =>
        httpClient.run(
          Request[F](method = Method.POST, uri = endpointUri, headers = hs)
            .withEntity(body)
            .withContentType(`Content-Type`(MediaType.application.json))
        )
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
            case None         => Async[F].unit

        val isSse = resp.headers
          .get(CIString("Content-Type"))
          .exists(_.exists(_.value.startsWith("text/event-stream")))

        val consume =
          if !isSse then
            resp.asJson.flatMap { json =>
              json.as[JsonRpcMessage] match
                case Right(message) => inbox.offer(message)
                case Left(err)      =>
                  Async[F].raiseError[Unit](
                    McpError.InternalError(s"Failed to parse response: ${err.getMessage}")
                  )
            }
          else
            resp.body
              .through(ServerSentEvent.decoder[F])
              .evalMapFilter { event =>
                event.data match
                  case None       => Async[F].pure(Option.empty[JsonRpcMessage])
                  case Some(data) =>
                    io.circe.parser.parse(data).flatMap(_.as[JsonRpcMessage]) match
                      case Right(message) => Async[F].pure(Some(message))
                      case Left(err)      =>
                        Async[F].raiseError[Option[JsonRpcMessage]](
                          McpError.InternalError(s"Failed to parse SSE event: $err")
                        )
              }
              .evalTap(inbox.offer)
              .takeThrough {
                case JsonRpcResponse(id, _)      => id != requestId
                case JsonRpcErrorResponse(id, _) => id != requestId
                case _                           => true
              }
              .compile
              .drain

        captureSession *> consume
      }
