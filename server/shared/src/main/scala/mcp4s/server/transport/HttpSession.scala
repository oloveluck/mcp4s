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

import cats.effect.{Async, Ref, Resource as CatsResource}
import cats.effect.std.{Queue, SecureRandom, UUIDGen}
import cats.syntax.all.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.server.{Dispatcher, Server}

import scala.concurrent.duration.FiniteDuration

/** One Streamable HTTP session: a [[mcp4s.server.Dispatcher]] with isolated protocol state plus the
  * session's SSE streams.
  *
  * Every in-flight request that was answered with an SSE stream owns a queue of its own, so the
  * messages a tool emits while handling that request (progress, logging, sampling/elicitation) land
  * on that request's stream and nowhere else — the spec's rule that server messages relate to the
  * originating request and are never broadcast. Messages unrelated to a request (subscribed
  * resource updates, or messages emitted after a request's stream closed) go to the standalone
  * stream a client opens with HTTP GET; without one, notifications are dropped and server-initiated
  * requests fail fast instead of waiting for a stream that will never read them.
  */
final class HttpSession[F[_]] private (
    val id: SessionId,
    session: ServerSession[F],
    val createdAt: FiniteDuration,
    lastAccessedRef: Ref[F, FiniteDuration],
    streams: Ref[F, Map[RequestId, HttpSession.Stream[F]]],
    standalone: Ref[F, Option[HttpSession.Stream[F]]],
    streamCounter: Ref[F, Long],
    val config: SessionConfig
)(using F: Async[F]):

  /** The per-session dispatcher. */
  def dispatcher: Dispatcher[F] = session.dispatcher

  /** Route one inbound message (client responses are correlated; requests and notifications are
    * dispatched).
    */
  def handle(message: JsonRpcMessage): F[Option[JsonRpcMessage]] = session.handle(message)

  /** Get the last access time for this session, as a duration since the epoch */
  def lastAccessed: F[FiniteDuration] = lastAccessedRef.get

  /** Update the last access time to now */
  def touch: F[Unit] = F.realTime.flatMap(lastAccessedRef.set)

  /** Check if this session has expired based on the configured timeout */
  def isExpired: F[Boolean] =
    for
      now  <- F.realTime
      last <- lastAccessedRef.get
    yield (now - last) > config.timeout

  /** Get the number of pending requests awaiting responses */
  def pendingRequestCount: F[Int] = session.pendingRequestCount

  /** Whether a standalone (GET) stream is currently attached. */
  def hasStandaloneStream: F[Boolean] = standalone.get.map(_.isDefined)

  /** Shutdown this session: fail pending requests, cancel tasks, and end every open SSE stream.
    * Idempotent.
    */
  def shutdown: F[Unit] =
    session.close *>
      streams.getAndSet(Map.empty).flatMap(_.values.toList.traverse_(_.close)) *>
      standalone.getAndSet(None).flatMap(_.traverse_(_.close))

  /** Open the SSE stream that answers request `reqId`. Released (and deregistered) when the
    * resource is.
    */
  private[transport] def requestStream(reqId: RequestId): CatsResource[F, HttpSession.Stream[F]] =
    CatsResource.make(
      newStream.flatTap(s => streams.update(_ + (reqId -> s)))
    )(s => streams.update(_ - reqId) *> s.close)

  /** Open the standalone (GET) stream, or `None` if one is already attached — a session carries at
    * most one.
    */
  private[transport] def standaloneStream: CatsResource[F, Option[HttpSession.Stream[F]]] =
    CatsResource.make(
      newStream.flatMap { s =>
        standalone.modify {
          case None     => (Some(s), Some(s))
          case existing => (existing, None)
        }
      }
    )(_.traverse_(s => standalone.update(_.filterNot(_ eq s)) *> s.close))

  private def newStream: F[HttpSession.Stream[F]] =
    for
      n     <- streamCounter.updateAndGet(_ + 1)
      queue <- Queue.bounded[F, Option[JsonRpcMessage]](config.maxQueueSize)
    yield HttpSession.Stream(n.toString, queue)

object HttpSession:

  /** An SSE stream: its id (the prefix of every event id it emits) and its message queue. `None`
    * terminates the stream.
    */
  final private[transport] case class Stream[F[_]](
      id: String,
      queue: Queue[F, Option[JsonRpcMessage]]
  ):
    def send(message: JsonRpcMessage): F[Unit] = queue.offer(Some(message))
    def close: F[Unit]                         = queue.offer(None)

  /** Create a new HTTP session with a fresh dispatcher and full bidirectional context support.
    *
    * @param server
    *   The MCP server to create a dispatcher for
    * @param config
    *   Session configuration (optional, defaults to SessionConfig.default)
    * @param dispatcherConfig
    *   Dispatcher settings (re-initialization policy, task retention)
    * @param tracer
    *   OpenTelemetry tracer for distributed tracing
    */
  def create[F[_]: Async](
      server: Server[F],
      config: SessionConfig = SessionConfig.default,
      dispatcherConfig: Dispatcher.Config = Dispatcher.Config.default
  )(using tracer: Tracer[F]): F[HttpSession[F]] =
    for
      id <- SecureRandom
        .javaSecuritySecureRandom[F]
        .flatMap { sr =>
          given SecureRandom[F] = sr
          UUIDGen[F].randomUUID
        }
        .map(uuid => SessionId(uuid.toString))
      streams    <- Ref.of[F, Map[RequestId, Stream[F]]](Map.empty)
      standalone <- Ref.of[F, Option[Stream[F]]](None)
      counter    <- Ref.of[F, Long](0L)

      // Request-related messages prefer the request's own stream; once that stream has closed
      // (e.g. a task still running after its CreateTaskResult was sent) they fall back to the
      // standalone stream, like messages that never related to a request.
      toStandalone = (message: JsonRpcMessage) =>
        standalone.get.flatMap {
          case Some(s) => s.send(message)
          case None    =>
            message match
              case _: JsonRpcRequest =>
                Async[F].raiseError[Unit](
                  McpError.InternalError(
                    "No SSE stream is open to deliver a server-initiated request: the client " +
                      "must accept text/event-stream or keep a GET stream open"
                  )
                )
              case _ => Async[F].unit
        }
      outbound = new Outbound[F]:
        def forRequest(reqId: RequestId): JsonRpcMessage => F[Unit] =
          message =>
            streams.get.map(_.get(reqId)).flatMap {
              case Some(s) => s.send(message)
              case None    => toStandalone(message)
            }
        def standalone: JsonRpcMessage => F[Unit] = toStandalone

      session <- ServerSession.create[F](
        server,
        outbound,
        config.requestTimeout,
        tracer,
        dispatcherConfig
      )
      now             <- Async[F].realTime
      lastAccessedRef <- Ref.of[F, FiniteDuration](now)
    yield new HttpSession[F](
      id,
      session,
      now,
      lastAccessedRef,
      streams,
      standalone,
      counter,
      config
    )
