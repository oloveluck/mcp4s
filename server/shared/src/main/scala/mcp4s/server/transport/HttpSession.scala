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

import cats.effect.{Async, Ref}
import cats.effect.std.{Queue, SecureRandom, UUIDGen}
import cats.syntax.all.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.server.*

import scala.concurrent.duration.FiniteDuration

/** Represents an HTTP session for MCP Streamable HTTP transport.
  *
  * Each session has its own Dispatcher instance, providing isolated state for initialization and
  * request handling per client. The bidirectional machinery (correlation, capability gating,
  * sampling/elicitation requesters) lives in the shared [[ServerSession]]; this class adds the
  * HTTP-specific parts: session id, expiry tracking, and the SSE out-queue.
  */
final class HttpSession[F[_]] private (
    val id: String,
    val dispatcher: Dispatcher[F],
    val createdAt: FiniteDuration,
    lastAccessedRef: Ref[F, FiniteDuration],
    val outQueue: Queue[F, JsonRpcMessage],
    session: ServerSession[F],
    val config: SessionConfig,
    /** Progress tokens of the streaming requests currently in flight on this session. The out-queue
      * is shared, so each SSE poller uses this to hand a foreign stream's progress events back
      * instead of emitting (and possibly losing) them.
      */
    val activeProgressTokens: Ref[F, Set[RequestId]]
)(using F: Async[F]):

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

  /** Shutdown this session, completing any pending requests with an error */
  def shutdown: F[Unit] = session.cancelPending

  /** Get the number of pending requests awaiting responses */
  def pendingRequestCount: F[Int] = session.pendingRequestCount

  /** Get the current queue size */
  def queueSize: F[Int] = outQueue.size

  /** Extract client capabilities from initialize request */
  def maybeExtractClientCaps(message: JsonRpcMessage): F[Unit] =
    session.maybeExtractClientCaps(message)

  /** Handle a response from the client to a server-initiated request */
  def handleResponse(resp: JsonRpcResponse): F[Unit] = session.handleResponse(resp)

  /** Handle an error response from the client to a server-initiated request */
  def handleErrorResponse(resp: JsonRpcErrorResponse): F[Unit] = session.handleErrorResponse(resp)

object HttpSession:

  /** Create a new HTTP session with a fresh Dispatcher and full context support.
    *
    * @param server
    *   The MCP server to create a dispatcher for
    * @param config
    *   Session configuration (optional, defaults to SessionConfig.default)
    * @param tracer
    *   OpenTelemetry tracer for distributed tracing
    */
  def create[F[_]: Async](
      server: Server[F],
      config: SessionConfig = SessionConfig.default
  )(using tracer: Tracer[F]): F[HttpSession[F]] =
    for
      id <- SecureRandom
        .javaSecuritySecureRandom[F]
        .flatMap { sr =>
          given SecureRandom[F] = sr
          UUIDGen[F].randomUUID
        }
        .map(_.toString)
      // Use bounded queue with configurable max size for backpressure
      outQueue        <- Queue.bounded[F, JsonRpcMessage](config.maxQueueSize)
      session         <- ServerSession.create[F](outQueue.offer, config.requestTimeout, tracer)
      now             <- Async[F].realTime
      lastAccessedRef <- Ref.of[F, FiniteDuration](now)
      activeTokens    <- Ref.of[F, Set[RequestId]](Set.empty)

      // Context factory with full capabilities; progressToken from _meta is used for
      // progress notifications when provided.
      contextFactory = (reqId: RequestId, progressToken: Option[RequestId]) =>
        ToolContext[F](
          session.samplingRequester,
          session.elicitationRequester,
          reqId,
          progressToken,
          session.sendProgressNotification,
          session.sendLoggingNotification
        )

      dispatcher <- Dispatcher.withContext[F](server, contextFactory)
    yield new HttpSession[F](
      id,
      dispatcher,
      now,
      lastAccessedRef,
      outQueue,
      session,
      config,
      activeTokens
    )
