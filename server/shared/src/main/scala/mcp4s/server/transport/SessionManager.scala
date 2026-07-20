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
import cats.effect.std.Supervisor
import cats.syntax.all.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.McpError
import mcp4s.server.Server

import scala.concurrent.duration.*

/** Configuration for HTTP sessions */
final case class SessionConfig(
    /** Session timeout - sessions are removed after this duration of inactivity */
    timeout: FiniteDuration = 30.minutes,
    /** Maximum queue size for outgoing messages (bounded queue with backpressure) */
    maxQueueSize: Int = 1000,
    /** Request timeout for pending operations */
    requestTimeout: FiniteDuration = 5.minutes,
    /** Maximum number of concurrent sessions. Rejects new sessions when at capacity. */
    maxSessions: Int = 1000
)

object SessionConfig:
  val default: SessionConfig = SessionConfig()

/** Identifier of an HTTP session, carried in the `Mcp-Session-Id` header. */
opaque type SessionId = String

object SessionId:
  def apply(value: String): SessionId = value

  extension (id: SessionId)
    /** The raw header value. */
    def value: String = id

/** Manages HTTP sessions for the MCP Streamable HTTP transport.
  *
  * Provides thread-safe session creation, lookup, removal, and automatic cleanup of expired
  * sessions.
  */
trait SessionManager[F[_]]:

  /** Get an existing session by ID */
  def get(sessionId: SessionId): F[Option[HttpSession[F]]]

  /** Create a new session */
  def create: F[HttpSession[F]]

  /** Remove a session by ID */
  def remove(sessionId: SessionId): F[Unit]

  /** Remove all expired sessions.
    * @return
    *   The number of sessions that were removed
    */
  def pruneExpired: F[Int]

  /** Get the current number of active sessions */
  def sessionCount: F[Int]

object SessionManager:

  /** Create a new SessionManager backed by a concurrent map.
    *
    * @param server
    *   The MCP server to create dispatchers for
    * @param config
    *   Session configuration
    * @param tracer
    *   OpenTelemetry tracer for distributed tracing
    */
  def apply[F[_]: Async](
      server: Server[F],
      config: SessionConfig = SessionConfig.default
  )(using Tracer[F]): F[SessionManager[F]] =
    Ref
      .of[F, Map[SessionId, HttpSession[F]]](Map.empty)
      .map: sessionsRef =>
        new SessionManagerImpl(server, config, sessionsRef)

  /** Create a SessionManager as a Resource with automatic cleanup loop.
    *
    * The cleanup loop runs in the background and removes expired sessions at the configured
    * interval. The loop is cancelled when the resource is released.
    *
    * @param server
    *   The MCP server to create dispatchers for
    * @param config
    *   Session configuration
    * @param tracer
    *   OpenTelemetry tracer for distributed tracing
    */
  def withCleanup[F[_]: Async](
      server: Server[F],
      config: SessionConfig = SessionConfig.default
  )(using Tracer[F]): CatsResource[F, SessionManager[F]] =
    val cleanupInterval = (config.timeout / 30).max(10.seconds).min(5.minutes)
    for
      manager    <- CatsResource.eval(apply[F](server, config))
      supervisor <- Supervisor[F]
      _          <- CatsResource.make(
        supervisor.supervise(cleanupLoop(manager, cleanupInterval))
      )(_.cancel)
    yield manager

  /** Continuously prune expired sessions at the given interval */
  private def cleanupLoop[F[_]: Async](
      manager: SessionManager[F],
      interval: FiniteDuration
  ): F[Unit] =
    (Async[F].sleep(interval) *> manager.pruneExpired).foreverM

  private class SessionManagerImpl[F[_]: Async](
      server: Server[F],
      config: SessionConfig,
      sessionsRef: Ref[F, Map[SessionId, HttpSession[F]]]
  )(using Tracer[F])
      extends SessionManager[F]:

    def get(sessionId: SessionId): F[Option[HttpSession[F]]] =
      sessionsRef.get.map(_.get(sessionId)).flatMap {
        case Some(session) =>
          session.touch.as(Some(session))
        case None =>
          Async[F].pure(None)
      }

    def create: F[HttpSession[F]] =
      val limitReached: F[HttpSession[F]] = Async[F].raiseError(
        McpError.InternalError(s"Maximum session limit (${config.maxSessions}) reached")
      )
      // Cheap pre-check, then re-check atomically at insert time: two concurrent
      // creates must not both pass the cap (check-then-act race).
      sessionsRef.get.flatMap: sessions =>
        if sessions.size >= config.maxSessions then limitReached
        else
          HttpSession
            .create[F](server, config)
            .flatMap: session =>
              sessionsRef
                .modify: current =>
                  if current.size >= config.maxSessions then (current, false)
                  else (current + (session.id -> session), true)
                .flatMap:
                  case true  => session.pure[F]
                  case false => session.shutdown *> limitReached

    def remove(sessionId: SessionId): F[Unit] =
      sessionsRef
        .modify { sessions =>
          sessions.get(sessionId) match
            case Some(session) => (sessions - sessionId, Some(session))
            case None          => (sessions, None)
        }
        .flatMap(_.traverse_(_.shutdown))

    def pruneExpired: F[Int] =
      for
        // Get all sessions and check which are expired
        sessions   <- sessionsRef.get
        expiredIds <- sessions.toList.traverseFilter((id, session) =>
          session.isExpired.map(if _ then Some(id) else None)
        )
        // Remove and shutdown expired sessions
        _ <- expiredIds.traverse_(remove)
      yield expiredIds.size

    def sessionCount: F[Int] =
      sessionsRef.get.map(_.size)
