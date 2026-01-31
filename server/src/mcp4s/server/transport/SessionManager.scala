package mcp4s.server.transport

import cats.effect.{Async, Ref, Resource as CatsResource}
import cats.effect.std.Supervisor
import cats.syntax.all.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.server.McpServer

import scala.concurrent.duration.*

/** Configuration for HTTP sessions */
final case class SessionConfig(
    /** Session timeout - sessions are removed after this duration of inactivity */
    timeout: FiniteDuration = 30.minutes,
    /** Maximum queue size for outgoing messages (bounded queue with backpressure) */
    maxQueueSize: Int = 1000,
    /** Request timeout for pending operations */
    requestTimeout: FiniteDuration = 5.minutes,
    /** Interval between cleanup runs */
    cleanupInterval: FiniteDuration = 1.minute
)

object SessionConfig:
  val default: SessionConfig = SessionConfig()

/** Manages HTTP sessions for the MCP Streamable HTTP transport.
  *
  * Provides thread-safe session creation, lookup, removal, and automatic cleanup
  * of expired sessions.
  */
trait SessionManager[F[_]]:

  /** Get an existing session by ID */
  def get(sessionId: String): F[Option[HttpSession[F]]]

  /** Create a new session */
  def create: F[HttpSession[F]]

  /** Remove a session by ID */
  def remove(sessionId: String): F[Unit]

  /** Remove all expired sessions.
    * @return The number of sessions that were removed
    */
  def pruneExpired: F[Int]

  /** Get the current number of active sessions */
  def sessionCount: F[Int]

object SessionManager:

  /** Create a new SessionManager backed by a concurrent map.
    *
    * @param server The MCP server to create dispatchers for
    * @param config Session configuration
    * @param tracer OpenTelemetry tracer for distributed tracing
    */
  def apply[F[_]: Async](
      server: McpServer[F],
      config: SessionConfig = SessionConfig.default
  )(using Tracer[F]): F[SessionManager[F]] =
    Ref.of[F, Map[String, HttpSession[F]]](Map.empty).map { sessionsRef =>
      new SessionManagerImpl(server, config, sessionsRef)
    }

  /** Create a SessionManager as a Resource with automatic cleanup loop.
    *
    * The cleanup loop runs in the background and removes expired sessions
    * at the configured interval. The loop is cancelled when the resource is released.
    *
    * @param server The MCP server to create dispatchers for
    * @param config Session configuration
    * @param tracer OpenTelemetry tracer for distributed tracing
    */
  def withCleanup[F[_]: Async](
      server: McpServer[F],
      config: SessionConfig = SessionConfig.default
  )(using Tracer[F]): CatsResource[F, SessionManager[F]] =
    for
      manager <- CatsResource.eval(apply[F](server, config))
      supervisor <- Supervisor[F]
      _ <- CatsResource.make(
        supervisor.supervise(cleanupLoop(manager, config.cleanupInterval))
      )(_.cancel)
    yield manager

  /** Continuously prune expired sessions at the given interval */
  private def cleanupLoop[F[_]: Async](manager: SessionManager[F], interval: FiniteDuration): F[Unit] =
    (Async[F].sleep(interval) *> manager.pruneExpired).foreverM

  private class SessionManagerImpl[F[_]: Async](
      server: McpServer[F],
      config: SessionConfig,
      sessionsRef: Ref[F, Map[String, HttpSession[F]]]
  )(using Tracer[F]) extends SessionManager[F]:

    def get(sessionId: String): F[Option[HttpSession[F]]] =
      sessionsRef.get.map(_.get(sessionId)).flatMap {
        case Some(session) =>
          session.touch.as(Some(session))
        case None =>
          Async[F].pure(None)
      }

    def create: F[HttpSession[F]] =
      HttpSession.create[F](server, config).flatMap { session =>
        sessionsRef.update(_ + (session.id -> session)).as(session)
      }

    def remove(sessionId: String): F[Unit] =
      sessionsRef.modify { sessions =>
        sessions.get(sessionId) match
          case Some(session) => (sessions - sessionId, Some(session))
          case None          => (sessions, None)
      }.flatMap(_.traverse_(_.shutdown))

    def pruneExpired: F[Int] =
      for
        // Get all sessions and check which are expired
        sessions <- sessionsRef.get
        expiredIds <- sessions.toList.traverseFilter { case (id, session) =>
          session.isExpired.map(if _ then Some(id) else None)
        }
        // Remove and shutdown expired sessions
        _ <- expiredIds.traverse_(remove)
      yield expiredIds.size

    def sessionCount: F[Int] =
      sessionsRef.get.map(_.size)
