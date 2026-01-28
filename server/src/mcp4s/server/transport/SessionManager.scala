package mcp4s.server.transport

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.server.McpServer

/** Manages HTTP sessions for the MCP Streamable HTTP transport.
  *
  * Provides thread-safe session creation, lookup, and removal.
  */
trait SessionManager[F[_]]:

  /** Get an existing session by ID */
  def get(sessionId: String): F[Option[HttpSession[F]]]

  /** Create a new session */
  def create: F[HttpSession[F]]

  /** Remove a session by ID */
  def remove(sessionId: String): F[Unit]

object SessionManager:

  /** Create a new SessionManager backed by a concurrent map.
    *
    * @param server The MCP server to create dispatchers for
    * @param tracer OpenTelemetry tracer for distributed tracing
    */
  def apply[F[_]: Async](server: McpServer[F])(using Tracer[F]): F[SessionManager[F]] =
    Ref.of[F, Map[String, HttpSession[F]]](Map.empty).map { sessionsRef =>
      new SessionManager[F]:

        def get(sessionId: String): F[Option[HttpSession[F]]] =
          sessionsRef.get.map(_.get(sessionId)).flatMap {
            case Some(session) =>
              session.touch.as(Some(session))
            case None =>
              Async[F].pure(None)
          }

        def create: F[HttpSession[F]] =
          HttpSession.create[F](server).flatMap { session =>
            sessionsRef.update(_ + (session.id -> session)).as(session)
          }

        def remove(sessionId: String): F[Unit] =
          sessionsRef.update(_ - sessionId)
    }
