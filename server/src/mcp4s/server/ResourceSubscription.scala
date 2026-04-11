package mcp4s.server

import cats.effect.{Concurrent, Ref}
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.Stream

/** Manages resource subscriptions for MCP servers.
  *
  * Tracks which clients (identified by session ID) are subscribed to which resources,
  * and provides methods to notify subscribers when resources change.
  *
  * Example usage:
  * {{{
  * for
  *   manager <- ResourceSubscriptionManager[IO]
  *   _ <- manager.subscribe("session-123", "file:///config.json")
  *   // When resource changes:
  *   _ <- manager.notifyChanged("file:///config.json")
  *   // Notifications are queued for delivery to subscribers
  * yield ()
  * }}}
  */
trait ResourceSubscriptionManager[F[_]]:

  /** Subscribe a session to resource updates.
    *
    * @param sessionId The session to subscribe
    * @param uri The resource URI to subscribe to
    */
  def subscribe(sessionId: String, uri: String): F[Unit]

  /** Unsubscribe a session from resource updates.
    *
    * @param sessionId The session to unsubscribe
    * @param uri The resource URI to unsubscribe from
    */
  def unsubscribe(sessionId: String, uri: String): F[Unit]

  /** Unsubscribe a session from all resources.
    *
    * This should be called when a session is closed.
    *
    * @param sessionId The session to unsubscribe
    */
  def unsubscribeAll(sessionId: String): F[Unit]

  /** Notify all subscribers that a resource has changed.
    *
    * This queues notifications for delivery to all sessions subscribed to the resource.
    *
    * @param uri The URI of the resource that changed
    */
  def notifyChanged(uri: String): F[Unit]

  /** Get all sessions subscribed to a resource.
    *
    * @param uri The resource URI
    * @return Set of session IDs subscribed to the resource
    */
  def getSubscribers(uri: String): F[Set[String]]

  /** Get all resources a session is subscribed to.
    *
    * @param sessionId The session ID
    * @return Set of resource URIs the session is subscribed to
    */
  def getSubscriptions(sessionId: String): F[Set[String]]

  /** Stream of resource update notifications.
    *
    * Returns a stream that emits (sessionId, uri) pairs when resources change.
    * Consumers can use this to send notifications to the appropriate sessions.
    */
  def notifications: Stream[F, (String, String)]

  /** Connect to a Resources change stream.
    *
    * Returns a stream that monitors resource changes and notifies subscribers.
    * This stream should be run concurrently with the server.
    *
    * Example:
    * {{{
    * val resources: Resources[IO] = ...
    * for
    *   manager <- ResourceSubscriptionManager[IO]
    *   _ <- manager.connect(resources).compile.drain.start
    * yield ()
    * }}}
    */
  def connect(resources: Resources[F]): Stream[F, Unit]

object ResourceSubscriptionManager:

  /** Default maximum queue size for resource change notifications */
  val DefaultMaxQueueSize: Int = 1000

  /** Create a new subscription manager with default bounded queue size. */
  def apply[F[_]: Concurrent]: F[ResourceSubscriptionManager[F]] =
    apply(DefaultMaxQueueSize)

  /** Create a new subscription manager.
    *
    * @param maxQueueSize Maximum number of pending notifications (bounded with backpressure)
    */
  def apply[F[_]: Concurrent](maxQueueSize: Int): F[ResourceSubscriptionManager[F]] =
    for
      // Map from resource URI -> Set of subscribed session IDs
      subscriptionsRef <- Ref.of[F, Map[String, Set[String]]](Map.empty)
      // Queue for notifications (sessionId, uri) — bounded with backpressure
      notificationQueue <- Queue.bounded[F, (String, String)](maxQueueSize)
    yield new ResourceSubscriptionManagerImpl(subscriptionsRef, notificationQueue)

  private class ResourceSubscriptionManagerImpl[F[_]: Concurrent](
      subscriptionsRef: Ref[F, Map[String, Set[String]]],
      notificationQueue: Queue[F, (String, String)]
  ) extends ResourceSubscriptionManager[F]:

    def subscribe(sessionId: String, uri: String): F[Unit] =
      subscriptionsRef.update { subs =>
        val current = subs.getOrElse(uri, Set.empty)
        subs + (uri -> (current + sessionId))
      }

    def unsubscribe(sessionId: String, uri: String): F[Unit] =
      subscriptionsRef.update { subs =>
        subs.get(uri) match
          case Some(sessions) =>
            val remaining = sessions - sessionId
            if remaining.isEmpty then subs - uri
            else subs + (uri -> remaining)
          case None => subs
      }

    def unsubscribeAll(sessionId: String): F[Unit] =
      subscriptionsRef.update { subs =>
        subs.map { case (uri, sessions) =>
          uri -> (sessions - sessionId)
        }.filter { case (_, sessions) => sessions.nonEmpty }
      }

    def notifyChanged(uri: String): F[Unit] =
      getSubscribers(uri).flatMap { subscribers =>
        subscribers.toList.traverse_ { sessionId =>
          notificationQueue.offer((sessionId, uri))
        }
      }

    def getSubscribers(uri: String): F[Set[String]] =
      subscriptionsRef.get.map(_.getOrElse(uri, Set.empty))

    def getSubscriptions(sessionId: String): F[Set[String]] =
      subscriptionsRef.get.map { subs =>
        subs.collect {
          case (uri, sessions) if sessions.contains(sessionId) => uri
        }.toSet
      }

    def notifications: Stream[F, (String, String)] =
      Stream.fromQueueUnterminated(notificationQueue)

    def connect(resources: Resources[F]): Stream[F, Unit] =
      resources.changes.evalMap(notifyChanged)
