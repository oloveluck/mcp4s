package mcp4s.server

import cats.effect.{Concurrent, Ref}
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.Stream
import mcp4s.protocol.*

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

object ResourceSubscriptionManager:

  /** Create a new subscription manager. */
  def apply[F[_]: Concurrent]: F[ResourceSubscriptionManager[F]] =
    for
      // Map from resource URI -> Set of subscribed session IDs
      subscriptionsRef <- Ref.of[F, Map[String, Set[String]]](Map.empty)
      // Queue for notifications (sessionId, uri)
      notificationQueue <- Queue.unbounded[F, (String, String)]
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

/** A subscribable resource that emits change notifications.
  *
  * Use this trait to create resources that notify subscribers when their content changes.
  *
  * Example:
  * {{{
  * val watchedFile = McpSubscribableResource.fromStream[IO](
  *   uri = "file:///config.json",
  *   name = "Configuration",
  *   onChange = fileWatcher.events.filter(_ == "config.json").void
  * ) { _ =>
  *   IO(ResourceContent.text("file:///config.json", readFile("/config.json")))
  * }
  * }}}
  */
trait McpSubscribableResource[F[_]]:
  /** The URI of this resource */
  def uri: String

  /** Stream that emits () when the resource changes */
  def onChange: Stream[F, Unit]

/** Factory for creating subscribable resources */
object McpSubscribableResource:

  /** Create a subscribable resource from a change stream.
    *
    * @param resourceUri The resource URI
    * @param name The resource name
    * @param description Optional description
    * @param changeStream Stream that emits when the resource content changes
    * @param handler Handler to read the resource content
    */
  def apply[F[_]: Concurrent](
      resourceUri: String,
      name: String,
      description: String = "",
      changeStream: Stream[F, Unit]
  )(handler: String => F[ResourceContent]): (McpResources[F], McpSubscribableResource[F]) =
    val resources = McpResource.handler[F](resourceUri, name) { _ => handler(resourceUri) }
    val subscribable = new McpSubscribableResource[F]:
      def uri: String = resourceUri
      def onChange: Stream[F, Unit] = changeStream

    (resources, subscribable)

  /** Create a subscribable resource that polls for changes.
    *
    * The resource will be checked periodically and notifications sent if changed.
    *
    * @param uri The resource URI
    * @param name The resource name
    * @param pollInterval How often to check for changes
    * @param hasChanged Function that returns true if the resource has changed since last check
    * @param handler Handler to read the resource content
    */
  def polling[F[_]: cats.effect.Temporal](
      resourceUri: String,
      name: String,
      pollInterval: scala.concurrent.duration.FiniteDuration,
      hasChanged: F[Boolean]
  )(handler: String => F[ResourceContent]): (McpResources[F], McpSubscribableResource[F]) =
    val changeStream = Stream
      .awakeEvery[F](pollInterval)
      .evalFilter(_ => hasChanged)
      .void

    apply(resourceUri, name, "", changeStream)(handler)

/** Extension to combine subscribable resources with subscription manager */
object ResourceSubscriptionOps:

  /** Connect a subscribable resource to a subscription manager.
    *
    * Returns a stream that monitors the resource and notifies subscribers when it changes.
    * This stream should be run concurrently with the server.
    */
  def connectSubscription[F[_]: Concurrent](
      resource: McpSubscribableResource[F],
      manager: ResourceSubscriptionManager[F]
  ): Stream[F, Unit] =
    resource.onChange.evalMap { _ =>
      manager.notifyChanged(resource.uri)
    }

  /** Connect multiple subscribable resources to a subscription manager. */
  def connectSubscriptions[F[_]: Concurrent](
      resources: List[McpSubscribableResource[F]],
      manager: ResourceSubscriptionManager[F]
  ): Stream[F, Unit] =
    Stream.emits(resources).map(r => connectSubscription(r, manager)).parJoinUnbounded
