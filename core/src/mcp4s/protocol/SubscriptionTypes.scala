package mcp4s.protocol

import io.circe.*
import io.circe.generic.semiauto.*

/** Parameters for subscribing to resource changes.
  *
  * When a client subscribes to a resource, it will receive notifications
  * whenever the resource content changes.
  */
final case class SubscribeResourceParams(
    /** The URI of the resource to subscribe to */
    uri: String
)

object SubscribeResourceParams:
  given Encoder[SubscribeResourceParams] = deriveEncoder[SubscribeResourceParams]
  given Decoder[SubscribeResourceParams] = deriveDecoder[SubscribeResourceParams]

/** Parameters for unsubscribing from resource changes. */
final case class UnsubscribeResourceParams(
    /** The URI of the resource to unsubscribe from */
    uri: String
)

object UnsubscribeResourceParams:
  given Encoder[UnsubscribeResourceParams] = deriveEncoder[UnsubscribeResourceParams]
  given Decoder[UnsubscribeResourceParams] = deriveDecoder[UnsubscribeResourceParams]

/** Notification sent when a subscribed resource has been updated.
  *
  * The client should re-read the resource to get the latest content.
  */
final case class ResourceUpdatedNotification(
    /** The URI of the resource that was updated */
    uri: String
)

object ResourceUpdatedNotification:
  given Encoder[ResourceUpdatedNotification] = deriveEncoder[ResourceUpdatedNotification]
  given Decoder[ResourceUpdatedNotification] = deriveDecoder[ResourceUpdatedNotification]

/** Notification sent when a resource has been added to the server.
  *
  * This allows clients to discover new resources without polling.
  */
final case class ResourceListChangedNotification()

object ResourceListChangedNotification:
  given Encoder[ResourceListChangedNotification] = Encoder.instance(_ => Json.obj())
  given Decoder[ResourceListChangedNotification] = Decoder.instance(_ => Right(ResourceListChangedNotification()))

/** MCP method names for resource subscription operations */
object SubscriptionMethods:
  /** Method to subscribe to resource updates */
  val Subscribe: String = "resources/subscribe"

  /** Method to unsubscribe from resource updates */
  val Unsubscribe: String = "resources/unsubscribe"

  /** Notification method for resource updates */
  val ResourceUpdated: String = "notifications/resources/updated"

  /** Notification method for resource list changes */
  val ResourceListChanged: String = "notifications/resources/list_changed"
