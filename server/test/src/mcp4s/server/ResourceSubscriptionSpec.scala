package mcp4s.server

import cats.effect.IO
import fs2.concurrent.SignallingRef
import mcp4s.protocol.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

class ResourceSubscriptionSpec extends CatsEffectSuite:

  // === ResourceSubscriptionManager Tests ===

  test("subscribe adds session to resource subscribers") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _ <- manager.subscribe("session-1", "file:///test.txt")
      subs <- manager.getSubscribers("file:///test.txt")
      _ = assertEquals(subs, Set("session-1"))
    yield ()
  }

  test("subscribe allows multiple sessions per resource") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _ <- manager.subscribe("session-1", "file:///test.txt")
      _ <- manager.subscribe("session-2", "file:///test.txt")
      _ <- manager.subscribe("session-3", "file:///test.txt")
      subs <- manager.getSubscribers("file:///test.txt")
      _ = assertEquals(subs, Set("session-1", "session-2", "session-3"))
    yield ()
  }

  test("subscribe allows session to subscribe to multiple resources") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _ <- manager.subscribe("session-1", "file:///a.txt")
      _ <- manager.subscribe("session-1", "file:///b.txt")
      _ <- manager.subscribe("session-1", "file:///c.txt")
      subs <- manager.getSubscriptions("session-1")
      _ = assertEquals(subs, Set("file:///a.txt", "file:///b.txt", "file:///c.txt"))
    yield ()
  }

  test("unsubscribe removes session from resource") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _ <- manager.subscribe("session-1", "file:///test.txt")
      _ <- manager.subscribe("session-2", "file:///test.txt")
      _ <- manager.unsubscribe("session-1", "file:///test.txt")
      subs <- manager.getSubscribers("file:///test.txt")
      _ = assertEquals(subs, Set("session-2"))
    yield ()
  }

  test("unsubscribe cleans up empty resource entries") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _ <- manager.subscribe("session-1", "file:///test.txt")
      _ <- manager.unsubscribe("session-1", "file:///test.txt")
      subs <- manager.getSubscribers("file:///test.txt")
      _ = assertEquals(subs, Set.empty)
    yield ()
  }

  test("unsubscribeAll removes session from all resources") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _ <- manager.subscribe("session-1", "file:///a.txt")
      _ <- manager.subscribe("session-1", "file:///b.txt")
      _ <- manager.subscribe("session-2", "file:///a.txt")
      _ <- manager.unsubscribeAll("session-1")
      subsA <- manager.getSubscribers("file:///a.txt")
      subsB <- manager.getSubscribers("file:///b.txt")
      _ = assertEquals(subsA, Set("session-2"))
      _ = assertEquals(subsB, Set.empty)
    yield ()
  }

  test("notifyChanged queues notifications for subscribers") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _ <- manager.subscribe("session-1", "file:///test.txt")
      _ <- manager.subscribe("session-2", "file:///test.txt")
      _ <- manager.notifyChanged("file:///test.txt")
      // Collect notifications (with timeout to avoid hanging)
      notifications <- manager.notifications
        .take(2)
        .compile.toList
        .timeout(1.second)
      _ = assertEquals(notifications.toSet, Set(
        ("session-1", "file:///test.txt"),
        ("session-2", "file:///test.txt")
      ))
    yield ()
  }

  test("notifyChanged does nothing for unsubscribed resources") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _ <- manager.notifyChanged("file:///unknown.txt")
      // Should not hang - queue should be empty
      result <- manager.notifications
        .take(1)
        .compile.toList
        .timeout(100.millis)
        .attempt
      _ = assert(result.isLeft) // Should timeout because no notifications
    yield ()
  }

  test("getSubscriptions returns empty set for unknown session") {
    for
      manager <- ResourceSubscriptionManager[IO]
      subs <- manager.getSubscriptions("unknown-session")
      _ = assertEquals(subs, Set.empty)
    yield ()
  }

  // === McpSubscribableResource Tests ===

  test("McpSubscribableResource creates resource and subscribable pair") {
    for
      signal <- SignallingRef[IO, Boolean](false)
      pair = McpSubscribableResource[IO](
        "file:///config.json",
        "Config",
        "",
        signal.discrete.filter(identity).as(())
      ) { _ =>
        IO.pure(ResourceContent.text("file:///config.json", """{"key": "value"}"""))
      }
      (resources, subscribable) = pair

      // Test resource
      resourceList <- resources.list
      _ = assertEquals(resourceList.size, 1)
      _ = assertEquals(resourceList.head.uri, "file:///config.json")

      // Test subscribable
      _ = assertEquals(subscribable.uri, "file:///config.json")
    yield ()
  }

  test("ResourceSubscriptionOps.connectSubscription notifies manager on change") {
    for
      manager <- ResourceSubscriptionManager[IO]
      changeSignal <- SignallingRef[IO, Boolean](false)

      pair = McpSubscribableResource[IO](
        "file:///watched.txt",
        "Watched",
        "",
        changeSignal.discrete.filter(identity).as(())
      ) { _ => IO.pure(ResourceContent.text("file:///watched.txt", "content")) }
      (_, subscribable) = pair

      _ <- manager.subscribe("session-1", "file:///watched.txt")

      // Start monitoring in background
      monitorFiber <- ResourceSubscriptionOps
        .connectSubscription(subscribable, manager)
        .compile.drain
        .start

      // Trigger change
      _ <- changeSignal.set(true)
      _ <- IO.sleep(50.millis)

      // Check notification was sent
      notif <- manager.notifications.take(1).compile.toList.timeout(500.millis)
      _ = assertEquals(notif, List(("session-1", "file:///watched.txt")))

      _ <- monitorFiber.cancel
    yield ()
  }

  // === SubscriptionTypes Tests ===

  test("SubscribeResourceParams encodes/decodes correctly") {
    import io.circe.syntax.*
    import io.circe.parser.decode

    val params = SubscribeResourceParams("file:///test.txt")
    val json = params.asJson
    val decoded = decode[SubscribeResourceParams](json.noSpaces)

    assertEquals(decoded, Right(params))
  }

  test("UnsubscribeResourceParams encodes/decodes correctly") {
    import io.circe.syntax.*
    import io.circe.parser.decode

    val params = UnsubscribeResourceParams("file:///test.txt")
    val json = params.asJson
    val decoded = decode[UnsubscribeResourceParams](json.noSpaces)

    assertEquals(decoded, Right(params))
  }

  test("ResourceUpdatedNotification encodes/decodes correctly") {
    import io.circe.syntax.*
    import io.circe.parser.decode

    val notif = ResourceUpdatedNotification("file:///changed.txt")
    val json = notif.asJson
    val decoded = decode[ResourceUpdatedNotification](json.noSpaces)

    assertEquals(decoded, Right(notif))
  }
