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

package mcp4s.server

import cats.effect.IO
import cats.syntax.semigroup.*
import fs2.concurrent.SignallingRef
import mcp4s.protocol.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

class ResourceSubscriptionSpec extends CatsEffectSuite:

  // === ResourceSubscriptionManager Tests ===

  test("subscribe adds session to resource subscribers") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _       <- manager.subscribe("session-1", "file:///test.txt")
      subs    <- manager.getSubscribers("file:///test.txt")
      _ = assertEquals(subs, Set("session-1"))
    yield ()
  }

  test("subscribe allows multiple sessions per resource") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _       <- manager.subscribe("session-1", "file:///test.txt")
      _       <- manager.subscribe("session-2", "file:///test.txt")
      _       <- manager.subscribe("session-3", "file:///test.txt")
      subs    <- manager.getSubscribers("file:///test.txt")
      _ = assertEquals(subs, Set("session-1", "session-2", "session-3"))
    yield ()
  }

  test("subscribe allows session to subscribe to multiple resources") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _       <- manager.subscribe("session-1", "file:///a.txt")
      _       <- manager.subscribe("session-1", "file:///b.txt")
      _       <- manager.subscribe("session-1", "file:///c.txt")
      subs    <- manager.getSubscriptions("session-1")
      _ = assertEquals(subs, Set("file:///a.txt", "file:///b.txt", "file:///c.txt"))
    yield ()
  }

  test("unsubscribe removes session from resource") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _       <- manager.subscribe("session-1", "file:///test.txt")
      _       <- manager.subscribe("session-2", "file:///test.txt")
      _       <- manager.unsubscribe("session-1", "file:///test.txt")
      subs    <- manager.getSubscribers("file:///test.txt")
      _ = assertEquals(subs, Set("session-2"))
    yield ()
  }

  test("unsubscribe cleans up empty resource entries") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _       <- manager.subscribe("session-1", "file:///test.txt")
      _       <- manager.unsubscribe("session-1", "file:///test.txt")
      subs    <- manager.getSubscribers("file:///test.txt")
      _ = assertEquals(subs, Set.empty)
    yield ()
  }

  test("unsubscribeAll removes session from all resources") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _       <- manager.subscribe("session-1", "file:///a.txt")
      _       <- manager.subscribe("session-1", "file:///b.txt")
      _       <- manager.subscribe("session-2", "file:///a.txt")
      _       <- manager.unsubscribeAll("session-1")
      subsA   <- manager.getSubscribers("file:///a.txt")
      subsB   <- manager.getSubscribers("file:///b.txt")
      _ = assertEquals(subsA, Set("session-2"))
      _ = assertEquals(subsB, Set.empty)
    yield ()
  }

  test("notifyChanged queues notifications for subscribers") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _       <- manager.subscribe("session-1", "file:///test.txt")
      _       <- manager.subscribe("session-2", "file:///test.txt")
      _       <- manager.notifyChanged("file:///test.txt")
      // Collect notifications (with timeout to avoid hanging)
      notifications <- manager.notifications
        .take(2)
        .compile
        .toList
        .timeout(1.second)
      _ = assertEquals(
        notifications.toSet,
        Set(
          ("session-1", "file:///test.txt"),
          ("session-2", "file:///test.txt")
        )
      )
    yield ()
  }

  test("notifyChanged does nothing for unsubscribed resources") {
    for
      manager <- ResourceSubscriptionManager[IO]
      _       <- manager.notifyChanged("file:///unknown.txt")
      // Should not hang - queue should be empty
      result <- manager.notifications
        .take(1)
        .compile
        .toList
        .timeout(100.millis)
        .attempt
      _ = assert(result.isLeft) // Should timeout because no notifications
    yield ()
  }

  test("getSubscriptions returns empty set for unknown session") {
    for
      manager <- ResourceSubscriptionManager[IO]
      subs    <- manager.getSubscriptions("unknown-session")
      _ = assertEquals(subs, Set.empty)
    yield ()
  }

  // === Subscribable Resource Tests ===

  test("McpResource.subscribable creates resource with change stream") {
    for
      signal <- SignallingRef[IO, Boolean](false)

      resources = McpResource.subscribable[IO](
        "file:///config.json",
        "Config",
        signal.discrete.filter(identity).as(())
      ) { _ =>
        IO.pure(ResourceContent.text("file:///config.json", """{"key": "value"}"""))
      }

      // Test resource listing
      resourceList <- resources.list
      _ = assertEquals(resourceList.size, 1)
      _ = assertEquals(resourceList.head.uri, "file:///config.json")

      // Test read
      content <- resources.read("file:///config.json").value
      _ = assertEquals(content.flatMap(_.text), Some("""{"key": "value"}"""))
    yield ()
  }

  test("manager.connect notifies subscribers on resource change") {
    for
      manager      <- ResourceSubscriptionManager[IO]
      changeSignal <- SignallingRef[IO, Boolean](false)

      resources = McpResource.subscribable[IO](
        "file:///watched.txt",
        "Watched",
        changeSignal.discrete.filter(identity).as(())
      )(_ => IO.pure(ResourceContent.text("file:///watched.txt", "content")))

      _ <- manager.subscribe("session-1", "file:///watched.txt")

      // Start monitoring in background
      monitorFiber <- manager.connect(resources).compile.drain.start

      // Trigger change
      _ <- changeSignal.set(true)
      _ <- IO.sleep(50.millis)

      // Check notification was sent
      notif <- manager.notifications.take(1).compile.toList.timeout(500.millis)
      _ = assertEquals(notif, List(("session-1", "file:///watched.txt")))

      _ <- monitorFiber.cancel
    yield ()
  }

  test("static resources have empty changes stream") {
    val resource = McpResource[IO]("file:///static", "Static")("content")

    for
      // Stream is empty, so compile.toList completes immediately with Nil; the
      // generous timeout only bounds the hanging-stream case on a loaded runner.
      result <- resource.changes.compile.toList.timeout(2.seconds).attempt
      _ = assertEquals(result, Right(Nil))
    yield ()
  }

  test("composed resources merge change streams") {
    for
      signal1 <- SignallingRef[IO, Boolean](false)
      signal2 <- SignallingRef[IO, Boolean](false)

      res1 = McpResource.subscribable[IO](
        "file:///a.txt",
        "A",
        signal1.discrete.filter(identity).as(())
      )(_ => IO.pure(ResourceContent.text("file:///a.txt", "a")))

      res2 = McpResource.subscribable[IO](
        "file:///b.txt",
        "B",
        signal2.discrete.filter(identity).as(())
      )(_ => IO.pure(ResourceContent.text("file:///b.txt", "b")))

      combined = res1 |+| res2

      // Collect changes in background
      changesRef   <- cats.effect.Ref.of[IO, List[String]](Nil)
      collectFiber <- combined.changes
        .evalMap(uri => changesRef.update(_ :+ uri))
        .compile
        .drain
        .start

      // Trigger both
      _ <- signal1.set(true)
      _ <- signal2.set(true)

      // Wait until both changes are observed (with timeout to avoid hanging)
      changes <- changesRef.get
        .iterateUntil(c => c.contains("file:///a.txt") && c.contains("file:///b.txt"))
        .timeout(5.seconds)
      _ = assert(changes.contains("file:///a.txt"))
      _ = assert(changes.contains("file:///b.txt"))

      _ <- collectFiber.cancel
    yield ()
  }

  // === SubscriptionTypes Tests ===

  test("SubscribeResourceParams encodes/decodes correctly") {
    import io.circe.syntax.*
    import io.circe.parser.decode

    val params  = SubscribeResourceParams("file:///test.txt")
    val json    = params.asJson
    val decoded = decode[SubscribeResourceParams](json.noSpaces)

    assertEquals(decoded, Right(params))
  }

  test("UnsubscribeResourceParams encodes/decodes correctly") {
    import io.circe.syntax.*
    import io.circe.parser.decode

    val params  = UnsubscribeResourceParams("file:///test.txt")
    val json    = params.asJson
    val decoded = decode[UnsubscribeResourceParams](json.noSpaces)

    assertEquals(decoded, Right(params))
  }

  test("ResourceUpdatedNotification encodes/decodes correctly") {
    import io.circe.syntax.*
    import io.circe.parser.decode

    val notif   = ResourceUpdatedNotification("file:///changed.txt")
    val json    = notif.asJson
    val decoded = decode[ResourceUpdatedNotification](json.noSpaces)

    assertEquals(decoded, Right(notif))
  }
