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
