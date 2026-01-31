package mcp4s.server

import cats.effect.{IO, Ref, Resource as CatsResource}
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.server.asResource
import munit.CatsEffectSuite

class LifecycleSpec extends CatsEffectSuite:

  // === McpLifecycleTool Tests ===

  test("McpLifecycleTool.apply creates tool with managed resource") {
    case class CounterArgs(increment: Int) derives ToolInput

    for
      // Track whether resource was acquired/released
      acquiredRef <- Ref.of[IO, Boolean](false)
      releasedRef <- Ref.of[IO, Boolean](false)
      counterRef <- Ref.of[IO, Int](0)

      // Resource that tracks acquisition/release
      managedResource = CatsResource.make(
        acquiredRef.set(true).as(counterRef)
      )(_ => releasedRef.set(true))

      // Create lifecycle tool
      toolResource = McpLifecycleTool[IO, CounterArgs, Ref[IO, Int]](
        "increment",
        "Increment counter",
        managedResource
      ) { (args, counter) =>
        counter.update(_ + args.increment) *>
          counter.get.map(v => ToolResult.text(s"Counter: $v"))
      }

      // Use the tool within the resource scope
      result <- toolResource.use { tools =>
        for
          acquired <- acquiredRef.get
          _ = assert(acquired, "Resource should be acquired")
          result1 <- tools.call("increment", Json.obj("increment" -> 5.asJson)).value
          result2 <- tools.call("increment", Json.obj("increment" -> 3.asJson)).value
          _ = assertEquals(result1.map(_.textContent), Some("Counter: 5"))
          _ = assertEquals(result2.map(_.textContent), Some("Counter: 8"))
        yield ()
      }

      // Verify resource was released
      released <- releasedRef.get
      _ = assert(released, "Resource should be released after use")
    yield ()
  }

  test("McpLifecycleTool.noArgs creates no-args tool with managed resource") {
    for
      counterRef <- Ref.of[IO, Int](42)
      acquiredRef <- Ref.of[IO, Boolean](false)

      managedResource = CatsResource.make(
        acquiredRef.set(true).as(counterRef)
      )(_ => IO.unit)

      toolResource = McpLifecycleTool.noArgs[IO, Ref[IO, Int]](
        "getValue",
        "Get current value",
        managedResource
      ) { counter =>
        counter.get.map(v => ToolResult.text(s"Value: $v"))
      }

      _ <- toolResource.use { tools =>
        for
          acquired <- acquiredRef.get
          _ = assert(acquired)
          result <- tools.call("getValue", Json.obj()).value
          _ = assertEquals(result.map(_.textContent), Some("Value: 42"))
        yield ()
      }
    yield ()
  }

  test("McpLifecycleTool.combine combines multiple lifecycle tools") {
    case class IncrementArgs(value: Int) derives ToolInput
    case class MultiplyArgs(value: Int) derives ToolInput

    for
      counterRef <- Ref.of[IO, Int](0)
      multiplierRef <- Ref.of[IO, Int](2)

      counterResource = CatsResource.pure[IO, Ref[IO, Int]](counterRef)
      multiplierResource = CatsResource.pure[IO, Ref[IO, Int]](multiplierRef)

      tool1 = McpLifecycleTool[IO, IncrementArgs, Ref[IO, Int]](
        "increment", "Increment",
        counterResource
      ) { (args, counter) =>
        counter.update(_ + args.value) *>
          counter.get.map(v => ToolResult.text(s"$v"))
      }

      tool2 = McpLifecycleTool[IO, MultiplyArgs, Ref[IO, Int]](
        "multiply", "Multiply",
        multiplierResource
      ) { (args, mult) =>
        mult.get.map(m => ToolResult.text(s"${args.value * m}"))
      }

      combined = McpLifecycleTool.combine[IO](tool1, tool2)

      _ <- combined.use { tools =>
        for
          toolList <- tools.list
          _ = assertEquals(toolList.map(_.name).toSet, Set("increment", "multiply"))
          r1 <- tools.call("increment", Json.obj("value" -> 5.asJson)).value
          r2 <- tools.call("multiply", Json.obj("value" -> 7.asJson)).value
          _ = assertEquals(r1.map(_.textContent), Some("5"))
          _ = assertEquals(r2.map(_.textContent), Some("14"))
        yield ()
      }
    yield ()
  }

  test("McpLifecycleTool.combineWith combines lifecycle and static tools") {
    case class IncrementArgs(value: Int) derives ToolInput

    for
      counterRef <- Ref.of[IO, Int](0)
      counterResource = CatsResource.pure[IO, Ref[IO, Int]](counterRef)

      lifecycleTool = McpLifecycleTool[IO, IncrementArgs, Ref[IO, Int]](
        "increment", "Increment counter",
        counterResource
      ) { (args, counter) =>
        counter.update(_ + args.value) *>
          counter.get.map(v => ToolResult.text(s"$v"))
      }

      staticTool = McpTool.twoNumbersPure[IO]("add", "Add numbers") { (a, b) =>
        s"${a + b}"
      }

      combined = McpLifecycleTool.combineWith[IO](lifecycleTool, staticTool)

      _ <- combined.use { tools =>
        for
          toolList <- tools.list
          _ = assertEquals(toolList.map(_.name).toSet, Set("increment", "add"))
          r1 <- tools.call("increment", Json.obj("value" -> 5.asJson)).value
          r2 <- tools.call("add", Json.obj("a" -> 3.asJson, "b" -> 2.asJson)).value
          _ = assertEquals(r1.map(_.textContent), Some("5"))
          _ = assertEquals(r2.map(_.textContent), Some("5.0"))
        yield ()
      }
    yield ()
  }

  // === McpServerLifecycle Tests ===

  test("McpServerLifecycle.withLifecycleToolsOnly creates server with managed tools") {
    for
      acquiredRef <- Ref.of[IO, Boolean](false)
      releasedRef <- Ref.of[IO, Boolean](false)

      toolResource = CatsResource.make(
        acquiredRef.set(true).as(
          McpTool.pureTextNoArgs[IO]("test", "Test tool")("result")
        )
      )(_ => releasedRef.set(true))

      serverResource = McpServerLifecycle.withLifecycleToolsOnly[IO](
        ServerInfo("test", "1.0.0"),
        toolResource
      )

      _ <- serverResource.use { server =>
        for
          acquired <- acquiredRef.get
          _ = assert(acquired, "Tool resource should be acquired")
          tools <- server.listTools
          _ = assertEquals(tools.map(_.name), List("test"))
          result <- server.callTool("test", Json.obj())
          _ = assertEquals(result.textContent, "result")
        yield ()
      }

      released <- releasedRef.get
      _ = assert(released, "Tool resource should be released")
    yield ()
  }

  // === McpLifecycle Trait Tests ===

  test("McpLifecycle.empty creates no-op lifecycle") {
    val lifecycle = McpLifecycle.empty[IO]

    lifecycle.initialize.use(_ => IO.unit) *>
      lifecycle.shutdown
  }

  test("McpLifecycle.fromResource wraps a resource") {
    for
      executedRef <- Ref.of[IO, Boolean](false)
      releasedRef <- Ref.of[IO, Boolean](false)

      resource = CatsResource.make(
        executedRef.set(true)
      )(_ => releasedRef.set(true))

      lifecycle = McpLifecycle.fromResource(resource)

      _ <- lifecycle.initialize.use { _ =>
        for
          executed <- executedRef.get
          _ = assert(executed, "Initialize should have run")
        yield ()
      }

      released <- releasedRef.get
      _ = assert(released, "Resource should be released")
    yield ()
  }

  // === McpServer Extension Tests ===

  test("McpServer can be wrapped in pure resource") {
    val server = McpServer.fromTools[IO](
      ServerInfo("test", "1.0.0"),
      McpTool.pureTextNoArgs[IO]("test", "Test")("result")
    )

    server.asResource.use { s =>
      for
        tools <- s.listTools
        _ = assertEquals(tools.map(_.name), List("test"))
      yield ()
    }
  }
