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

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import munit.CatsEffectSuite

/** Session-scoped protocol behaviour of the dispatcher: version negotiation, logging level,
  * subscriptions, completions, and task-augmented tool calls.
  */
class DispatcherFeaturesSpec extends CatsEffectSuite:

  given Tracer[IO] = Tracer.noop[IO]

  import mcp4s.server.dsl.*

  enum Color derives Schema:
    case Red, Green, Blue

  case class PaintArgs(
      @description("Colour to use") color: Color,
      shade: Option[Color] = None,
      label: String = "x"
  ) derives Schema

  case class SleepArgs(millis: Long) derives Schema

  /** `started` completes when the slow tool begins, so tests can cancel it mid-flight. */
  def server(started: Deferred[IO, Unit]): Server[IO] =
    val tools =
      Tool("quick").withDescription("sync only").handle[IO](_ => IO.pure(ok("quick"))) |+|
        Tool("slow")
          .withDescription("task optional")
          .input[SleepArgs]
          .withTaskSupport(TaskSupport.Optional)
          .handle[IO](a => started.complete(()) *> IO.sleep(a.millis.millis).as(ok("slept"))) |+|
        Tool("must_task")
          .withDescription("task required")
          .withTaskSupport(TaskSupport.Required)
          .handle[IO](_ => IO.pure(ok("tasked"))) |+|
        Tool("failing")
          .withDescription("reports an error result")
          .withTaskSupport(TaskSupport.Optional)
          .handle[IO](_ => IO.pure(error("boom")))
    val prompts =
      Prompt("paint").withDescription("Paint").input[PaintArgs].handle[IO] { a =>
        IO.pure(messages(user(s"paint ${a.color}")))
      }
    McpServer[IO](ServerInfo("features", "1.0.0"))
      .withTools(tools)
      .withPrompts(prompts)
      .withResources(Resource.text[IO]("file:///a", "A")("a"))
      .withInstructions("Be nice")
      .toServer

  def initParams(version: String = McpVersion.Current): Json = Json.obj(
    "protocolVersion" -> Json.fromString(version),
    "capabilities"    -> Json.obj(),
    "clientInfo"      -> Json.obj("name" -> "t".asJson, "version" -> "1".asJson)
  )

  def request(d: Dispatcher[IO], method: String, params: Json = Json.obj()): IO[JsonRpcMessage] =
    d.dispatch(JsonRpcRequest(RequestId.NumberId(1), method, Some(params))).map(_.get)

  def result(d: Dispatcher[IO], method: String, params: Json = Json.obj()): IO[Json] =
    request(d, method, params).flatMap:
      case JsonRpcResponse(_, json)   => IO.pure(json)
      case JsonRpcErrorResponse(_, e) => IO.raiseError(McpError.fromJsonRpcError(e))
      case other                      => IO.raiseError(new Exception(s"unexpected $other"))

  def errorCode(d: Dispatcher[IO], method: String, params: Json = Json.obj()): IO[Int] =
    request(d, method, params).flatMap:
      case JsonRpcErrorResponse(_, e) => IO.pure(e.code)
      case other                      => IO.raiseError(new Exception(s"expected error, got $other"))

  def initialized(version: String = McpVersion.Current): IO[(Dispatcher[IO], Deferred[IO, Unit])] =
    for
      started <- Deferred[IO, Unit]
      d       <- Dispatcher[IO](server(started))
      _       <- result(d, McpMethod.Initialize, initParams(version))
    yield (d, started)

  // === Version negotiation ===

  test("initialize echoes a supported requested version") {
    initialized("2025-06-18").flatMap { (d, _) =>
      d.protocolVersion.map(v => assertEquals(v, Some("2025-06-18")))
    }
  }

  test("initialize answers an unknown version with the latest supported one") {
    for
      started <- Deferred[IO, Unit]
      d       <- Dispatcher[IO](server(started))
      json    <- result(d, McpMethod.Initialize, initParams("1999-01-01"))
    yield
      assertEquals(json.hcursor.get[String]("protocolVersion"), Right(McpVersion.Current))
      assertEquals(json.hcursor.get[String]("instructions"), Right("Be nice"))
  }

  test("a stateless dispatcher accepts initialize more than once") {
    for
      started <- Deferred[IO, Unit]
      d       <- Dispatcher[IO](server(started), Dispatcher.Config(allowReinitialize = true))
      _       <- result(d, McpMethod.Initialize, initParams())
      again   <- result(d, McpMethod.Initialize, initParams("2025-06-18"))
      v       <- d.protocolVersion
    yield
      assertEquals(again.hcursor.get[String]("protocolVersion"), Right("2025-06-18"))
      assertEquals(v, Some("2025-06-18"))
  }

  // === Logging level and subscriptions ===

  test("logging/setLevel is remembered for the session") {
    initialized().flatMap { (d, _) =>
      for
        before <- d.logLevel
        _      <- result(d, McpMethod.LoggingSetLevel, Json.obj("level" -> "warning".asJson))
        after  <- d.logLevel
      yield
        assertEquals(before, None)
        assertEquals(after, Some(LogLevel.Warning))
    }
  }

  test("resources/subscribe and unsubscribe are tracked") {
    initialized().flatMap { (d, _) =>
      for
        _    <- result(d, McpMethod.ResourcesSubscribe, Json.obj("uri" -> "file:///a".asJson))
        one  <- d.subscriptions
        _    <- result(d, McpMethod.ResourcesUnsubscribe, Json.obj("uri" -> "file:///a".asJson))
        none <- d.subscriptions
      yield
        assertEquals(one, Set("file:///a"))
        assertEquals(none, Set.empty[String])
    }
  }

  // === Completions ===

  def completeParams(argument: String, value: String): Json =
    CompleteParams(CompletionRef.PromptRef("paint"), CompletionArgument(argument, value)).asJson

  test("completion/complete suggests enum values for a typed prompt argument") {
    initialized().flatMap { (d, _) =>
      for
        green <- result(d, McpMethod.CompletionComplete, completeParams("color", "g"))
        all   <- result(d, McpMethod.CompletionComplete, completeParams("shade", ""))
        free  <- result(d, McpMethod.CompletionComplete, completeParams("label", "x"))
      yield
        assertEquals(green.as[CompleteResult].map(_.completion.values), Right(List("Green")))
        assertEquals(
          all.as[CompleteResult].map(_.completion.values),
          Right(List("Red", "Green", "Blue"))
        )
        assertEquals(free.as[CompleteResult].map(_.completion.values), Right(Nil))
    }
  }

  test("completion/complete for an unknown prompt or a resource template is empty") {
    initialized().flatMap { (d, _) =>
      val resourceRef = CompleteParams(
        CompletionRef.ResourceRef("file:///{x}"),
        CompletionArgument("x", "")
      ).asJson
      val unknown = CompleteParams(
        CompletionRef.PromptRef("nope"),
        CompletionArgument("color", "")
      ).asJson
      for
        r1 <- result(d, McpMethod.CompletionComplete, resourceRef)
        r2 <- result(d, McpMethod.CompletionComplete, unknown)
      yield
        assertEquals(r1.as[CompleteResult].map(_.completion.values), Right(Nil))
        assertEquals(r2.as[CompleteResult].map(_.completion.values), Right(Nil))
    }
  }

  // === Tasks ===

  test("a server with task-capable tools advertises the tasks capability") {
    Deferred[IO, Unit].map { started =>
      assert(server(started).capabilities.tasks.exists(_.supportsToolCalls))
    }
  }

  def callAsTask(d: Dispatcher[IO], name: String, args: Json = Json.obj()): IO[Task] =
    result(
      d,
      McpMethod.ToolsCall,
      Json.obj("name" -> name.asJson, "arguments" -> args, "task" -> Json.obj())
    ).flatMap(_.as[CreateTaskResult].map(_.task).fold(IO.raiseError, IO.pure))

  def taskParams(id: String): Json = Json.obj("taskId" -> id.asJson)

  test("task-augmented tools/call returns a working task whose result is retrievable") {
    initialized().flatMap { (d, _) =>
      for
        task   <- callAsTask(d, "slow", Json.obj("millis" -> 10.asJson))
        res    <- result(d, McpMethod.TasksResult, taskParams(task.taskId))
        after  <- result(d, McpMethod.TasksGet, taskParams(task.taskId))
        listed <- result(d, McpMethod.TasksList)
      yield
        assertEquals(task.status, TaskStatus.Working)
        assert(task.ttl.isDefined)
        assert(task.pollInterval.isDefined)
        assertEquals(res.as[ToolResult].map(_.textContent), Right("slept"))
        assertEquals(after.as[Task].map(_.status), Right(TaskStatus.Completed))
        assertEquals(
          listed.as[ListTasksResult].map(_.tasks.map(_.taskId)),
          Right(List(task.taskId))
        )
    }
  }

  test("a tool error result marks the task failed and tasks/result still returns it") {
    initialized().flatMap { (d, _) =>
      for
        task  <- callAsTask(d, "failing")
        res   <- result(d, McpMethod.TasksResult, taskParams(task.taskId))
        state <- result(d, McpMethod.TasksGet, taskParams(task.taskId))
      yield
        assertEquals(res.as[ToolResult].map(_.isError), Right(Some(true)))
        assertEquals(state.as[Task].map(_.status), Right(TaskStatus.Failed))
    }
  }

  test("tasks/cancel stops a running task and later tasks/result fails") {
    initialized().flatMap { (d, started) =>
      for
        task      <- callAsTask(d, "slow", Json.obj("millis" -> 60000.asJson))
        _         <- started.get
        cancelled <- result(d, McpMethod.TasksCancel, taskParams(task.taskId))
        resCode   <- errorCode(d, McpMethod.TasksResult, taskParams(task.taskId))
        again     <- errorCode(d, McpMethod.TasksCancel, taskParams(task.taskId))
      yield
        assertEquals(cancelled.as[Task].map(_.status), Right(TaskStatus.Cancelled))
        assertEquals(resCode, JsonRpcErrorCode.RequestCancelled)
        assertEquals(again, JsonRpcErrorCode.InvalidParams)
    }
  }

  test("task augmentation is refused for forbidden tools and required for required ones") {
    initialized().flatMap { (d, _) =>
      for
        forbidden <- errorCode(
          d,
          McpMethod.ToolsCall,
          Json.obj("name" -> "quick".asJson, "task" -> Json.obj())
        )
        required <- errorCode(d, McpMethod.ToolsCall, Json.obj("name" -> "must_task".asJson))
        asTask   <- callAsTask(d, "must_task")
        unknown  <- errorCode(d, McpMethod.TasksGet, taskParams("nope"))
      yield
        assertEquals(forbidden, JsonRpcErrorCode.MethodNotFound)
        assertEquals(required, JsonRpcErrorCode.MethodNotFound)
        assertEquals(asTask.status, TaskStatus.Working)
        assertEquals(unknown, JsonRpcErrorCode.InvalidParams)
    }
  }

  test("without the tasks capability, task metadata is ignored and tasks/get is unsupported") {
    val plain = McpServer[IO](ServerInfo("plain", "1"))
      .withTools(Tool("quick").handle[IO](_ => IO.pure(ok("quick"))))
      .toServer
    for
      d   <- Dispatcher[IO](plain)
      _   <- result(d, McpMethod.Initialize, initParams())
      res <- result(
        d,
        McpMethod.ToolsCall,
        Json.obj("name" -> "quick".asJson, "task" -> Json.obj())
      )
      code <- errorCode(d, McpMethod.TasksGet, taskParams("x"))
    yield
      assertEquals(res.as[ToolResult].map(_.textContent), Right("quick"))
      assertEquals(code, JsonRpcErrorCode.MethodNotFound)
  }

  test("close cancels running tasks") {
    initialized().flatMap { (d, started) =>
      for
        task <- callAsTask(d, "slow", Json.obj("millis" -> 60000.asJson))
        _    <- started.get
        _    <- d.close
        code <- errorCode(d, McpMethod.TasksGet, taskParams(task.taskId))
      yield assertEquals(code, JsonRpcErrorCode.InvalidParams)
    }
  }

  test("Iso8601 formats epoch millis as UTC timestamps") {
    assertEquals(Iso8601.format(0L), "1970-01-01T00:00:00.000Z")
    assertEquals(Iso8601.format(1700000000123L), "2023-11-14T22:13:20.123Z")
    assertEquals(Iso8601.format(951782400000L), "2000-02-29T00:00:00.000Z")
  }
