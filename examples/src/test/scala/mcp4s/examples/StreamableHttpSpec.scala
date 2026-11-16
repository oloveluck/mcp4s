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

package mcp4s.examples

import scala.concurrent.duration.*

import cats.effect.{IO, Ref, Resource as CatsResource}
import cats.syntax.all.*
import com.comcast.ip4s.port
import fs2.concurrent.Topic
import io.circe.Json
import io.circe.syntax.*
import mcp4s.client.*
import mcp4s.client.transport.*
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*
import mcp4s.server.transport.*
import mcp4s.testkit.*
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.ci.CIString
import org.typelevel.otel4s.trace.Tracer

/** Wire-level behaviour of the Streamable HTTP transport (spec 2025-11-25) plus end-to-end checks
  * of the features that ride on it: standalone GET streams, resource subscriptions, logging levels,
  * tasks, and progress under concurrency.
  */
class StreamableHttpSpec extends CatsEffectSuite:

  given Tracer[IO] = Tracer.noop[IO]

  import mcp4s.server.dsl.*

  override val munitIOTimeout: FiniteDuration = 60.seconds

  // === Fixtures ===

  def serve(server: Server[IO], config: HttpConfig[IO] = HttpConfig(port = port"0")) =
    HttpTransport.serve[IO](server, config.copy(port = port"0"))

  val httpClient: CatsResource[IO, Client[IO]] = EmberClientBuilder.default[IO].build

  def mcpUri(server: org.http4s.server.Server): Uri =
    Uri.unsafeFromString(s"http://localhost:${server.address.getPort}/mcp")

  def jsonRpc(method: String, id: Int = 1, params: Json = Json.obj()): Json =
    JsonRpcRequest(RequestId.NumberId(id.toLong), method, Some(params)).asJson

  val initBody: Json = jsonRpc(
    McpMethod.Initialize,
    params = InitializeParams(McpVersion.Current, ClientCapabilities(), ClientInfo("t", "1")).asJson
  )

  def post(
      client: Client[IO],
      uri: Uri,
      body: Json,
      headers: Header.ToRaw*
  ): CatsResource[IO, Response[IO]] =
    client.run(
      Request[IO](Method.POST, uri)
        .withEntity(body)
        .putHeaders(Header.Raw(CIString("Accept"), "application/json, text/event-stream"))
        .putHeaders(headers*)
    )

  def sessionOf(resp: Response[IO]): Option[String] =
    resp.headers.get(CIString("Mcp-Session-Id")).map(_.head.value)

  def session(value: String): Header.ToRaw = Header.Raw(CIString("Mcp-Session-Id"), value)

  /** Initialize over raw HTTP and return the session id. */
  def initialize(client: Client[IO], uri: Uri): IO[String] =
    post(client, uri, initBody).use { resp =>
      IO(assertEquals(resp.status, Status.Ok)) *>
        resp.bodyText.compile.drain.as(sessionOf(resp).getOrElse(fail("no session id")))
    }

  def withServer[A](server: Server[IO], config: HttpConfig[IO] = HttpConfig(port = port"0"))(
      f: (Client[IO], Uri) => IO[A]
  ): IO[A] =
    (serve(server, config), httpClient).tupled.use((s, c) => f(c, mcpUri(s)))

  def connect(
      server: org.http4s.server.Server,
      client: McpClient[IO] = DeterministicClients.simple[IO]
  ) =
    httpClient.flatMap(c =>
      HttpClientTransport.connect[IO](client, HttpTransportConfig(mcpUri(server).renderString), c)
    )

  // === Session handling ===

  test("notifications and client responses are acknowledged with 202 and no body") {
    withServer(TestServers.simple[IO]) { (client, uri) =>
      initialize(client, uri).flatMap { sid =>
        val notification = JsonRpcNotification(McpMethod.Initialized, None).asJson
        post(client, uri, notification, session(sid)).use { resp =>
          resp.bodyText.compile.string.map { body =>
            assertEquals(resp.status, Status.Accepted)
            assertEquals(body, "")
          }
        }
      }
    }
  }

  test("a request without a session id is refused with 400, an unknown session with 404") {
    withServer(TestServers.simple[IO]) { (client, uri) =>
      for
        _       <- initialize(client, uri)
        missing <- post(client, uri, jsonRpc(McpMethod.Ping)).use(r => IO.pure(r.status))
        unknown <- post(client, uri, jsonRpc(McpMethod.Ping), session("nope")).use(r =>
          IO.pure(r.status)
        )
      yield
        assertEquals(missing, Status.BadRequest)
        assertEquals(unknown, Status.NotFound)
    }
  }

  test("DELETE terminates the session; the id is then unknown") {
    withServer(TestServers.simple[IO]) { (client, uri) =>
      initialize(client, uri).flatMap { sid =>
        for
          deleted <- client
            .run(Request[IO](Method.DELETE, uri).putHeaders(session(sid)))
            .use(r => IO.pure(r.status))
          after <- post(client, uri, jsonRpc(McpMethod.Ping), session(sid)).use(r =>
            IO.pure(r.status)
          )
        yield
          assertEquals(deleted, Status.Ok)
          assertEquals(after, Status.NotFound)
      }
    }
  }

  test("an unsupported MCP-Protocol-Version header is refused with 400") {
    withServer(TestServers.simple[IO]) { (client, uri) =>
      initialize(client, uri).flatMap { sid =>
        val version = (v: String) => Header.Raw(CIString("MCP-Protocol-Version"), v)
        for
          bad <- post(client, uri, jsonRpc(McpMethod.Ping), session(sid), version("1999-01-01"))
            .use(r => IO.pure(r.status))
          good <- post(client, uri, jsonRpc(McpMethod.Ping), session(sid), version("2025-06-18"))
            .use(r => IO.pure(r.status))
        yield
          assertEquals(bad, Status.BadRequest)
          assertEquals(good, Status.Ok)
      }
    }
  }

  test("requests from a foreign Origin are refused with 403 on a localhost binding") {
    withServer(TestServers.simple[IO]) { (client, uri) =>
      val origin = (o: String) => Header.Raw(CIString("Origin"), o)
      for
        evil <- post(client, uri, initBody, origin("http://evil.example.com")).use(r =>
          IO.pure(r.status)
        )
        local <- post(client, uri, initBody, origin("http://localhost:1234")).use(r =>
          IO.pure(r.status)
        )
      yield
        assertEquals(evil, Status.Forbidden)
        assertEquals(local, Status.Ok)
    }
  }

  test("stateless mode issues no session id, re-initializes freely, and has no DELETE") {
    val stateless = HttpConfig[IO](port = port"0", enableSessions = false)
    withServer(TestServers.simple[IO], stateless) { (client, uri) =>
      for
        first  <- post(client, uri, initBody).use(r => IO.pure((r.status, sessionOf(r))))
        second <- post(client, uri, initBody).use(r => IO.pure(r.status))
        ping   <- post(client, uri, jsonRpc(McpMethod.Ping)).use(r => IO.pure(r.status))
        del    <- client.run(Request[IO](Method.DELETE, uri)).use(r => IO.pure(r.status))
      yield
        assertEquals(first, (Status.Ok, None))
        assertEquals(second, Status.Ok)
        assertEquals(ping, Status.Ok)
        assertEquals(del, Status.MethodNotAllowed)
    }
  }

  // === Standalone GET stream ===

  test("GET opens one SSE stream per session, primed with an event id") {
    withServer(TestServers.simple[IO]) { (client, uri) =>
      initialize(client, uri).flatMap { sid =>
        val get = Request[IO](Method.GET, uri)
          .putHeaders(session(sid), Header.Raw(CIString("Accept"), "text/event-stream"))
        val noAccept = Request[IO](Method.GET, uri).putHeaders(session(sid))
        client.run(get).use { resp =>
          for
            priming       <- resp.body.through(ServerSentEvent.decoder[IO]).head.compile.lastOrError
            second        <- client.run(get).use(r => IO.pure(r.status))
            notAcceptable <- client.run(noAccept).use(r => IO.pure(r.status))
          yield
            assertEquals(resp.status, Status.Ok)
            assert(
              resp.headers
                .get(CIString("Content-Type"))
                .exists(_.head.value.startsWith("text/event-stream"))
            )
            assert(priming.id.isDefined)
            assertEquals(priming.data, Some(""))
            assertEquals(second, Status.Conflict)
            assertEquals(notAcceptable, Status.NotAcceptable)
        }
      }
    }
  }

  // === Resource subscriptions ===

  def watchedServer(changes: Topic[IO, Unit]): Server[IO] =
    McpServer[IO](ServerInfo("watched", "1.0.0"))
      .withResources(
        Resource.subscribable[IO]("file:///watched", "Watched", changes.subscribe(16)) { uri =>
          IO.pure(text(uri, "content"))
        }
      )
      .toServer

  def notifying(seen: Ref[IO, List[ServerNotification]]): McpClient[IO] =
    McpClientBuilder[IO]("notifying", "1.0.0")
      .withNotifications(Notifications[IO] { case n => seen.update(_ :+ n) })
      .toClient

  /** Publish until the client has seen an update (the GET stream attaches asynchronously). */
  def awaitUpdate(changes: Topic[IO, Unit], seen: Ref[IO, List[ServerNotification]]): IO[Unit] =
    (changes.publish1(()) *> IO.sleep(100.millis) *> seen.get)
      .iterateUntil(_.contains(ServerNotification.ResourceUpdated("file:///watched")))
      .timeout(10.seconds)
      .void

  test("subscribed resource updates reach the client over the HTTP GET stream") {
    (Topic[IO, Unit], Ref.of[IO, List[ServerNotification]](Nil)).tupled.flatMap { (changes, seen) =>
      serve(watchedServer(changes)).flatMap(connect(_, notifying(seen))).use { conn =>
        for
          _ <- IO(assert(conn.supportsResourceSubscriptions))
          _ <- conn.subscribeResource("file:///watched")
          _ <- awaitUpdate(changes, seen)
          _ <- conn.unsubscribeResource("file:///watched")
        yield ()
      }
    }
  }

  test("subscribed resource updates reach the client over WebSocket") {
    (Topic[IO, Unit], Ref.of[IO, List[ServerNotification]](Nil)).tupled.flatMap { (changes, seen) =>
      WebSocketTransport
        .serve[IO](watchedServer(changes), WebSocketConfig(port = port"0"))
        .flatMap { ws =>
          WebSocketClientTransport.connect[IO](
            notifying(seen),
            WebSocketTransportConfig(s"ws://localhost:${ws.address.getPort}/ws")
          )
        }
        .use { conn =>
          conn.subscribeResource("file:///watched") *> awaitUpdate(changes, seen)
        }
    }
  }

  test("unsubscribed resources do not notify") {
    (Topic[IO, Unit], Ref.of[IO, List[ServerNotification]](Nil)).tupled.flatMap { (changes, seen) =>
      serve(watchedServer(changes)).flatMap(connect(_, notifying(seen))).use { conn =>
        for
          _    <- conn.readResource("file:///watched")
          _    <- changes.publish1(()) *> IO.sleep(300.millis)
          seen <- seen.get
        yield assertEquals(seen, Nil)
      }
    }
  }

  // === Logging level ===

  val loggingServer: Server[IO] =
    McpServer[IO](ServerInfo("logging", "1.0.0"))
      .withTools(
        Tool("chatty").handleWith[IO] { (_, ctx) =>
          ctx.log(LogLevel.Debug, "debug") *> ctx.log(LogLevel.Error, "error").as(ok("done"))
        }
      )
      .toServer

  test("log messages below the level set with logging/setLevel are not sent") {
    Ref.of[IO, List[ServerNotification]](Nil).flatMap { seen =>
      serve(loggingServer).flatMap(connect(_, notifying(seen))).use { conn =>
        for
          _        <- conn.callTool("chatty", Json.obj())
          _        <- IO.sleep(100.millis)
          all      <- seen.get
          _        <- seen.set(Nil)
          _        <- conn.setLogLevel(LogLevel.Warning)
          _        <- conn.callTool("chatty", Json.obj())
          _        <- IO.sleep(100.millis)
          filtered <- seen.get
        yield
          val levels = (ns: List[ServerNotification]) =>
            ns.collect { case ServerNotification.Log(m) => m.level }
          assertEquals(levels(all), List(LogLevel.Debug, LogLevel.Error))
          assertEquals(levels(filtered), List(LogLevel.Error))
      }
    }
  }

  // === Tasks ===

  case class SleepArgs(millis: Long) derives Schema

  val taskServer: Server[IO] =
    McpServer[IO](ServerInfo("tasks", "1.0.0"))
      .withTools(
        Tool("slow")
          .input[SleepArgs]
          .withTaskSupport(TaskSupport.Optional)
          .handleWith[IO] { (a, ctx) =>
            ctx.progress(1, Some(2)) *> IO.sleep(a.millis.millis).as(ok("slept"))
          }
      )
      .toServer

  test("tasks: create, poll, fetch result, list and cancel over HTTP") {
    serve(taskServer).flatMap(connect(_)).use { conn =>
      for
        _         <- IO(assert(conn.supportsToolTasks))
        task      <- conn.callToolTask("slow", Json.obj("millis" -> 50.asJson))
        state     <- conn.getTask(task.taskId)
        result    <- conn.taskResult(task.taskId)
        done      <- conn.getTask(task.taskId)
        listed    <- conn.listTasks()
        long      <- conn.callToolTask("slow", Json.obj("millis" -> 60000.asJson))
        cancelled <- conn.cancelTask(long.taskId)
      yield
        assertEquals(task.status, TaskStatus.Working)
        assert(Set(TaskStatus.Working, TaskStatus.Completed).contains(state.status))
        assertEquals(result.textContent, "slept")
        assertEquals(done.status, TaskStatus.Completed)
        assertEquals(listed._1.map(_.taskId).toSet, Set(task.taskId))
        assertEquals(cancelled.status, TaskStatus.Cancelled)
    }
  }

  // === Progress under concurrency (regression for the shared-queue race) ===

  test("every concurrent call receives exactly its own progress notifications") {
    serve(TestServers.withProgress[IO]).flatMap(connect(_)).use { conn =>
      (1 to 20).toList
        .parTraverse { _ =>
          for
            seen <- Ref.of[IO, Int](0)
            _    <- conn.callTool("count", Json.obj(), _ => seen.update(_ + 1))
            n    <- seen.get
          yield n
        }
        .map(counts => assertEquals(counts, List.fill(20)(3)))
    }
  }
