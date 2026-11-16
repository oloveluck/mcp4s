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

package mcp4s.server.transport

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.{Pipe, Stream}
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*
import munit.CatsEffectSuite
import org.typelevel.otel4s.trace.Tracer

/** Drives the stdio server over in-memory pipes: the client side writes lines into `stdin` and
  * reads whatever the server prints to `stdout`.
  */
class StdioTransportSpec extends CatsEffectSuite:

  given Tracer[IO] = Tracer.noop[IO]

  import mcp4s.server.dsl.*

  val server: Server[IO] =
    McpServer[IO](ServerInfo("stdio", "1.0.0"))
      .withTools(
        Tool("ask").withDescription("Samples the client").handleWith[IO] { (_, ctx) =>
          ctx.log(LogLevel.Info, "asking") *>
            ctx.sampling
              .createMessage(
                CreateMessageParams(
                  List(SamplingMessage(Role.User, SamplingTextContent("2+2?"))),
                  maxTokens = 10
                )
              )
              .map(r =>
                r.content match
                  case SamplingTextContent(t) => ok(t)
                  case _                      => error("unexpected")
              )
        }
      )
      .toServer

  /** A running server plus the client's ends of its pipes. */
  final case class Pipes(stdin: Queue[IO, Option[String]], stdout: Queue[IO, JsonRpcMessage]):
    def write(message: JsonRpcMessage): IO[Unit] = stdin.offer(Some(message.asJson.noSpaces))
    def read: IO[JsonRpcMessage]                 = stdout.take.timeout(10.seconds)
    def close: IO[Unit]                          = stdin.offer(None)

  def withServer[A](f: Pipes => IO[A]): IO[A] =
    for
      stdin  <- Queue.unbounded[IO, Option[String]]
      stdout <- Queue.unbounded[IO, JsonRpcMessage]
      output: Pipe[IO, Byte, Unit] = _.through(fs2.text.utf8.decode)
        .through(fs2.text.lines)
        .filter(_.nonEmpty)
        .evalMap(line => decode[JsonRpcMessage](line).liftTo[IO].flatMap(stdout.offer))
      input = Stream.fromQueueNoneTerminated(stdin).map(_ + "\n").through(fs2.text.utf8.encode)
      pipes = Pipes(stdin, stdout)
      result <- StdioTransport.runWith[IO](server, input, output).background.use { done =>
        f(pipes).guarantee(pipes.close) <* done.void.timeout(10.seconds)
      }
    yield result

  val initialize: JsonRpcRequest = JsonRpcRequest(
    RequestId.NumberId(1),
    McpMethod.Initialize,
    Some(
      InitializeParams(
        McpVersion.Current,
        ClientCapabilities(sampling = Some(SamplingCapability())),
        ClientInfo("test", "1")
      ).asJson
    )
  )

  test("stdio answers requests and ends when stdin closes") {
    withServer { pipes =>
      for
        _    <- pipes.write(initialize)
        init <- pipes.read
        _    <- pipes.write(JsonRpcRequest(RequestId.NumberId(2), McpMethod.Ping, None))
        pong <- pipes.read
      yield
        assert(init.isInstanceOf[JsonRpcResponse])
        assertEquals(pong, JsonRpcResponse(RequestId.NumberId(2), Json.obj()))
    }
  }

  test("stdio is bidirectional: a tool samples the client and gets its answer back") {
    withServer { pipes =>
      for
        _ <- pipes.write(initialize)
        _ <- pipes.read
        _ <- pipes.write(
          JsonRpcRequest(
            RequestId.NumberId(2),
            McpMethod.ToolsCall,
            Some(Json.obj("name" -> "ask".asJson, "arguments" -> Json.obj()))
          )
        )
        log      <- pipes.read
        sampling <- pipes.read
        reqId    <- sampling match
          case JsonRpcRequest(id, McpMethod.SamplingCreateMessage, _) => IO.pure(id)
          case other => IO.raiseError(new Exception(s"expected sampling request, got $other"))
        _ <- pipes.write(
          JsonRpcResponse(
            reqId,
            CreateMessageResult(Role.Assistant, SamplingTextContent("four"), "model").asJson
          )
        )
        result <- pipes.read
      yield
        log match
          case JsonRpcNotification(method, _) => assertEquals(method, McpMethod.LoggingMessage)
          case other                          => fail(s"expected a log notification, got $other")
        result match
          case JsonRpcResponse(RequestId.NumberId(2), json) =>
            assertEquals(json.as[ToolResult].map(_.textContent), Right("four"))
          case other => fail(s"unexpected $other")
    }
  }

  test("stdio reports parse errors without stopping") {
    withServer { pipes =>
      for
        _   <- pipes.stdin.offer(Some("not json"))
        err <- pipes.read
        _   <- pipes.write(initialize)
        ok  <- pipes.read
      yield
        err match
          case JsonRpcErrorResponse(RequestId.NullId, e) =>
            assertEquals(e.code, JsonRpcErrorCode.ParseError)
          case other => fail(s"unexpected $other")
        assert(ok.isInstanceOf[JsonRpcResponse])
    }
  }
