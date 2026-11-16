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

import cats.effect.{Async, LiftIO}
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.{Pipe, Stream}
import fs2.io.{stdin, stdout}
import fs2.text
import io.circe.parser.*
import io.circe.syntax.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*

import scala.concurrent.duration.*

/** Stdio transport configuration.
  *
  * @param requestTimeout
  *   how long a server-initiated request (sampling, elicitation) waits for the client's answer
  * @param maxConcurrentRequests
  *   requests dispatched concurrently; dispatch must not be serial, or a `notifications/cancelled`
  *   line could never interrupt a running tool call
  */
final case class StdioConfig(
    requestTimeout: FiniteDuration = 5.minutes,
    maxConcurrentRequests: Int = 256,
    maxQueueSize: Int = 1024
)

object StdioConfig:
  val default: StdioConfig = StdioConfig()

/** Stdio transport for MCP servers.
  *
  * Reads newline-delimited JSON-RPC messages from stdin and writes messages to stdout. Suitable for
  * local tool servers launched by clients such as Claude Desktop. The transport is fully
  * bidirectional: tools get a [[ToolContext]] whose progress, logging, sampling and elicitation
  * travel over the same pipe, and the client's answers are correlated back to the waiting tool.
  */
object StdioTransport:

  /** Default buffer size for reading from stdin */
  private val DefaultBufferSize: Int = 4096

  /** Run the MCP server using stdio transport until stdin closes.
    *
    * @param server
    *   The MCP server to run
    * @param tracer
    *   Optional OpenTelemetry tracer for distributed tracing (defaults to noop)
    */
  def run[F[_]: Async: LiftIO](server: Server[F], config: StdioConfig = StdioConfig.default)(using
      Tracer[F]
  ): F[Unit] =
    // fs2's stdin/stdout require LiftIO on Scala Native; on the JVM and JS the
    // instance is otherwise unused, so reference it to satisfy -Wunused:all.
    val _ = LiftIO[F]
    runWith[F](server, stdin[F](DefaultBufferSize), stdout[F], config)

  /** Run the server over arbitrary byte streams: `input` carries newline-delimited JSON-RPC from
    * the client and `output` receives the server's lines. [[run]] is this over stdin/stdout; tests
    * and custom pipes (sockets, in-memory channels) can supply their own.
    */
  def runWith[F[_]: Async](
      server: Server[F],
      input: Stream[F, Byte],
      output: Pipe[F, Byte, Unit],
      config: StdioConfig = StdioConfig.default
  )(using tracer: Tracer[F]): F[Unit] =
    Queue.bounded[F, Option[String]](config.maxQueueSize).flatMap { outQueue =>
      val send = (message: JsonRpcMessage) => outQueue.offer(Some(message.asJson.noSpaces))

      val writer: Stream[F, Unit] =
        Stream
          .fromQueueNoneTerminated(outQueue)
          .map(_ + "\n")
          .through(text.utf8.encode)
          .through(output)

      ServerSession
        .create[F](server, Outbound.uniform(send), config.requestTimeout, tracer)
        .bracket { session =>
          // Responses are emitted in completion order (JSON-RPC correlates by id, not order);
          // the single writer keeps the output serial.
          val reader: Stream[F, Unit] =
            input
              .through(text.utf8.decode)
              .through(text.lines)
              .filter(_.nonEmpty)
              .parEvalMapUnordered(config.maxConcurrentRequests)(handleLine(session, _))
              .unNone
              .evalMap(send)

          writer.compile.drain.background.use { writerDone =>
            reader.compile.drain *> outQueue.offer(None) *> writerDone.void
          }
        }(_.close)
    }

  private def handleLine[F[_]: Async](
      session: ServerSession[F],
      line: String
  ): F[Option[JsonRpcMessage]] =
    decode[JsonRpcMessage](line) match
      case Right(message) => session.handle(message)
      case Left(err)      =>
        Async[F]
          .pure(JsonRpcErrorResponse(RequestId.NullId, JsonRpcError.parseError(err.getMessage)))
          .map(Some(_))
