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

package mcp4s.client.transport

import cats.effect.{Async, Resource as CatsResource}
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Stream
import fs2.io.process.{ProcessBuilder, Processes}
import io.circe.parser.*
import io.circe.syntax.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.{ConnectionRunner, McpClient, McpConnection}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.transport.{McpChannel, Timeouts}

/** Configuration for stdio-based MCP server connections. */
final case class StdioTransportConfig(
    command: String,
    args: List[String] = List.empty,
    workingDirectory: Option[String] = None,
    env: Map[String, String] = Map.empty,
    /** Bound on the outgoing message queue, for backpressure. */
    maxQueueSize: Int = 1024,
    timeouts: Timeouts = Timeouts.default
)

/** Transport for communicating with MCP servers via standard input/output.
  *
  * Spawns the configured command and speaks newline-delimited JSON-RPC over its stdin/stdout;
  * stderr lines are forwarded to this process's stderr.
  */
object StdioClientTransport:

  /** Build a [[ClientTransport]] for the given process configuration. */
  def apply[F[_]: Async: Processes](config: StdioTransportConfig): ClientTransport[F] =
    new ClientTransport[F]:
      def open: CatsResource[F, McpChannel[F]] =
        for
          outQueue <- CatsResource.eval(Queue.bounded[F, String](config.maxQueueSize))

          process <- ProcessBuilder(config.command, config.args)
            .withWorkingDirectory(
              config.workingDirectory
                .map(fs2.io.file.Path(_))
                .getOrElse(fs2.io.file.Path("./"))
            )
            .withExtraEnv(config.env)
            .spawn[F]

          // Forward stderr lines for diagnostics.
          _ <- process.stderr
            .through(fs2.text.utf8.decode)
            .through(fs2.text.lines)
            .filter(_.trim.nonEmpty)
            .evalMap(line => Async[F].delay(System.err.println(s"[MCP Server stderr]: $line")))
            .compile
            .drain
            .background

          // Pump outgoing lines into the process's stdin.
          _ <- Stream
            .fromQueueUnterminated(outQueue)
            .map(_ + "\n")
            .through(fs2.text.utf8.encode)
            .through(process.stdin)
            .compile
            .drain
            .background
        yield new McpChannel[F]:
          def send(message: JsonRpcMessage): F[Unit] =
            outQueue.offer(message.asJson.noSpaces)

          def incoming: Stream[F, JsonRpcMessage] =
            process.stdout
              .through(fs2.text.utf8.decode)
              .through(fs2.text.lines)
              .filter(_.trim.nonEmpty)
              .evalMapFilter { line =>
                parse(line).flatMap(_.as[JsonRpcMessage]) match
                  case Right(message) => Async[F].pure(Some(message))
                  case Left(err) =>
                    Async[F]
                      .delay(System.err.println(s"[MCP Client] Failed to parse message: $err"))
                      .as(None)
              }

  /** Connect to an MCP server by spawning a process. */
  def connect[F[_]: Async: Processes](
      client: McpClient[F],
      config: StdioTransportConfig
  )(using Tracer[F]): CatsResource[F, McpConnection[F]] =
    ConnectionRunner.run(client, apply(config), config.timeouts, summon[Tracer[F]])
