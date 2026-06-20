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

import cats.effect.{Async, Concurrent, LiftIO}
import cats.syntax.all.*
import fs2.{Pipe, Stream}
import fs2.io.stdin
import fs2.io.stdout
import fs2.text
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*

/** Stdio transport for MCP servers.
  *
  * Reads JSON-RPC messages from stdin (one per line) and writes responses to stdout. This transport
  * is suitable for local tool servers communicating with clients like Claude Desktop.
  */
object StdioTransport:

  /** Default buffer size for reading from stdin */
  private val DefaultBufferSize: Int = 4096

  /** Run the MCP server using stdio transport.
    *
    * Reads newline-delimited JSON-RPC messages from stdin and writes responses to stdout. Runs
    * until stdin is closed or the server receives a shutdown request.
    *
    * @param server
    *   The MCP server to run
    * @param tracer
    *   Optional OpenTelemetry tracer for distributed tracing (defaults to noop)
    */
  def run[F[_]: Async: LiftIO](server: Server[F])(using Tracer[F]): F[Unit] =
    // fs2's stdin/stdout require LiftIO on Scala Native; on the JVM and JS the
    // instance is otherwise unused, so reference it to satisfy -Wunused:all.
    val _ = LiftIO[F]
    mcp4s.server.Dispatcher[F](server).flatMap { dispatcher =>
      val input: Stream[F, String] =
        stdin[F](DefaultBufferSize)
          .through(text.utf8.decode)
          .through(text.lines)
          .filter(_.nonEmpty)

      val process: Pipe[F, String, String] = _.evalMap { line =>
        parseAndDispatch(dispatcher, line)
      }.unNone

      input
        .through(process)
        .map(_ + "\n")
        .through(text.utf8.encode)
        .through(stdout[F])
        .compile
        .drain
    }

  private def parseAndDispatch[F[_]: Concurrent](
      dispatcher: Dispatcher[F],
      line: String
  ): F[Option[String]] =
    decode[JsonRpcMessage](line) match
      case Right(message) =>
        dispatcher.dispatch(message).map(_.map(_.asJson.noSpaces))
      case Left(err) =>
        val errorResponse = JsonRpcErrorResponse(
          RequestId.NullId,
          JsonRpcError.parseError(err.getMessage)
        )
        Some(errorResponse.asJson.noSpaces).pure[F]
