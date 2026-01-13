package mcp4s.server.transport

import cats.effect.{Async, Concurrent}
import cats.effect.std.Console
import cats.syntax.all.*
import fs2.{Pipe, Stream}
import fs2.io.stdin
import fs2.io.stdout
import fs2.text
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.*

/** Stdio transport for MCP servers.
  *
  * Reads JSON-RPC messages from stdin (one per line) and writes responses to stdout.
  * This transport is suitable for local tool servers communicating with clients
  * like Claude Desktop.
  */
object StdioTransport:

  /** Default buffer size for reading from stdin */
  private val DefaultBufferSize: Int = 4096

  /** Run the MCP server using stdio transport.
    *
    * Reads newline-delimited JSON-RPC messages from stdin and writes responses to stdout.
    * Runs until stdin is closed or the server receives a shutdown request.
    */
  def run[F[_]: Async: Console](server: McpServer[F]): F[Unit] =
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
