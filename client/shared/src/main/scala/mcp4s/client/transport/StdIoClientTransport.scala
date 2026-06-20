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

import cats.effect.{Async, Ref, Resource as CatsResource}
import cats.effect.std.Queue
import cats.effect.syntax.spawn.genSpawnOps
import cats.syntax.all.*
import fs2.Stream
import fs2.io.process.{ProcessBuilder, Processes}
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.RequestCorrelator
import mcp4s.client.{McpClient, McpConnection}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given

/** Configuration for stdio-based MCP server connection */
case class StdioClientConfig(
    command: String,
    args: List[String] = List.empty,
    workingDirectory: Option[String] = None,
    env: Map[String, String] = Map.empty,
    /** Bound on the outgoing message queue, for backpressure. */
    maxQueueSize: Int = 1024
)

/** Transport for communicating with MCP servers via standard input/output */
object StdioClientTransport:

  /** Connect to an MCP server via stdio by spawning a process.
    *
    * @param client
    *   The MCP client configuration
    * @param config
    *   Configuration for the process to spawn
    * @param tracer
    *   Optional OpenTelemetry tracer for distributed tracing (defaults to noop)
    */
  def connect[F[_]: Async: Processes](
      client: McpClient[F],
      config: StdioClientConfig
  )(using Tracer[F]): CatsResource[F, McpConnection[F]] =
    for
      // Create queue + correlator for message passing
      inputQueue <- CatsResource.eval(Queue.bounded[F, String](config.maxQueueSize))
      correlator <- CatsResource.eval(RequestCorrelator[F])

      // Indirection for progress handlers - set after connection creation
      progressHandlersRef <- CatsResource.eval(
        Ref.of[F, Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]]](None)
      )

      // Build the process
      processBuilder = ProcessBuilder(
        config.command,
        config.args
      ).withWorkingDirectory(
        config.workingDirectory
          .map(fs2.io.file.Path(_))
          .getOrElse(fs2.io.file.Path("./"))
      )

      // Spawn the process
      process <- processBuilder.spawn[F]

      // Handle stdout - read from process and parse JSON-RPC messages.
      // Each pump runs in a background fiber that is cancelled when the
      // connection resource is released (Resource.make .. _.cancel).
      _ <- CatsResource.make(
        process.stdout
          .through(fs2.text.utf8.decode)
          .through(fs2.text.lines)
          .filter(_.trim.nonEmpty)
          .evalMap: line =>
            parse(line) match
              case Right(json) => handleResponse(json, correlator, progressHandlersRef)
              case Left(err) =>
                Async[F].delay(System.err.println(s"[MCP Client] Failed to parse response: $err"))
          .compile
          .drain
          .start
      )(_.cancel)

      // Handle stderr - log errors
      _ <- CatsResource.make(
        process.stderr
          .through(fs2.text.utf8.decode)
          .through(fs2.text.lines)
          .filter(_.trim.nonEmpty)
          .evalMap: line =>
            Async[F].delay(System.err.println(s"[MCP Server stderr]: $line"))
          .compile
          .drain
          .start
      )(_.cancel)

      // Handle stdin - write messages to process
      _ <- CatsResource.make(
        Stream
          .fromQueueUnterminated(inputQueue)
          .map(x => x + "\n")
          .through(fs2.text.utf8.encode)
          .through(process.stdin)
          .compile
          .drain
          .start
      )(_.cancel)

      // Create the connection
      connection <- CatsResource.eval(
        establishConnection(client, inputQueue, correlator, progressHandlersRef, summon[Tracer[F]])
      )
    yield connection

  /** Handle incoming JSON-RPC responses and notifications */
  private def handleResponse[F[_]: Async](
      json: Json,
      correlator: RequestCorrelator[F],
      progressHandlersRef: Ref[F, Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]]]
  ): F[Unit] =
    json.as[JsonRpcMessage] match
      case Right(JsonRpcResponse(id, result)) =>
        correlator.complete(id, result)

      case Right(JsonRpcErrorResponse(id, error)) =>
        correlator.fail(id, error)

      case Right(JsonRpcNotification(method, params)) if method == McpMethod.Progress =>
        // Route progress notifications to registered handlers
        val pp = params.flatMap(_.as[ProgressParams].toOption)
        pp.traverse_ { p =>
          progressHandlersRef.get.flatMap(_.traverse_ { handlers =>
            handlers.get.flatMap(_.get(p.progressToken).traverse_(_(p)))
          })
        }

      case Right(JsonRpcNotification(method, params)) =>
        // Handle other notifications from server (e.g., logging)
        Async[F].delay(
          System.err.println(
            s"[MCP Server notification] $method: ${params.map(_.noSpaces).getOrElse("")}"
          )
        )

      case Right(JsonRpcRequest(id, method, params)) =>
        // Handle request from server (not typically expected in client)
        Async[F].delay(
          System.err.println(
            s"[MCP Server request] $method (id: $id): ${params.map(_.noSpaces).getOrElse("")}"
          )
        )

      case Left(err) =>
        Async[F].delay(
          System.err.println(s"[MCP Client] Failed to decode JSON-RPC message: $err")
        )

  /** Establish connection and perform initialization handshake */
  private def establishConnection[F[_]: Async](
      client: McpClient[F],
      inputQueue: Queue[F, String],
      correlator: RequestCorrelator[F],
      progressHandlersRef: Ref[F, Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]]],
      tracer: Tracer[F]
  ): F[McpConnection[F]] =
    val sendRequest      = createRequestSender(inputQueue, correlator)
    val sendNotification = createNotificationSender(inputQueue)
    val initRequest = JsonRpcRequest(
      RequestId.NumberId(1),
      McpMethod.Initialize,
      Some(
        InitializeParams(
          protocolVersion = McpVersion.Current,
          capabilities = client.capabilities,
          clientInfo = client.info
        ).asJson
      )
    )
    for
      initResult <- sendInitRequest(initRequest, sendRequest)
      _          <- sendNotification(JsonRpcNotification(McpMethod.Initialized, None))
      conn <- McpConnection[F](
        initResult.serverInfo,
        initResult.capabilities,
        sendRequest,
        sendNotification,
        tracer
      )
      _ <- progressHandlersRef.set(Some(conn.progressHandlers))
    yield conn

  /** Send initialize request and parse response */
  private def sendInitRequest[F[_]: Async](
      initRequest: JsonRpcRequest,
      sendRequest: JsonRpcRequest => F[Json]
  ): F[InitializeResult] =
    sendRequest(initRequest).flatMap: responseJson =>
      responseJson.as[InitializeResult] match
        case Right(result) => Async[F].pure(result)
        case Left(err) =>
          Async[F].raiseError(
            McpError.InternalError(s"Failed to parse initialize response: ${err.getMessage}")
          )

  /** Create a request sender that correlates responses by request id (awaits indefinitely). */
  private def createRequestSender[F[_]](
      inputQueue: Queue[F, String],
      correlator: RequestCorrelator[F]
  ): JsonRpcRequest => F[Json] = req =>
    correlator.requestUntimed(req.id)(inputQueue.offer(req.asJson.noSpaces))

  /** Create a notification sender function */
  private def createNotificationSender[F[_]](
      inputQueue: Queue[F, String]
  ): JsonRpcNotification => F[Unit] = notif => inputQueue.offer(notif.asJson.noSpaces)
