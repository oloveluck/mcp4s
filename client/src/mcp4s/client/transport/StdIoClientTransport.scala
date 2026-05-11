package mcp4s.client.transport

import cats.effect.{Async, Deferred, Ref, Resource as CatsResource}
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.Stream
import fs2.io.process.ProcessBuilder
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.{McpClient, McpConnection}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import fs2.io.process.Processes
import cats.effect.syntax.spawn.genSpawnOps

/** Configuration for stdio-based MCP server connection */
case class StdioClientConfig(
    command: String,
    args: List[String] = List.empty,
    workingDirectory: Option[String] = None,
    env: Map[String, String] = Map.empty
)

/** Transport for communicating with MCP servers via standard input/output */
object StdioClientTransport:

  /** Connect to an MCP server via stdio by spawning a process.
    *
    * @param client The MCP client configuration
    * @param config Configuration for the process to spawn
    * @param tracer Optional OpenTelemetry tracer for distributed tracing (defaults to noop)
    */
  def connect[F[_]: Async: Processes](
      client: McpClient[F],
      config: StdioClientConfig
  )(using Tracer[F]): CatsResource[F, McpConnection[F]] =
    for
      // Create queues for message passing
      inputQueue <- CatsResource.eval(Queue.unbounded[F, String])
      responseMap <- CatsResource.eval(Ref.of[F, Map[RequestId, Deferred[F, Json]]](Map.empty))

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

      // Handle stdout - read from process and parse JSON-RPC messages
      _ <- CatsResource.eval(
        process.stdout
          .through(fs2.text.utf8.decode)
          .through(fs2.text.lines)
          .filter(_.trim.nonEmpty)
          .evalMap: line =>
            parse(line) match
              case Right(json) => handleResponse(json, responseMap, progressHandlersRef)
              case Left(err) =>
                Async[F].delay(System.err.println(s"[MCP Client] Failed to parse response: $err"))
          .compile
          .drain
          .start
      )

      // Handle stderr - log errors
      _ <- CatsResource.eval(
        process.stderr
          .through(fs2.text.utf8.decode)
          .through(fs2.text.lines)
          .filter(_.trim.nonEmpty)
          .evalMap: line =>
            Async[F].delay(System.err.println(s"[MCP Server stderr]: $line"))
          .compile
          .drain
          .start
      )

      // Handle stdin - write messages to process
      _ <- CatsResource.eval(
        Stream
          .fromQueueUnterminated(inputQueue)
          .map(x => x+"\n")
          .through(fs2.text.utf8.encode)
          .through(process.stdin)
          .compile
          .drain
          .start
      )

      // Create the connection
      connection <- CatsResource.eval(
        establishConnection(client, inputQueue, responseMap, progressHandlersRef, summon[Tracer[F]])
      )

    yield connection

  /** Handle incoming JSON-RPC responses and notifications */
  private def handleResponse[F[_]: Async](
      json: Json,
      responseMap: Ref[F, Map[RequestId, Deferred[F, Json]]],
      progressHandlersRef: Ref[F, Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]]]
  ): F[Unit] =
    json.as[JsonRpcMessage] match
      case Right(JsonRpcResponse(id, result)) =>
        // Handle response - complete the waiting request
        responseMap.modify { map =>
          map.get(id) match
            case Some(deferred) => (map - id, deferred.complete(result).void)
            case None =>
              // Response without matching request - log warning
              (
                map,
                Async[F].delay(
                  System.err.println(s"[MCP Client] Received response for unknown request: $id")
                )
              )
        }.flatten

      case Right(JsonRpcErrorResponse(id, error)) =>
        // Handle error response - complete with error
        responseMap.modify { map =>
          map.get(id) match
            case Some(deferred) =>
              (
                map - id,
                deferred.complete(Json.obj("error" -> error.asJson)).void
              )
            case None =>
              (
                map,
                Async[F].delay(
                  System.err.println(
                    s"[MCP Client] Received error response for unknown request: $id"
                  )
                )
              )
        }.flatten

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
      responseMap: Ref[F, Map[RequestId, Deferred[F, Json]]],
      progressHandlersRef: Ref[F, Option[Ref[F, Map[RequestId, ProgressParams => F[Unit]]]]],
      tracer: Tracer[F]
  ): F[McpConnection[F]] =
    val sendRequest = createRequestSender(inputQueue, responseMap)
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
      _ <- sendNotification(JsonRpcNotification(McpMethod.Initialized, None))
      conn <- McpConnection[F](
        initResult.serverInfo, initResult.capabilities,
        sendRequest, sendNotification, tracer
      )
      _ <- progressHandlersRef.set(Some(conn.progressHandlers))
    yield conn

  /** Send initialize request and parse response */
  private def sendInitRequest[F[_]: Async](
      initRequest: JsonRpcRequest,
      sendRequest: JsonRpcRequest => F[Json]
  ): F[InitializeResult] =
    sendRequest(initRequest).flatMap: responseJson =>
      responseJson.hcursor.downField("error").focus match
        case Some(errorJson) =>
          errorJson.as[JsonRpcError] match
            case Right(error) =>
              Async[F].raiseError(McpError.fromJsonRpcError(error))
            case Left(_) =>
              Async[F].raiseError(
                McpError.InternalError("Failed to parse error response")
              )
        case None =>
          responseJson.as[InitializeResult] match
            case Right(result) => Async[F].pure(result)
            case Left(err) =>
              Async[F].raiseError(
                McpError.InternalError(s"Failed to parse initialize response: ${err.getMessage}")
              )

  /** Create a request sender function that handles request/response correlation */
  private def createRequestSender[F[_]: Async](
      inputQueue: Queue[F, String],
      responseMap: Ref[F, Map[RequestId, Deferred[F, Json]]]
  ): JsonRpcRequest => F[Json] = req =>
    import cats.effect.syntax.all.monadCancelOps_
    for
      // Create a deferred to wait for the response
      deferred <- Deferred[F, Json]

      // Register the request
      _ <- responseMap.update(_ + (req.id -> deferred))

      // Send the request
      _ <- inputQueue.offer(req.asJson.noSpaces)

      // Wait for the response (with cleanup on error/cancellation)
      result <- deferred.get.guarantee(responseMap.update(_ - req.id))

      // Check if response contains an error
      parsed <- result.hcursor.downField("error").focus match
        case Some(errorJson) =>
          errorJson.as[JsonRpcError] match
            case Right(error) =>
              Async[F].raiseError(McpError.fromJsonRpcError(error))
            case Left(_) =>
              Async[F].pure(result)
        case None =>
          Async[F].pure(result)
    yield parsed

  /** Create a notification sender function */
  private def createNotificationSender[F[_]](
      inputQueue: Queue[F, String]
  ): JsonRpcNotification => F[Unit] = notif =>
    inputQueue.offer(notif.asJson.noSpaces)
