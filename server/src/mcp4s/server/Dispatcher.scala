package mcp4s.server

import cats.effect.{Concurrent, Deferred, Ref}
import cats.effect.syntax.monadCancel.*
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given

/** Dispatcher handles incoming JSON-RPC messages and routes them to the appropriate server methods.
  *
  * Manages server lifecycle (initialization, shutdown) and handles capability negotiation.
  * Supports request cancellation via the notifications/cancelled protocol.
  */
trait Dispatcher[F[_]]:

  /** Process an incoming JSON-RPC message and return the response (if any) */
  def dispatch(message: JsonRpcMessage): F[Option[JsonRpcMessage]]

object Dispatcher:

  /** JSON-RPC error code for cancelled requests */
  private val CancelledErrorCode: Int = -32800

  /** Create a new dispatcher for the given server */
  def apply[F[_]: Concurrent](server: McpServer[F]): F[Dispatcher[F]] =
    for
      stateRef <- Ref.of[F, State](State.Uninitialized)
      inFlightRef <- Ref.of[F, Map[RequestId, Deferred[F, Unit]]](Map.empty)
    yield new DispatcherImpl(server, stateRef, inFlightRef)

  private enum State:
    case Uninitialized
    case Initialized
    case ShuttingDown

  private class DispatcherImpl[F[_]: Concurrent](
      server: McpServer[F],
      stateRef: Ref[F, State],
      inFlightRequests: Ref[F, Map[RequestId, Deferred[F, Unit]]]
  ) extends Dispatcher[F]:

    def dispatch(message: JsonRpcMessage): F[Option[JsonRpcMessage]] =
      message match
        case req: JsonRpcRequest      => handleRequest(req).map(Some(_))
        case notif: JsonRpcNotification => handleNotification(notif).as(None)
        case _                        => Concurrent[F].pure(None)

    private def handleRequest(req: JsonRpcRequest): F[JsonRpcMessage] =
      (for
        cancelToken <- Deferred[F, Unit]
        _ <- inFlightRequests.update(_ + (req.id -> cancelToken))
        result <- Concurrent[F].race(
          cancelToken.get,
          handleMethod(req.method, req.params.getOrElse(Json.obj()))
        ).guarantee(inFlightRequests.update(_ - req.id))
      yield result match
        case Left(_) =>
          JsonRpcErrorResponse(
            req.id,
            JsonRpcError(CancelledErrorCode, "Request cancelled", None)
          )
        case Right(json) =>
          JsonRpcResponse(req.id, json)
      ).handleError { err =>
        val rpcError = err match
          case e: McpError => McpError.toJsonRpcError(e)
          case e           => JsonRpcError.internalError(e.getMessage)
        JsonRpcErrorResponse(req.id, rpcError)
      }

    private def handleNotification(notif: JsonRpcNotification): F[Unit] =
      notif.method match
        case McpMethod.Initialized =>
          stateRef.update {
            case State.Uninitialized => State.Initialized
            case s                   => s
          }
        case McpMethod.Cancelled =>
          notif.params.flatMap(_.as[CancelledParams].toOption) match
            case Some(cp) =>
              inFlightRequests.get.flatMap(_.get(cp.requestId) match
                case Some(token) => token.complete(()).void
                case None        => Concurrent[F].unit // Already completed
              )
            case None =>
              Concurrent[F].unit // Invalid params, ignore
        case _ =>
          Concurrent[F].unit

    private def handleMethod(method: String, params: Json): F[Json] =
      method match
        case McpMethod.Initialize =>
          handleInitialize(params)

        case McpMethod.Ping =>
          Concurrent[F].pure(Json.obj())

        case McpMethod.ToolsList =>
          requireInitialized *> server.listTools.map { tools =>
            Json.obj("tools" -> tools.asJson)
          }

        case McpMethod.ToolsCall =>
          requireInitialized *> {
            val cursor = params.hcursor
            for
              name <- cursor.get[String]("name").liftTo[F]
              args <- cursor.get[Option[Json]]("arguments").map(_.getOrElse(Json.obj())).liftTo[F]
              result <- server.callTool(name, args)
            yield result.asJson
          }

        case McpMethod.ResourcesList =>
          requireInitialized *> server.listResources.map { resources =>
            Json.obj("resources" -> resources.asJson)
          }

        case McpMethod.ResourcesTemplatesList =>
          requireInitialized *> server.listResourceTemplates.map { templates =>
            Json.obj("resourceTemplates" -> templates.asJson)
          }

        case McpMethod.ResourcesRead =>
          requireInitialized *> {
            val cursor = params.hcursor
            for
              uri <- cursor.get[String]("uri").liftTo[F]
              content <- server.readResource(uri)
            yield Json.obj("contents" -> List(content).asJson)
          }

        case McpMethod.PromptsList =>
          requireInitialized *> server.listPrompts.map { prompts =>
            Json.obj("prompts" -> prompts.asJson)
          }

        case McpMethod.PromptsGet =>
          requireInitialized *> {
            val cursor = params.hcursor
            for
              name <- cursor.get[String]("name").liftTo[F]
              args <- cursor.get[Option[Map[String, String]]]("arguments").liftTo[F]
              result <- server.getPrompt(name, args.getOrElse(Map.empty))
            yield result.asJson
          }

        case McpMethod.Shutdown =>
          stateRef.set(State.ShuttingDown).as(Json.obj())

        case other =>
          Concurrent[F].raiseError(McpError.MethodNotFound(other))

    private def handleInitialize(params: Json): F[Json] =
      params.as[InitializeParams].liftTo[F].flatMap { initParams =>
        val clientVersion = initParams.protocolVersion

        // Validate protocol version - accept current and backwards compatible versions
        if !isCompatibleVersion(clientVersion) then
          Concurrent[F].raiseError(
            McpError.ProtocolVersionMismatch(clientVersion, McpVersion.Current)
          )
        else
          stateRef.get.flatMap {
            case State.Uninitialized =>
              val result = InitializeResult(
                protocolVersion = McpVersion.Current,
                capabilities = server.capabilities,
                serverInfo = server.info
              )
              result.asJson.pure[F]
            case _ =>
              Concurrent[F].raiseError(McpError.AlreadyInitialized())
          }
      }

    private def isCompatibleVersion(version: String): Boolean =
      McpVersion.Supported.contains(version)

    private def requireInitialized: F[Unit] =
      stateRef.get.flatMap {
        case State.Initialized => Concurrent[F].unit
        case State.Uninitialized =>
          Concurrent[F].raiseError(McpError.NotInitialized())
        case State.ShuttingDown =>
          Concurrent[F].raiseError(McpError.InternalError("Server is shutting down"))
      }
