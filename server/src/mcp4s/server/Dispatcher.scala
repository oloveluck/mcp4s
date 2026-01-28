package mcp4s.server

import cats.effect.{Concurrent, Deferred, Ref}
import cats.effect.syntax.monadCancel.*
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.{Span, Tracer}
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

  /** Create a new dispatcher for the given server.
    *
    * @param server The MCP server to dispatch requests to
    * @param tracer Optional OpenTelemetry tracer for distributed tracing (defaults to noop)
    */
  def apply[F[_]: Concurrent](server: McpServer[F])(using Tracer[F]): F[Dispatcher[F]] =
    for
      stateRef <- Ref.of[F, State](State.Uninitialized)
      inFlightRef <- Ref.of[F, Map[RequestId, Deferred[F, Unit]]](Map.empty)
    yield new DispatcherImpl(server, stateRef, inFlightRef, None, summon[Tracer[F]])

  /** Create a dispatcher with a ToolContext factory for context-aware tools.
    *
    * @param server The MCP server to dispatch requests to
    * @param contextFactory Factory to create ToolContext for each request
    * @param tracer Optional OpenTelemetry tracer for distributed tracing
    */
  def withContext[F[_]: Concurrent](
      server: McpServer[F],
      contextFactory: RequestId => ToolContext[F]
  )(using Tracer[F]): F[Dispatcher[F]] =
    for
      stateRef <- Ref.of[F, State](State.Uninitialized)
      inFlightRef <- Ref.of[F, Map[RequestId, Deferred[F, Unit]]](Map.empty)
    yield new DispatcherImpl(server, stateRef, inFlightRef, Some(contextFactory), summon[Tracer[F]])

  private enum State:
    case Uninitialized
    case Initialized
    case ShuttingDown

  private class DispatcherImpl[F[_]: Concurrent](
      server: McpServer[F],
      stateRef: Ref[F, State],
      inFlightRequests: Ref[F, Map[RequestId, Deferred[F, Unit]]],
      contextFactory: Option[RequestId => ToolContext[F]],
      tracer: Tracer[F]
  ) extends Dispatcher[F]:

    def dispatch(message: JsonRpcMessage): F[Option[JsonRpcMessage]] =
      message match
        case req: JsonRpcRequest =>
          tracer.span("mcp.request", Attribute("mcp.method", req.method), Attribute("mcp.request_id", req.id.toString)).use { span =>
            handleRequest(req, span).map(Some(_))
          }
        case notif: JsonRpcNotification =>
          tracer.span("mcp.notification", Attribute("mcp.method", notif.method)).surround {
            handleNotification(notif).as(None)
          }
        case _ => Concurrent[F].pure(None)

    private def handleRequest(req: JsonRpcRequest, span: Span[F]): F[JsonRpcMessage] =
      (for
        cancelToken <- Deferred[F, Unit]
        _ <- inFlightRequests.update(_ + (req.id -> cancelToken))
        result <- Concurrent[F].race(
          cancelToken.get,
          handleMethod(req.id, req.method, req.params.getOrElse(Json.obj()))
        ).guarantee(inFlightRequests.update(_ - req.id))
      yield result match
        case Left(_) =>
          JsonRpcErrorResponse(
            req.id,
            JsonRpcError(CancelledErrorCode, "Request cancelled", None)
          )
        case Right(json) =>
          JsonRpcResponse(req.id, json)
      ).handleErrorWith { err =>
        val rpcError = err match
          case e: McpError => McpError.toJsonRpcError(e)
          case e           => JsonRpcError.internalError(e.getMessage)
        span.addAttribute(Attribute("error", true)) *>
          span.addAttribute(Attribute("error.type", err.getClass.getSimpleName)) *>
          span.addAttribute(Attribute("error.message", err.getMessage)) *>
          Concurrent[F].pure(JsonRpcErrorResponse(req.id, rpcError))
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
              inFlightRequests.modify { map =>
                map.get(cp.requestId) match
                  case Some(token) => (map - cp.requestId, token.complete(()).void)
                  case None        => (map, Concurrent[F].unit)
              }.flatten
            case None =>
              Concurrent[F].unit // Invalid params, ignore
        case _ =>
          Concurrent[F].unit

    private def handleMethod(reqId: RequestId, method: String, params: Json): F[Json] =
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
              result <- contextFactory match
                case Some(factory) =>
                  val ctx = factory(reqId)
                  server.callToolWithContext(name, args, ctx)
                case None =>
                  server.callTool(name, args)
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
        // Accept any version and respond with our supported version.
        // Per MCP spec, server responds with the version it supports,
        // and the client decides whether to continue.
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

    private def requireInitialized: F[Unit] =
      stateRef.get.flatMap {
        case State.Initialized => Concurrent[F].unit
        case State.Uninitialized =>
          Concurrent[F].raiseError(McpError.NotInitialized())
        case State.ShuttingDown =>
          Concurrent[F].raiseError(McpError.InternalError("Server is shutting down"))
      }
