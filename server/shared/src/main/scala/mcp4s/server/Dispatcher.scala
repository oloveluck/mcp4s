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
  * Manages server lifecycle (initialization, shutdown) and handles capability negotiation. Supports
  * request cancellation via the notifications/cancelled protocol.
  */
trait Dispatcher[F[_]]:

  /** Process an incoming JSON-RPC message and return the response (if any) */
  def dispatch(message: JsonRpcMessage): F[Option[JsonRpcMessage]]

object Dispatcher:

  /** JSON-RPC error code for cancelled requests */
  private val CancelledErrorCode: Int = -32800

  /** Create a new dispatcher for the given server.
    *
    * @param server
    *   The MCP server to dispatch requests to
    * @param tracer
    *   Optional OpenTelemetry tracer for distributed tracing (defaults to noop)
    */
  def apply[F[_]: Concurrent](server: Server[F])(using Tracer[F]): F[Dispatcher[F]] =
    for
      stateRef     <- Ref.of[F, State](State.Uninitialized)
      inFlightRef  <- Ref.of[F, Map[RequestId, Deferred[F, Unit]]](Map.empty)
      listCacheRef <- Ref.of[F, Map[String, (AnyRef, Json)]](Map.empty)
    yield new DispatcherImpl(server, stateRef, inFlightRef, listCacheRef, None, summon[Tracer[F]])

  /** Create a dispatcher with a ToolContext factory for context-aware tools.
    *
    * @param server
    *   The MCP server to dispatch requests to
    * @param contextFactory
    *   Factory to create ToolContext for each request
    * @param tracer
    *   Optional OpenTelemetry tracer for distributed tracing
    */
  def withContext[F[_]: Concurrent](
      server: Server[F],
      contextFactory: (RequestId, Option[RequestId]) => ToolContext[F]
  )(using Tracer[F]): F[Dispatcher[F]] =
    for
      stateRef     <- Ref.of[F, State](State.Uninitialized)
      inFlightRef  <- Ref.of[F, Map[RequestId, Deferred[F, Unit]]](Map.empty)
      listCacheRef <- Ref.of[F, Map[String, (AnyRef, Json)]](Map.empty)
    yield new DispatcherImpl(
      server,
      stateRef,
      inFlightRef,
      listCacheRef,
      Some(contextFactory),
      summon[Tracer[F]]
    )

  private enum State:
    case Uninitialized
    case Initialized
    case ShuttingDown

  private class DispatcherImpl[F[_]: Concurrent](
      server: Server[F],
      stateRef: Ref[F, State],
      inFlightRequests: Ref[F, Map[RequestId, Deferred[F, Unit]]],
      listCache: Ref[F, Map[String, (AnyRef, Json)]],
      contextFactory: Option[(RequestId, Option[RequestId]) => ToolContext[F]],
      tracer: Tracer[F]
  ) extends Dispatcher[F]:

    def dispatch(message: JsonRpcMessage): F[Option[JsonRpcMessage]] =
      message match
        case req: JsonRpcRequest =>
          tracer
            .span(
              "mcp.request",
              Attribute("mcp.method", req.method),
              Attribute("mcp.request_id", req.id.toString)
            )
            .use: span =>
              handleRequest(req, span).map(Some(_))
        case notif: JsonRpcNotification =>
          tracer.span("mcp.notification", Attribute("mcp.method", notif.method)).surround {
            handleNotification(notif).as(None)
          }
        case _ => Concurrent[F].pure(None)

    private def handleRequest(req: JsonRpcRequest, span: Span[F]): F[JsonRpcMessage] =
      (for
        cancelToken <- Deferred[F, Unit]
        _           <- inFlightRequests.update(_ + (req.id -> cancelToken))
        result <- Concurrent[F]
          .race(
            cancelToken.get,
            handleMethod(req.id, req.method, req.params.getOrElse(Json.obj()))
          )
          .guarantee(inFlightRequests.update(_ - req.id))
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
          // No-op: state already transitioned to Initialized in handleInitialize.
          // The notifications/initialized notification is informational per spec.
          Concurrent[F].unit
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

    /** Encode a list response, reusing the cached JSON while the source list is unchanged.
      *
      * Encoding tool/resource/prompt lists (circe derivation + `dropNullValues`) is the heaviest
      * allocator on these endpoints, yet the lists are static between `list_changed` events. A
      * static server therefore encodes once; a server that swaps its list re-encodes the new one
      * (matched by value equality).
      */
    private def cachedListJson(key: String, source: AnyRef)(encode: => Json): F[Json] =
      listCache.modify: cache =>
        cache.get(key) match
          case Some((prev, json)) if prev == source => (cache, json)
          case _ =>
            val json = encode
            (cache.updated(key, (source, json)), json)

    private def handleToolsCall(reqId: RequestId, params: Json): F[Json] =
      val cursor = params.hcursor
      for
        name <- cursor.get[String]("name").liftTo[F]
        args <- cursor.get[Option[Json]]("arguments").map(_.getOrElse(Json.obj())).liftTo[F]
        progressToken = cursor.downField("_meta").downField("progressToken").as[RequestId].toOption
        result <- contextFactory match
          case Some(factory) =>
            server.callToolWithContext(name, args, factory(reqId, progressToken))
          case None => server.callTool(name, args)
      yield result.asJson

    private def handleResourcesRead(params: Json): F[Json] =
      for
        uri     <- params.hcursor.get[String]("uri").liftTo[F]
        content <- server.readResource(uri)
      yield Json.obj("contents" -> List(content).asJson)

    private def handlePromptsGet(params: Json): F[Json] =
      for
        name   <- params.hcursor.get[String]("name").liftTo[F]
        args   <- params.hcursor.get[Option[Map[String, String]]]("arguments").liftTo[F]
        result <- server.getPrompt(name, args.getOrElse(Map.empty))
      yield result.asJson

    private val emptyCompletion: Json =
      Json.obj(
        "completion" -> Json.obj(
          "values"  -> Json.arr(),
          "total"   -> Json.fromInt(0),
          "hasMore" -> Json.False
        )
      )

    private def handleMethod(reqId: RequestId, method: String, params: Json): F[Json] =
      method match
        case McpMethod.Initialize =>
          handleInitialize(params)

        case McpMethod.Ping =>
          Concurrent[F].pure(Json.obj())

        case McpMethod.ToolsList =>
          requireInitialized *> server.listTools.flatMap: tools =>
            cachedListJson(McpMethod.ToolsList, tools)(Json.obj("tools" -> tools.asJson))

        case McpMethod.ToolsCall =>
          requireInitialized *> handleToolsCall(reqId, params)

        case McpMethod.ResourcesList =>
          requireInitialized *> server.listResources.flatMap: resources =>
            cachedListJson(McpMethod.ResourcesList, resources)(
              Json.obj("resources" -> resources.asJson)
            )

        case McpMethod.ResourcesTemplatesList =>
          requireInitialized *> server.listResourceTemplates.flatMap: templates =>
            cachedListJson(McpMethod.ResourcesTemplatesList, templates)(
              Json.obj("resourceTemplates" -> templates.asJson)
            )

        case McpMethod.ResourcesRead =>
          requireInitialized *> handleResourcesRead(params)

        case McpMethod.PromptsList =>
          requireInitialized *> server.listPrompts.flatMap: prompts =>
            cachedListJson(McpMethod.PromptsList, prompts)(Json.obj("prompts" -> prompts.asJson))

        case McpMethod.PromptsGet =>
          requireInitialized *> handlePromptsGet(params)

        case McpMethod.Shutdown =>
          stateRef.set(State.ShuttingDown).as(Json.obj())

        // Accepted-but-no-op methods (acknowledged per spec).
        case McpMethod.LoggingSetLevel =>
          requireInitialized *> params.hcursor.get[String]("level").liftTo[F].as(Json.obj())

        case McpMethod.ResourcesSubscribe =>
          requireInitialized *> params.hcursor.get[String]("uri").liftTo[F].as(Json.obj())

        case McpMethod.ResourcesUnsubscribe =>
          requireInitialized *> params.hcursor.get[String]("uri").liftTo[F].as(Json.obj())

        case McpMethod.CompletionComplete =>
          requireInitialized *> Concurrent[F].pure(emptyCompletion)

        case other =>
          Concurrent[F].raiseError(McpError.MethodNotFound(other))

    private def handleInitialize(params: Json): F[Json] =
      params
        .as[InitializeParams]
        .liftTo[F]
        .flatMap: _ =>
          // Accept any version and respond with our supported version.
          // Per MCP spec, server responds with the version it supports,
          // and the client decides whether to continue.
          stateRef.modify {
            case State.Uninitialized =>
              val result = InitializeResult(
                protocolVersion = McpVersion.Current,
                capabilities = server.capabilities,
                serverInfo = server.info
              )
              // Transition to Initialized immediately — per spec, the server should
              // accept requests after responding to initialize. The subsequent
              // notifications/initialized is informational.
              (State.Initialized, result.asJson.pure[F])
            case _ =>
              (State.Initialized, Concurrent[F].raiseError[Json](McpError.AlreadyInitialized))
          }.flatten

    private def requireInitialized: F[Unit] =
      stateRef.get.flatMap:
        case State.Initialized => Concurrent[F].unit
        case State.Uninitialized =>
          Concurrent[F].raiseError(McpError.NotInitialized)
        case State.ShuttingDown =>
          Concurrent[F].raiseError(McpError.InternalError("Server is shutting down"))
