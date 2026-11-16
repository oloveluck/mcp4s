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

import cats.effect.{Deferred, Ref, Temporal}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.{Span, Tracer}
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given

import scala.concurrent.duration.*

/** The per-session MCP protocol state machine.
  *
  * A dispatcher routes incoming JSON-RPC messages to the [[Server]] and owns everything the
  * protocol scopes to one session: the initialize handshake and negotiated version, the client's
  * advertised capabilities, in-flight requests (for `notifications/cancelled`), the logging level
  * set via `logging/setLevel`, resource subscriptions, and task-augmented tool calls. Transports
  * create one dispatcher per connection (or per HTTP session) and read the session state back
  * through the accessors below.
  */
trait Dispatcher[F[_]]:

  /** Process an incoming JSON-RPC message and return the response (if any) */
  def dispatch(message: JsonRpcMessage): F[Option[JsonRpcMessage]]

  /** The protocol version negotiated during `initialize`, once initialized. */
  def protocolVersion: F[Option[String]]

  /** The capabilities the client advertised during `initialize`, once initialized. */
  def clientCapabilities: F[Option[ClientCapabilities]]

  /** The minimum log level requested via `logging/setLevel`; `None` until the client sets one. */
  def logLevel: F[Option[LogLevel]]

  /** Resource URIs the client subscribed to with `resources/subscribe`. */
  def subscriptions: F[Set[String]]

  /** Release session-scoped work: cancels running tasks. Safe to call more than once. */
  def close: F[Unit]

object Dispatcher:

  /** Settings for task-augmented requests (SEP-1686). */
  final case class TaskConfig(
      /** Retention applied when the client requests no `ttl`; `None` retains tasks until `close`.
        */
      defaultTtl: Option[FiniteDuration] = Some(10.minutes),
      /** Polling interval suggested to clients. */
      pollInterval: FiniteDuration = 500.millis,
      /** Upper bound on tasks retained per session; creation fails beyond it. */
      maxTasks: Int = 1000,
      /** Page size for `tasks/list`. */
      pageSize: Int = 50
  )

  object TaskConfig:
    val default: TaskConfig = TaskConfig()

  final case class Config(
      /** Accept `initialize` on an already-initialized dispatcher (stateless HTTP, where one
        * dispatcher serves every client) instead of answering `AlreadyInitialized`.
        */
      allowReinitialize: Boolean = false,
      tasks: TaskConfig = TaskConfig.default
  )

  object Config:
    val default: Config = Config()

  /** A dispatcher whose tools run without server-to-client context (no sampling, elicitation,
    * progress or logging). Suitable for request/response-only use and tests.
    */
  def apply[F[_]: Temporal](server: Server[F], config: Config = Config.default)(using
      Tracer[F]
  ): F[Dispatcher[F]] =
    create(server, None, config)

  /** A dispatcher whose tool calls receive a [[ToolContext]] built by `contextFactory` from the
    * request id and the client's `_meta.progressToken`, if any.
    */
  def withContext[F[_]: Temporal](
      server: Server[F],
      contextFactory: (RequestId, Option[RequestId]) => F[ToolContext[F]],
      config: Config = Config.default
  )(using Tracer[F]): F[Dispatcher[F]] =
    create(server, Some(contextFactory), config)

  private def create[F[_]: Temporal](
      server: Server[F],
      contextFactory: Option[(RequestId, Option[RequestId]) => F[ToolContext[F]]],
      config: Config
  )(using tracer: Tracer[F]): F[Dispatcher[F]] =
    for
      stateRef     <- Ref.of[F, State](State.Uninitialized)
      inFlightRef  <- Ref.of[F, Map[RequestId, Deferred[F, Unit]]](Map.empty)
      listCacheRef <- Ref.of[F, Map[String, (AnyRef, Json)]](Map.empty)
      logLevelRef  <- Ref.of[F, Option[LogLevel]](None)
      subsRef      <- Ref.of[F, Set[String]](Set.empty)
      supportRef   <- Ref.of[F, Option[(List[Tool], Map[String, TaskSupport])]](None)
      tasks        <- TaskRegistry[F](config.tasks)
    yield new Impl(
      server,
      config,
      contextFactory,
      stateRef,
      inFlightRef,
      listCacheRef,
      logLevelRef,
      subsRef,
      supportRef,
      tasks,
      tracer
    )

  private enum State:
    case Uninitialized
    case Initialized(protocolVersion: String, clientCapabilities: ClientCapabilities)

  private class Impl[F[_]](
      server: Server[F],
      config: Config,
      contextFactory: Option[(RequestId, Option[RequestId]) => F[ToolContext[F]]],
      stateRef: Ref[F, State],
      inFlightRequests: Ref[F, Map[RequestId, Deferred[F, Unit]]],
      listCache: Ref[F, Map[String, (AnyRef, Json)]],
      logLevelRef: Ref[F, Option[LogLevel]],
      subscriptionsRef: Ref[F, Set[String]],
      taskSupportIndex: Ref[F, Option[(List[Tool], Map[String, TaskSupport])]],
      tasks: TaskRegistry[F],
      tracer: Tracer[F]
  )(using F: Temporal[F])
      extends Dispatcher[F]:

    def protocolVersion: F[Option[String]] =
      stateRef.get.map:
        case State.Initialized(v, _) => Some(v)
        case _                       => None

    def clientCapabilities: F[Option[ClientCapabilities]] =
      stateRef.get.map:
        case State.Initialized(_, caps) => Some(caps)
        case _                          => None

    def logLevel: F[Option[LogLevel]] = logLevelRef.get

    def subscriptions: F[Set[String]] = subscriptionsRef.get

    def close: F[Unit] = tasks.close

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
        case _ => F.pure(None)

    private def handleRequest(req: JsonRpcRequest, span: Span[F]): F[JsonRpcMessage] =
      (for
        cancelToken <- Deferred[F, Unit]
        _           <- inFlightRequests.update(_ + (req.id -> cancelToken))
        result      <- F
          .race(
            cancelToken.get,
            handleMethod(req.id, req.method, req.params.getOrElse(Json.obj()))
          )
          .guarantee(inFlightRequests.update(_ - req.id))
      yield result match
        case Left(_) =>
          JsonRpcErrorResponse(
            req.id,
            JsonRpcError(JsonRpcErrorCode.RequestCancelled, "Request cancelled", None)
          )
        case Right(json) =>
          JsonRpcResponse(req.id, json)
      ).handleErrorWith { err =>
        span.addAttribute(Attribute("error", true)) *>
          span.addAttribute(Attribute("error.type", err.getClass.getSimpleName)) *>
          span.addAttribute(Attribute("error.message", err.getMessage)) *>
          F.pure(JsonRpcErrorResponse(req.id, toRpcError(err)))
      }

    private def toRpcError(err: Throwable): JsonRpcError =
      err match
        case e: McpError => McpError.toJsonRpcError(e)
        case e           => JsonRpcError.internalError(e.getMessage)

    private def handleNotification(notif: JsonRpcNotification): F[Unit] =
      notif.method match
        case McpMethod.Cancelled =>
          // Spec: this MUST NOT cancel tasks; those use tasks/cancel.
          notif.params.flatMap(_.as[CancelledParams].toOption) match
            case Some(cp) =>
              inFlightRequests.modify { map =>
                map.get(cp.requestId) match
                  case Some(token) => (map - cp.requestId, token.complete(()).void)
                  case None        => (map, F.unit)
              }.flatten
            case None => F.unit
        case _ =>
          // notifications/initialized is informational: the state already moved to
          // Initialized when the initialize response was produced.
          F.unit

    /** Encode a list response, reusing the cached JSON while the source list is unchanged.
      *
      * Encoding tool/resource/prompt lists (circe derivation + `dropNullValues`) is the heaviest
      * allocator on these endpoints, yet the lists are static between `list_changed` events. A
      * static server therefore encodes once; a server that swaps its list re-encodes the new one
      * (matched by value equality).
      */
    private def cachedListJson(key: String, source: AnyRef)(encode: => Json): F[Json] =
      // The encode runs outside the Ref update: Ref.modify may re-evaluate its function
      // under CAS contention, which would re-run the expensive encode. Concurrent misses
      // may encode twice, but the result is identical either way.
      listCache.get
        .map(_.get(key))
        .flatMap:
          case Some((prev, json)) if prev == source => json.pure[F]
          case _                                    =>
            val json = encode
            listCache.update(_.updated(key, (source, json))).as(json)

    private def handleMethod(reqId: RequestId, method: String, params: Json): F[Json] =
      method match
        case McpMethod.Initialize =>
          handleInitialize(params)

        case McpMethod.Ping =>
          Json.obj().pure[F]

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

        case McpMethod.ResourcesSubscribe =>
          requireInitialized *> uriParam(params).flatMap(uri =>
            subscriptionsRef.update(_ + uri).as(Json.obj())
          )

        case McpMethod.ResourcesUnsubscribe =>
          requireInitialized *> uriParam(params).flatMap(uri =>
            subscriptionsRef.update(_ - uri).as(Json.obj())
          )

        case McpMethod.PromptsList =>
          requireInitialized *> server.listPrompts.flatMap: prompts =>
            cachedListJson(McpMethod.PromptsList, prompts)(Json.obj("prompts" -> prompts.asJson))

        case McpMethod.PromptsGet =>
          requireInitialized *> handlePromptsGet(params)

        case McpMethod.LoggingSetLevel =>
          requireInitialized *> params.hcursor
            .get[LogLevel]("level")
            .liftTo[F]
            .flatMap(level => logLevelRef.set(Some(level)))
            .as(Json.obj())

        case McpMethod.CompletionComplete =>
          requireInitialized *> params
            .as[CompleteParams]
            .liftTo[F]
            .flatMap(server.complete)
            .map(c => CompleteResult(c).asJson)

        case McpMethod.TasksGet =>
          requireInitialized *> requireTasks *> taskIdParam(params).flatMap(tasks.get).map(_.asJson)

        case McpMethod.TasksResult =>
          requireInitialized *> requireTasks *> taskIdParam(params).flatMap(tasks.result)

        case McpMethod.TasksCancel =>
          requireInitialized *> requireTasks *> taskIdParam(params)
            .flatMap(tasks.cancel)
            .map(_.asJson)

        case McpMethod.TasksList =>
          requireInitialized *> requireTasks *>
            tasks.list(params.hcursor.get[String]("cursor").toOption).map(_.asJson)

        case other =>
          McpError.MethodNotFound(other).raiseError[F, Json]

    private def uriParam(params: Json): F[String] =
      params.hcursor.get[String]("uri").liftTo[F]

    private def taskIdParam(params: Json): F[String] =
      params.hcursor.get[String]("taskId").liftTo[F]

    private def handleInitialize(params: Json): F[Json] =
      params
        .as[InitializeParams]
        .liftTo[F]
        .flatMap: init =>
          // Version negotiation: echo a supported requested version, otherwise answer with
          // the latest we support and let the client decide whether to continue.
          val negotiated = McpVersion.negotiate(init.protocolVersion)
          val result     = InitializeResult(
            protocolVersion = negotiated,
            capabilities = server.capabilities,
            serverInfo = server.info,
            instructions = server.instructions
          )
          stateRef.modify {
            case State.Uninitialized =>
              // Requests are accepted as soon as the initialize response is produced; the
              // subsequent notifications/initialized is informational.
              (State.Initialized(negotiated, init.capabilities), result.asJson.pure[F])
            case State.Initialized(_, _) if config.allowReinitialize =>
              (State.Initialized(negotiated, init.capabilities), result.asJson.pure[F])
            case other =>
              (other, McpError.AlreadyInitialized.raiseError[F, Json])
          }.flatten

    private def requireInitialized: F[Unit] =
      stateRef.get.flatMap:
        case State.Initialized(_, _) => F.unit
        case State.Uninitialized     => McpError.NotInitialized.raiseError[F, Unit]

    // === Tools ===

    private def handleToolsCall(reqId: RequestId, params: Json): F[Json] =
      val cursor = params.hcursor
      for
        name <- cursor.get[String]("name").liftTo[F]
        args <- cursor.get[Option[Json]]("arguments").map(_.getOrElse(Json.obj())).liftTo[F]
        progressToken = cursor.downField("_meta").downField("progressToken").as[RequestId].toOption
        taskMeta <- cursor.get[Option[TaskMetadata]]("task").liftTo[F]
        result   <- taskMeta match
          case Some(meta) if tasksEnabled => createTask(reqId, name, args, progressToken, meta)
          case _                          =>
            // Spec: without the tasks capability, task metadata is ignored and the call runs
            // normally; with it, a `required` tool must be invoked as a task.
            requireTaskSupportSatisfied(name, taskMeta.isDefined) *>
              callTool(reqId, name, args, progressToken).map(_.asJson)
      yield result

    private def callTool(
        reqId: RequestId,
        name: String,
        args: Json,
        progressToken: Option[RequestId]
    ): F[ToolResult] =
      contextFactory match
        case Some(factory) =>
          factory(reqId, progressToken).flatMap(server.callToolWithContext(name, args, _))
        case None => server.callTool(name, args)

    private def handleResourcesRead(params: Json): F[Json] =
      for
        uri     <- uriParam(params)
        content <- server.readResource(uri)
      yield Json.obj("contents" -> List(content).asJson)

    private def handlePromptsGet(params: Json): F[Json] =
      for
        name   <- params.hcursor.get[String]("name").liftTo[F]
        args   <- params.hcursor.get[Option[Map[String, String]]]("arguments").liftTo[F]
        result <- server.getPrompt(name, args.getOrElse(Map.empty))
      yield result.asJson

    // === Tasks (SEP-1686) ===

    private def tasksEnabled: Boolean =
      server.capabilities.tasks.exists(_.supportsToolCalls)

    private def requireTasks: F[Unit] =
      F.raiseUnless(tasksEnabled)(McpError.MethodNotSupported("tasks"))

    /** Declared task support per tool. The tool list is static between `list_changed` events, so
      * the index is rebuilt only when the list changes (value equality, like the list cache).
      */
    private def taskSupportOf(name: String): F[Option[TaskSupport]] =
      server.listTools.flatMap { tools =>
        taskSupportIndex.get.flatMap {
          case Some((prev, index)) if prev == tools => index.get(name).pure[F]
          case _                                    =>
            val index = tools.map(t => t.name -> t.taskSupport).toMap
            taskSupportIndex.set(Some((tools, index))).as(index.get(name))
        }
      }

    private def requireTaskSupportSatisfied(name: String, asTask: Boolean): F[Unit] =
      if !tasksEnabled || asTask then F.unit
      else
        taskSupportOf(name).flatMap { support =>
          F.raiseWhen(support.contains(TaskSupport.Required))(
            McpError.MethodNotSupported(s"tools/call: tool '$name' must be invoked as a task")
          )
        }

    private def createTask(
        reqId: RequestId,
        name: String,
        args: Json,
        progressToken: Option[RequestId],
        meta: TaskMetadata
    ): F[Json] =
      taskSupportOf(name).flatMap:
        case Some(TaskSupport.Optional) | Some(TaskSupport.Required) =>
          val ttl = meta.ttl.orElse(config.tasks.defaultTtl.map(_.toMillis))
          tasks
            .create(ttl)(callTool(reqId, name, args, progressToken).attempt.map(settle))
            .map(CreateTaskResult(_).asJson)
        case Some(TaskSupport.Forbidden) =>
          McpError
            .MethodNotSupported(s"tools/call: tool '$name' does not support tasks")
            .raiseError[F, Json]
        case None => McpError.ToolNotFound(name).raiseError[F, Json]

    /** 2025-11-25 semantics: a tool result with `isError` is a failed task whose result is still
      * retrievable; a thrown error is a failed task with no result.
      */
    private def settle(result: Either[Throwable, ToolResult]): TaskRegistry.Settled =
      result match
        case Right(r) if r.isError.contains(true) =>
          TaskRegistry.Settled(TaskStatus.Failed, Right(r.asJson))
        case Right(r)  => TaskRegistry.Settled(TaskStatus.Completed, Right(r.asJson))
        case Left(err) =>
          TaskRegistry.Settled(TaskStatus.Failed, Left(toRpcError(err)), Some(err.getMessage))
