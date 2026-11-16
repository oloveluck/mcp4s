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

package mcp4s.client

import scala.annotation.targetName
import scala.concurrent.duration.FiniteDuration
import cats.effect.{Concurrent, Deferred, Ref}
import cats.effect.syntax.monadCancel.*
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given

/** Represents an active connection to an MCP server.
  *
  * Provides methods to invoke server capabilities:
  *   - List and call tools
  *   - List and read resources
  *   - List and get prompts
  *
  * Connections are created via the transport verbs on [[McpClient]]/`McpClientBuilder` —
  * `client.stdio(...)`, `client.http(...)`, and (JVM, `import mcp4s.client.syntax.*`)
  * `client.webSocket(...)` — or directly via a transport object such as
  * [[mcp4s.client.transport.HttpClientTransport]].
  */
trait McpConnection[F[_]]:

  /** Server info received during initialization */
  def serverInfo: ServerInfo

  /** Server capabilities received during initialization */
  def serverCapabilities: ServerCapabilities

  /** The protocol version negotiated during initialization */
  def protocolVersion: String

  /** Usage hints the server returned at initialization, for the model's system prompt */
  def instructions: Option[String]

  // === Capability Checks ===

  /** Check if server supports tools capability */
  def supportsTools: Boolean = serverCapabilities.tools.isDefined

  /** Check if server supports resources capability */
  def supportsResources: Boolean = serverCapabilities.resources.isDefined

  /** Check if server supports prompts capability */
  def supportsPrompts: Boolean = serverCapabilities.prompts.isDefined

  /** Check if server supports resource subscriptions */
  def supportsResourceSubscriptions: Boolean =
    serverCapabilities.resources.exists(_.subscribe.contains(true))

  /** Check if server accepts task-augmented `tools/call` requests */
  def supportsToolTasks: Boolean = serverCapabilities.tasks.exists(_.supportsToolCalls)

  // === Tool Operations ===

  /** List available tools from the server.
    *
    * @param cursor
    *   Optional pagination cursor from a previous response's `nextCursor`
    */
  def listTools(cursor: Option[String] = None): F[(List[Tool], Option[String])]

  /** List all tools, automatically following pagination. */
  def listAllTools: F[List[Tool]]

  /** Call a tool with the given arguments.
    *
    * Requests are cancellable via fiber cancellation - the library automatically sends a
    * cancellation notification to the server when the fiber is cancelled.
    */
  def callTool[A: Encoder](name: ToolName, arguments: A): F[ToolResult]

  /** Call a tool with string name (convenience overload) */
  @targetName("callToolString")
  def callTool[A: Encoder](name: String, arguments: A): F[ToolResult]

  /** Call a tool with progress reporting.
    *
    * The server sends `notifications/progress` during tool execution. Each progress notification is
    * routed to the `onProgress` callback with the progress value and optional total. Unlike the
    * TypeScript SDK (which only supports one global notification handler per type), per-request
    * callbacks never conflict with other notification handlers.
    *
    * @param name
    *   Tool name
    * @param arguments
    *   Tool arguments
    * @param onProgress
    *   Called for each progress notification from the server
    */
  def callTool[A: Encoder](
      name: ToolName,
      arguments: A,
      onProgress: ProgressParams => F[Unit]
  ): F[ToolResult]

  /** Call a tool with progress reporting (string name convenience overload) */
  @targetName("callToolStringWithProgress")
  def callTool[A: Encoder](
      name: String,
      arguments: A,
      onProgress: ProgressParams => F[Unit]
  ): F[ToolResult]

  /** Call a tool only if the server supports tools capability. Returns None if tools are not
    * supported.
    */
  def callToolIfSupported[A: Encoder](name: ToolName, arguments: A): F[Option[ToolResult]]

  // === Resource Operations ===

  /** List available resources from the server.
    *
    * @param cursor
    *   Optional pagination cursor from a previous response's `nextCursor`
    */
  def listResources(cursor: Option[String] = None): F[(List[Resource], Option[String])]

  /** List all resources, automatically following pagination. */
  def listAllResources: F[List[Resource]]

  /** List available resource templates.
    *
    * @param cursor
    *   Optional pagination cursor from a previous response's `nextCursor`
    */
  def listResourceTemplates(
      cursor: Option[String] = None
  ): F[(List[ResourceTemplate], Option[String])]

  /** List all resource templates, automatically following pagination. */
  def listAllResourceTemplates: F[List[ResourceTemplate]]

  /** Read a resource by URI.
    *
    * Requests are cancellable via fiber cancellation - the library automatically sends a
    * cancellation notification to the server when the fiber is cancelled.
    */
  def readResource(uri: ResourceUri): F[ResourceContent]

  /** Read a resource with string URI (convenience overload) */
  @targetName("readResourceString")
  def readResource(uri: String): F[ResourceContent]

  /** Read a resource only if the server supports resources capability. Returns None if resources
    * are not supported.
    */
  def readResourceIfSupported(uri: ResourceUri): F[Option[ResourceContent]]

  /** Subscribe to `notifications/resources/updated` for a resource. Updates are delivered to the
    * client's notification handler (see `McpClientBuilder.withNotifications`).
    */
  def subscribeResource(uri: String): F[Unit]

  /** Cancel a resource subscription. */
  def unsubscribeResource(uri: String): F[Unit]

  // === Prompt Operations ===

  /** List available prompts from the server.
    *
    * @param cursor
    *   Optional pagination cursor from a previous response's `nextCursor`
    */
  def listPrompts(cursor: Option[String] = None): F[(List[Prompt], Option[String])]

  /** List all prompts, automatically following pagination. */
  def listAllPrompts: F[List[Prompt]]

  /** Get a prompt with the given arguments.
    *
    * Requests are cancellable via fiber cancellation - the library automatically sends a
    * cancellation notification to the server when the fiber is cancelled.
    */
  def getPrompt[A: Encoder](name: PromptName, arguments: A): F[GetPromptResult]

  /** Get a prompt with string name (convenience overload) */
  @targetName("getPromptString")
  def getPrompt[A: Encoder](name: String, arguments: A): F[GetPromptResult]

  /** Get a prompt only if the server supports prompts capability. Returns None if prompts are not
    * supported.
    */
  def getPromptIfSupported[A: Encoder](name: PromptName, arguments: A): F[Option[GetPromptResult]]

  // === Utilities ===

  /** Ask the server for argument completions (`completion/complete`). */
  def complete(params: CompleteParams): F[Completion]

  /** Set the minimum level of log messages the server should send (`logging/setLevel`). */
  def setLogLevel(level: LogLevel): F[Unit]

  // === Tasks ===

  /** Invoke a tool as a task-augmented request (SEP-1686). Returns immediately with the task; poll
    * with [[getTask]] and fetch the outcome with [[taskResult]].
    *
    * @param ttl
    *   requested retention of the task on the server, from creation
    */
  def callToolTask[A: Encoder](
      name: String,
      arguments: A,
      ttl: Option[FiniteDuration] = None
  ): F[Task]

  /** Current state of a task (`tasks/get`). */
  def getTask(taskId: String): F[Task]

  /** The result of a tool task (`tasks/result`); the server answers once the task is terminal.
    * Fails with the task's error if it failed or was cancelled.
    */
  def taskResult(taskId: String): F[ToolResult]

  /** Cancel a task (`tasks/cancel`). */
  def cancelTask(taskId: String): F[Task]

  /** One page of the session's tasks (`tasks/list`). */
  def listTasks(cursor: Option[String] = None): F[(List[Task], Option[String])]

  // === Lifecycle ===

  /** Ping the server to check connectivity */
  def ping: F[Unit]

  /** Shutdown the connection */
  def shutdown: F[Unit]

  /** Cancel a pending request by ID.
    *
    * This cancels the local fiber waiting for the response (if any) and sends a cancellation
    * notification to the server. The server may or may not honor the cancellation depending on the
    * request's state.
    *
    * @param requestId
    *   The ID of the request to cancel
    * @param reason
    *   Optional reason for cancellation (for logging/debugging)
    */
  def cancel(requestId: RequestId, reason: Option[String] = None): F[Unit]

object McpConnection:

  /** @param nextId
    *   allocator for fresh request ids — the connection's single id source (shared with the
    *   handshake via [[mcp4s.RequestCorrelator.nextId]] so ids are never reused)
    * @param progressHandlers
    *   per-request progress callbacks; the transport runner routes `notifications/progress` here
    */
  def apply[F[_]: Concurrent](
      initResult: InitializeResult,
      nextId: F[RequestId],
      sendRequest: JsonRpcRequest => F[Json],
      sendNotification: JsonRpcNotification => F[Unit],
      progressHandlers: Ref[F, Map[RequestId, ProgressParams => F[Unit]]],
      tracer: Tracer[F]
  ): F[McpConnection[F]] =
    Ref.of[F, Map[RequestId, Deferred[F, Unit]]](Map.empty).map { inFlightRequests =>
      new Impl[F](
        initResult,
        nextId,
        sendRequest,
        sendNotification,
        inFlightRequests,
        tracer,
        progressHandlers
      )
    }

  /** A connection over an already-established channel with no progress routing (handy for tests and
    * custom transports).
    */
  def apply[F[_]: Concurrent](
      serverInfo: ServerInfo,
      serverCapabilities: ServerCapabilities,
      nextId: F[RequestId],
      sendRequest: JsonRpcRequest => F[Json],
      sendNotification: JsonRpcNotification => F[Unit],
      tracer: Tracer[F]
  ): F[McpConnection[F]] =
    Ref.of[F, Map[RequestId, ProgressParams => F[Unit]]](Map.empty).flatMap { handlers =>
      apply(
        InitializeResult(McpVersion.Current, serverCapabilities, serverInfo),
        nextId,
        sendRequest,
        sendNotification,
        handlers,
        tracer
      )
    }

  private class Impl[F[_]: Concurrent](
      initResult: InitializeResult,
      nextId: F[RequestId],
      sendRequest: JsonRpcRequest => F[Json],
      sendNotification: JsonRpcNotification => F[Unit],
      inFlightRequests: Ref[F, Map[RequestId, Deferred[F, Unit]]],
      tracer: Tracer[F],
      progressHandlers: Ref[F, Map[RequestId, ProgressParams => F[Unit]]]
  ) extends McpConnection[F]:

    val serverInfo: ServerInfo                 = initResult.serverInfo
    val serverCapabilities: ServerCapabilities = initResult.capabilities
    val protocolVersion: String                = initResult.protocolVersion
    val instructions: Option[String]           = initResult.instructions

    private def cancelAndNotify(reqId: RequestId): F[Unit] =
      for
        tokenOpt <- inFlightRequests.get.map(_.get(reqId))
        _        <- tokenOpt.traverse_(_.complete(()).void.handleErrorWith(_ => Concurrent[F].unit))
        _        <- sendNotification(
          JsonRpcNotification(
            McpMethod.Cancelled,
            Some(CancelledParams(reqId, Some("Fiber cancelled")).asJson)
          )
        )
      yield ()

    private def request[A](
        method: String,
        params: Json,
        decode: Json => F[A],
        onProgress: Option[ProgressParams => F[Unit]] = None
    ): F[A] =
      tracer
        .span(s"mcp.client.$method")
        .use: span =>
          for
            reqId       <- nextId
            _           <- span.addAttribute(Attribute("mcp.request_id", reqId.toString))
            cancelToken <- Deferred[F, Unit]
            _           <- inFlightRequests.update(_ + (reqId -> cancelToken))
            // Inject _meta.progressToken when progress callback is provided
            finalParams = onProgress.fold(params): _ =>
              params.deepMerge(Json.obj("_meta" -> Json.obj("progressToken" -> reqId.asJson)))
            _ <- onProgress.traverse_(cb => progressHandlers.update(_ + (reqId -> cb)))
            req = JsonRpcRequest(reqId, method, Some(finalParams))
            result <- Concurrent[F]
              .race(
                cancelToken.get,
                sendRequest(req).flatMap(decode)
              )
              .flatMap {
                case Left(_) =>
                  span.addAttribute(Attribute("mcp.cancelled", true)) *>
                    Concurrent[F].raiseError(McpError.RequestCancelled(reqId))
                case Right(a) => Concurrent[F].pure(a)
              }
              .guarantee(
                inFlightRequests.update(_ - reqId) *>
                  progressHandlers.update(_ - reqId)
              )
              .onCancel(cancelAndNotify(reqId))
              .handleErrorWith: err =>
                span.addAttribute(Attribute("error", true)) *>
                  span.addAttribute(Attribute("error.type", err.getClass.getSimpleName)) *>
                  span.addAttribute(Attribute("error.message", err.getMessage)) *>
                  Concurrent[F].raiseError(err)
          yield result

    private def requestJson(method: String, params: Json = Json.obj()): F[Json] =
      request(method, params, Concurrent[F].pure)

    private def cursorParams(cursor: Option[String]): Json =
      cursor match
        case Some(c) => Json.obj("cursor" -> Json.fromString(c))
        case None    => Json.obj()

    private def paginate[A](fetch: Option[String] => F[(List[A], Option[String])]): F[List[A]] =
      def loop(cursor: Option[String], acc: List[A]): F[List[A]] =
        fetch(cursor).flatMap: (items, nextCursor) =>
          val combined = acc ++ items
          nextCursor match
            case Some(c) => loop(Some(c), combined)
            case None    => Concurrent[F].pure(combined)
      loop(None, Nil)

    def listTools(cursor: Option[String] = None): F[(List[Tool], Option[String])] =
      request(
        McpMethod.ToolsList,
        cursorParams(cursor),
        json =>
          for
            tools <- json.hcursor.get[List[Tool]]("tools").liftTo[F]
            next = json.hcursor.get[String]("nextCursor").toOption
          yield (tools, next)
      )

    def listAllTools: F[List[Tool]] = paginate(listTools)

    def callTool[A: Encoder](name: ToolName, arguments: A): F[ToolResult] =
      request(
        McpMethod.ToolsCall,
        Json.obj("name" -> Json.fromString(name.value), "arguments" -> Encoder[A].apply(arguments)),
        _.as[ToolResult].liftTo[F]
      )

    @targetName("callToolString")
    def callTool[A: Encoder](name: String, arguments: A): F[ToolResult] =
      callTool(ToolName(name), arguments)

    def callTool[A: Encoder](
        name: ToolName,
        arguments: A,
        onProgress: ProgressParams => F[Unit]
    ): F[ToolResult] =
      request(
        McpMethod.ToolsCall,
        Json.obj("name" -> Json.fromString(name.value), "arguments" -> Encoder[A].apply(arguments)),
        _.as[ToolResult].liftTo[F],
        Some(onProgress)
      )

    @targetName("callToolStringWithProgress")
    def callTool[A: Encoder](
        name: String,
        arguments: A,
        onProgress: ProgressParams => F[Unit]
    ): F[ToolResult] =
      callTool(ToolName(name), arguments, onProgress)

    def callToolIfSupported[A: Encoder](name: ToolName, arguments: A): F[Option[ToolResult]] =
      if supportsTools then callTool(name, arguments).map(Some(_))
      else Concurrent[F].pure(None)

    def listResources(cursor: Option[String] = None): F[(List[Resource], Option[String])] =
      request(
        McpMethod.ResourcesList,
        cursorParams(cursor),
        json =>
          for
            resources <- json.hcursor.get[List[Resource]]("resources").liftTo[F]
            next = json.hcursor.get[String]("nextCursor").toOption
          yield (resources, next)
      )

    def listAllResources: F[List[Resource]] = paginate(listResources)

    def listResourceTemplates(
        cursor: Option[String] = None
    ): F[(List[ResourceTemplate], Option[String])] =
      request(
        McpMethod.ResourcesTemplatesList,
        cursorParams(cursor),
        json =>
          for
            templates <- json.hcursor.get[List[ResourceTemplate]]("resourceTemplates").liftTo[F]
            next = json.hcursor.get[String]("nextCursor").toOption
          yield (templates, next)
      )

    def listAllResourceTemplates: F[List[ResourceTemplate]] = paginate(listResourceTemplates)

    def readResource(uri: ResourceUri): F[ResourceContent] =
      request(
        McpMethod.ResourcesRead,
        Json.obj("uri" -> Json.fromString(uri.value)),
        _.hcursor.get[List[ResourceContent]]("contents").liftTo[F].flatMap {
          case head :: _ => Concurrent[F].pure(head)
          case Nil       => Concurrent[F].raiseError(McpError.ResourceNotFound(uri.value))
        }
      )

    @targetName("readResourceString")
    def readResource(uri: String): F[ResourceContent] =
      readResource(ResourceUri(uri))

    def readResourceIfSupported(uri: ResourceUri): F[Option[ResourceContent]] =
      if supportsResources then readResource(uri).map(Some(_))
      else Concurrent[F].pure(None)

    def subscribeResource(uri: String): F[Unit] =
      requestJson(McpMethod.ResourcesSubscribe, Json.obj("uri" -> Json.fromString(uri))).void

    def unsubscribeResource(uri: String): F[Unit] =
      requestJson(McpMethod.ResourcesUnsubscribe, Json.obj("uri" -> Json.fromString(uri))).void

    def listPrompts(cursor: Option[String] = None): F[(List[Prompt], Option[String])] =
      request(
        McpMethod.PromptsList,
        cursorParams(cursor),
        json =>
          for
            prompts <- json.hcursor.get[List[Prompt]]("prompts").liftTo[F]
            next = json.hcursor.get[String]("nextCursor").toOption
          yield (prompts, next)
      )

    def listAllPrompts: F[List[Prompt]] = paginate(listPrompts)

    def getPrompt[A: Encoder](name: PromptName, arguments: A): F[GetPromptResult] =
      request(
        McpMethod.PromptsGet,
        Json.obj("name" -> Json.fromString(name.value), "arguments" -> Encoder[A].apply(arguments)),
        _.as[GetPromptResult].liftTo[F]
      )

    @targetName("getPromptString")
    def getPrompt[A: Encoder](name: String, arguments: A): F[GetPromptResult] =
      getPrompt(PromptName(name), arguments)

    def getPromptIfSupported[A: Encoder](
        name: PromptName,
        arguments: A
    ): F[Option[GetPromptResult]] =
      if supportsPrompts then getPrompt(name, arguments).map(Some(_))
      else Concurrent[F].pure(None)

    def complete(params: CompleteParams): F[Completion] =
      request(
        McpMethod.CompletionComplete,
        params.asJson,
        _.as[CompleteResult].liftTo[F].map(_.completion)
      )

    def setLogLevel(level: LogLevel): F[Unit] =
      requestJson(McpMethod.LoggingSetLevel, Json.obj("level" -> level.asJson)).void

    private def taskParams(taskId: String): Json =
      Json.obj("taskId" -> Json.fromString(taskId))

    def callToolTask[A: Encoder](
        name: String,
        arguments: A,
        ttl: Option[FiniteDuration] = None
    ): F[Task] =
      request(
        McpMethod.ToolsCall,
        Json.obj(
          "name"      -> Json.fromString(name),
          "arguments" -> Encoder[A].apply(arguments),
          "task"      -> TaskMetadata(ttl.map(_.toMillis)).asJson
        ),
        _.as[CreateTaskResult].liftTo[F].map(_.task)
      )

    def getTask(taskId: String): F[Task] =
      request(McpMethod.TasksGet, taskParams(taskId), _.as[Task].liftTo[F])

    def taskResult(taskId: String): F[ToolResult] =
      request(McpMethod.TasksResult, taskParams(taskId), _.as[ToolResult].liftTo[F])

    def cancelTask(taskId: String): F[Task] =
      request(McpMethod.TasksCancel, taskParams(taskId), _.as[Task].liftTo[F])

    def listTasks(cursor: Option[String] = None): F[(List[Task], Option[String])] =
      request(
        McpMethod.TasksList,
        cursorParams(cursor),
        _.as[ListTasksResult].liftTo[F].map(r => (r.tasks, r.nextCursor))
      )

    def ping: F[Unit] =
      requestJson(McpMethod.Ping).void

    def shutdown: F[Unit] =
      requestJson(McpMethod.Shutdown).void

    def cancel(requestId: RequestId, reason: Option[String] = None): F[Unit] =
      for
        tokenOpt <- inFlightRequests.get.map(_.get(requestId))
        _        <- tokenOpt.traverse_(_.complete(()).void.handleErrorWith(_ => Concurrent[F].unit))
        _        <- sendNotification(
          JsonRpcNotification(
            McpMethod.Cancelled,
            Some(CancelledParams(requestId, reason).asJson)
          )
        )
      yield ()
