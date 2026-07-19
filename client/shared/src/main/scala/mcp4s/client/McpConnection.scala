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

  /** Progress handler registry shared between connection and transport */
  def progressHandlers: Ref[F, Map[RequestId, ProgressParams => F[Unit]]]

  // === Capability Checks ===

  /** Check if server supports tools capability */
  def supportsTools: Boolean = serverCapabilities.tools.isDefined

  /** Check if server supports resources capability */
  def supportsResources: Boolean = serverCapabilities.resources.isDefined

  /** Check if server supports prompts capability */
  def supportsPrompts: Boolean = serverCapabilities.prompts.isDefined

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

  def apply[F[_]: Concurrent](
      serverInfo: ServerInfo,
      serverCapabilities: ServerCapabilities,
      sendRequest: JsonRpcRequest => F[Json],
      sendNotification: JsonRpcNotification => F[Unit],
      tracer: Tracer[F]
  ): F[McpConnection[F]] =
    for
      requestIdGen     <- Ref.of[F, Long](0L)
      inFlightRequests <- Ref.of[F, Map[RequestId, Deferred[F, Unit]]](Map.empty)
      progressHandlers <- Ref.of[F, Map[RequestId, ProgressParams => F[Unit]]](Map.empty)
    yield new Impl[F](
      serverInfo,
      serverCapabilities,
      sendRequest,
      sendNotification,
      requestIdGen,
      inFlightRequests,
      tracer,
      progressHandlers
    )

  private class Impl[F[_]: Concurrent](
      val serverInfo: ServerInfo,
      val serverCapabilities: ServerCapabilities,
      sendRequest: JsonRpcRequest => F[Json],
      sendNotification: JsonRpcNotification => F[Unit],
      requestIdGen: Ref[F, Long],
      inFlightRequests: Ref[F, Map[RequestId, Deferred[F, Unit]]],
      tracer: Tracer[F],
      val progressHandlers: Ref[F, Map[RequestId, ProgressParams => F[Unit]]]
  ) extends McpConnection[F]:

    private def nextId: F[RequestId] =
      requestIdGen.getAndUpdate(_ + 1).map(n => RequestId.NumberId(n + 1))

    private def cancelAndNotify(reqId: RequestId): F[Unit] =
      for
        tokenOpt <- inFlightRequests.get.map(_.get(reqId))
        _        <- tokenOpt.traverse_(_.complete(()).void.handleErrorWith(_ => Concurrent[F].unit))
        _ <- sendNotification(
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
            finalParams = onProgress match
              case Some(_) =>
                params.deepMerge(Json.obj("_meta" -> Json.obj("progressToken" -> reqId.asJson)))
              case None => params
            _ <- onProgress.traverse_(_ => progressHandlers.update(_ + (reqId -> onProgress.get)))
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

    def ping: F[Unit] =
      requestJson(McpMethod.Ping).void

    def shutdown: F[Unit] =
      requestJson(McpMethod.Shutdown).void

    def cancel(requestId: RequestId, reason: Option[String] = None): F[Unit] =
      for
        tokenOpt <- inFlightRequests.get.map(_.get(requestId))
        _        <- tokenOpt.traverse_(_.complete(()).void.handleErrorWith(_ => Concurrent[F].unit))
        _ <- sendNotification(
          JsonRpcNotification(
            McpMethod.Cancelled,
            Some(CancelledParams(requestId, reason).asJson)
          )
        )
      yield ()
