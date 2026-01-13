package mcp4s.client

import scala.annotation.targetName
import cats.effect.{Concurrent, Deferred, Ref}
import cats.effect.syntax.monadCancel.*
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given

/** Implementation of McpConnection that manages JSON-RPC communication.
  *
  * This class handles request/response correlation and JSON encoding/decoding.
  * All requests are cancellable via Cats Effect fiber cancellation - when a fiber
  * is cancelled, a cancellation notification is automatically sent to the server.
  */
class McpConnectionImpl[F[_]: Concurrent](
    val serverInfo: ServerInfo,
    val serverCapabilities: ServerCapabilities,
    sendRequest: JsonRpcRequest => F[Json],
    sendNotification: JsonRpcNotification => F[Unit],
    requestIdGen: Ref[F, Long],
    inFlightRequests: Ref[F, Map[RequestId, Deferred[F, Unit]]]
) extends McpConnection[F]:

  private def nextId: F[RequestId] =
    requestIdGen.getAndUpdate(_ + 1).map(n => RequestId.NumberId(n + 1))

  private def cancelAndNotify(reqId: RequestId): F[Unit] =
    for
      tokenOpt <- inFlightRequests.get.map(_.get(reqId))
      _ <- tokenOpt.traverse_(_.complete(()).void.handleErrorWith(_ => Concurrent[F].unit))
      _ <- sendNotification(JsonRpcNotification(
        McpMethod.Cancelled,
        Some(CancelledParams(reqId, Some("Fiber cancelled")).asJson)
      ))
    yield ()

  private def request[A](method: String, params: Json, decode: Json => F[A]): F[A] =
    for
      reqId <- nextId
      cancelToken <- Deferred[F, Unit]
      _ <- inFlightRequests.update(_ + (reqId -> cancelToken))
      req = JsonRpcRequest(reqId, method, Some(params))
      result <- Concurrent[F].race(
        cancelToken.get,
        sendRequest(req).flatMap(decode)
      ).flatMap {
        case Left(_)  => Concurrent[F].raiseError(McpError.RequestCancelled(reqId))
        case Right(a) => Concurrent[F].pure(a)
      }.guarantee(inFlightRequests.update(_ - reqId))
        .onCancel(cancelAndNotify(reqId))
    yield result

  private def requestJson(method: String, params: Json = Json.obj()): F[Json] =
    request(method, params, Concurrent[F].pure)

  def listTools: F[List[Tool]] =
    request(
      McpMethod.ToolsList,
      Json.obj(),
      _.hcursor.get[List[Tool]]("tools").liftTo[F]
    )

  def callTool[A: Encoder](name: ToolName, arguments: A): F[ToolResult] =
    request(
      McpMethod.ToolsCall,
      Json.obj("name" -> Json.fromString(name.value), "arguments" -> Encoder[A].apply(arguments)),
      _.as[ToolResult].liftTo[F]
    )

  @targetName("callToolString")
  def callTool[A: Encoder](name: String, arguments: A): F[ToolResult] =
    callTool(ToolName(name), arguments)

  def callToolIfSupported[A: Encoder](name: ToolName, arguments: A): F[Option[ToolResult]] =
    if supportsTools then callTool(name, arguments).map(Some(_))
    else Concurrent[F].pure(None)

  def listResources: F[List[Resource]] =
    request(
      McpMethod.ResourcesList,
      Json.obj(),
      _.hcursor.get[List[Resource]]("resources").liftTo[F]
    )

  def listResourceTemplates: F[List[ResourceTemplate]] =
    request(
      McpMethod.ResourcesTemplatesList,
      Json.obj(),
      _.hcursor.get[List[ResourceTemplate]]("resourceTemplates").liftTo[F]
    )

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

  def listPrompts: F[List[Prompt]] =
    request(
      McpMethod.PromptsList,
      Json.obj(),
      _.hcursor.get[List[Prompt]]("prompts").liftTo[F]
    )

  def getPrompt[A: Encoder](name: PromptName, arguments: A): F[GetPromptResult] =
    request(
      McpMethod.PromptsGet,
      Json.obj("name" -> Json.fromString(name.value), "arguments" -> Encoder[A].apply(arguments)),
      _.as[GetPromptResult].liftTo[F]
    )

  @targetName("getPromptString")
  def getPrompt[A: Encoder](name: String, arguments: A): F[GetPromptResult] =
    getPrompt(PromptName(name), arguments)

  def getPromptIfSupported[A: Encoder](name: PromptName, arguments: A): F[Option[GetPromptResult]] =
    if supportsPrompts then getPrompt(name, arguments).map(Some(_))
    else Concurrent[F].pure(None)

  def ping: F[Unit] =
    requestJson(McpMethod.Ping).void

  def shutdown: F[Unit] =
    requestJson(McpMethod.Shutdown).void

  def cancel(requestId: RequestId, reason: Option[String] = None): F[Unit] =
    for
      tokenOpt <- inFlightRequests.get.map(_.get(requestId))
      _ <- tokenOpt.traverse_(_.complete(()).void.handleErrorWith(_ => Concurrent[F].unit))
      _ <- sendNotification(JsonRpcNotification(
        McpMethod.Cancelled,
        Some(CancelledParams(requestId, reason).asJson)
      ))
    yield ()
