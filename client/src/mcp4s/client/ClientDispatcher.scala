package mcp4s.client

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given

/** Dispatcher for handling server-initiated requests to the client.
  *
  * Routes incoming JSON-RPC messages from servers to the appropriate client handlers
  * for roots, sampling, and elicitation.
  */
trait ClientDispatcher[F[_]]:

  /** Process incoming server request and return response */
  def dispatch(message: JsonRpcMessage): F[Option[JsonRpcMessage]]

object ClientDispatcher:

  /** Create a new dispatcher for the given client */
  def apply[F[_]: Concurrent](client: McpClient[F]): F[ClientDispatcher[F]] =
    Concurrent[F].pure(new ClientDispatcherImpl(client))

  private class ClientDispatcherImpl[F[_]: Concurrent](
      client: McpClient[F]
  ) extends ClientDispatcher[F]:

    def dispatch(message: JsonRpcMessage): F[Option[JsonRpcMessage]] =
      message match
        case req: JsonRpcRequest       => handleRequest(req).map(Some(_))
        case notif: JsonRpcNotification => handleNotification(notif).as(None)
        case _                         => Concurrent[F].pure(None)

    private def handleRequest(req: JsonRpcRequest): F[JsonRpcMessage] =
      handleMethod(req.method, req.params.getOrElse(Json.obj()))
        .map(result => JsonRpcResponse(req.id, result))
        .handleError { err =>
          val rpcError = err match
            case e: McpError => McpError.toJsonRpcError(e)
            case e           => JsonRpcError.internalError(e.getMessage)
          JsonRpcErrorResponse(req.id, rpcError)
        }

    private def handleNotification(notif: JsonRpcNotification): F[Unit] =
      notif.method match
        case McpMethod.Cancelled => Concurrent[F].unit
        case McpMethod.Progress  => Concurrent[F].unit
        case _                   => Concurrent[F].unit

    private def handleMethod(method: String, params: Json): F[Json] =
      method match
        case McpMethod.RootsList =>
          client.listRoots.map(_.asJson)

        case McpMethod.SamplingCreateMessage =>
          params.as[CreateMessageParams].liftTo[F].flatMap { p =>
            client.createMessage(p).map(_.asJson)
          }

        case McpMethod.ElicitationCreate =>
          params.as[ElicitParams].liftTo[F].flatMap { p =>
            client.elicit(p).map(_.asJson)
          }

        case McpMethod.Ping =>
          Concurrent[F].pure(Json.obj())

        case other =>
          Concurrent[F].raiseError(McpError.MethodNotFound(other))
