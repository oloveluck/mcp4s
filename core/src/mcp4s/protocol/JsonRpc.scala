package mcp4s.protocol

import io.circe.Json

/** JSON-RPC 2.0 protocol types as per https://www.jsonrpc.org/specification */

/** Request ID can be a string, number, or null */
enum RequestId:
  case StringId(value: String)
  case NumberId(value: Long)
  case NullId

object RequestId:
  def apply(s: String): RequestId = StringId(s)
  def apply(n: Long): RequestId   = NumberId(n)

/** JSON-RPC 2.0 error codes */
object JsonRpcErrorCode:
  val ParseError: Int          = -32700
  val InvalidRequest: Int      = -32600
  val MethodNotFound: Int      = -32601
  val InvalidParams: Int       = -32602
  val InternalError: Int       = -32603
  val ServerErrorStart: Int    = -32099
  val ServerErrorEnd: Int      = -32000

/** JSON-RPC 2.0 error object */
final case class JsonRpcError(
    code: Int,
    message: String,
    data: Option[Json] = None
)

object JsonRpcError:
  def parseError(message: String): JsonRpcError =
    JsonRpcError(JsonRpcErrorCode.ParseError, message)

  def invalidRequest(message: String): JsonRpcError =
    JsonRpcError(JsonRpcErrorCode.InvalidRequest, message)

  def methodNotFound(method: String): JsonRpcError =
    JsonRpcError(JsonRpcErrorCode.MethodNotFound, s"Method not found: $method")

  def invalidParams(message: String): JsonRpcError =
    JsonRpcError(JsonRpcErrorCode.InvalidParams, message)

  def internalError(message: String): JsonRpcError =
    JsonRpcError(JsonRpcErrorCode.InternalError, message)

/** JSON-RPC 2.0 message types */
sealed trait JsonRpcMessage

object JsonRpcMessage:
  val Version: String = "2.0"

/** JSON-RPC 2.0 Request */
final case class JsonRpcRequest(
    id: RequestId,
    method: String,
    params: Option[Json] = None
) extends JsonRpcMessage:
  def jsonrpc: String = JsonRpcMessage.Version

/** JSON-RPC 2.0 Notification (request without id) */
final case class JsonRpcNotification(
    method: String,
    params: Option[Json] = None
) extends JsonRpcMessage:
  def jsonrpc: String = JsonRpcMessage.Version

/** JSON-RPC 2.0 Response (success) */
final case class JsonRpcResponse(
    id: RequestId,
    result: Json
) extends JsonRpcMessage:
  def jsonrpc: String = JsonRpcMessage.Version

/** JSON-RPC 2.0 Error Response */
final case class JsonRpcErrorResponse(
    id: RequestId,
    error: JsonRpcError
) extends JsonRpcMessage:
  def jsonrpc: String = JsonRpcMessage.Version
