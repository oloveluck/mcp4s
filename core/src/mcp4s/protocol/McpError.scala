package mcp4s.protocol

/** MCP-specific errors for protocol operations.
  *
  * These errors are raised during MCP operations and can be converted
  * to/from JSON-RPC errors via `toJsonRpcError` and `fromJsonRpcError`.
  */
sealed abstract class McpError(val message: String) extends Exception(message)

object McpError:
  /** Tool with the given name was not found on the server */
  final case class ToolNotFound(name: String)
      extends McpError(s"Tool not found: $name")

  /** Resource with the given URI was not found */
  final case class ResourceNotFound(uri: String)
      extends McpError(s"Resource not found: $uri")

  /** Prompt with the given name was not found */
  final case class PromptNotFound(name: String)
      extends McpError(s"Prompt not found: $name")

  /** Tool arguments failed validation */
  final case class InvalidToolArguments(name: String, reason: String)
      extends McpError(s"Invalid arguments for tool '$name': $reason")

  /** Client and server protocol versions are incompatible */
  final case class ProtocolVersionMismatch(requested: String, supported: String)
      extends McpError(s"Protocol version mismatch: requested $requested, supported $supported")

  /** Operation attempted before server initialization */
  final case class NotInitialized()
      extends McpError("Server not initialized")

  /** Initialization attempted on already-initialized server */
  final case class AlreadyInitialized()
      extends McpError("Server already initialized")

  /** Generic internal error */
  final case class InternalError(override val message: String)
      extends McpError(message)

  /** JSON-RPC method not found */
  final case class MethodNotFound(method: String)
      extends McpError(s"Method not found: $method")

  /** Method exists but is not supported by this server */
  final case class MethodNotSupported(method: String)
      extends McpError(s"Method not supported: $method")

  /** Request was cancelled by the client */
  final case class RequestCancelled(requestId: RequestId)
      extends McpError(s"Request cancelled: $requestId")

  /** Server does not support the required capability */
  final case class CapabilityNotSupported(capability: String)
      extends McpError(s"Server does not support capability: $capability")

  /** Convert a JSON-RPC error to a typed McpError */
  def fromJsonRpcError(error: JsonRpcError): McpError = error.code match
    case JsonRpcErrorCode.MethodNotFound => MethodNotFound(error.message)
    case JsonRpcErrorCode.InvalidParams  => InternalError(error.message)
    case JsonRpcErrorCode.InvalidRequest => InternalError(error.message)
    case -32800 => InternalError(error.message)  // Cancelled
    case _      => InternalError(error.message)

  def toJsonRpcError(err: McpError): JsonRpcError = err match
    case ToolNotFound(_)       => JsonRpcError.methodNotFound(err.message)
    case ResourceNotFound(_)   => JsonRpcError.invalidParams(err.message)
    case PromptNotFound(_)     => JsonRpcError.invalidParams(err.message)
    case InvalidToolArguments(_, _) => JsonRpcError.invalidParams(err.message)
    case ProtocolVersionMismatch(_, _) => JsonRpcError.invalidRequest(err.message)
    case NotInitialized()      => JsonRpcError.invalidRequest(err.message)
    case AlreadyInitialized()  => JsonRpcError.invalidRequest(err.message)
    case MethodNotFound(_)     => JsonRpcError.methodNotFound(err.message)
    case MethodNotSupported(_) => JsonRpcError.methodNotFound(err.message)
    case RequestCancelled(_)   => JsonRpcError(-32800, err.message, None)
    case CapabilityNotSupported(_) => JsonRpcError.invalidRequest(err.message)
    case InternalError(_)      => JsonRpcError.internalError(err.message)
