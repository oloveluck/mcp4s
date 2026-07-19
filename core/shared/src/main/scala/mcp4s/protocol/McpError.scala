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

package mcp4s.protocol

/** MCP-specific errors for protocol operations.
  *
  * These errors are raised during MCP operations and can be converted to/from JSON-RPC errors via
  * `toJsonRpcError` and `fromJsonRpcError`.
  *
  * The no-argument cases are enum singletons, so each is a single shared instance (with a single
  * shared stack trace). That is fine here: an `McpError` is only ever read via its `message` and
  * converted into a JSON-RPC error for the wire — its stack trace is never consumed.
  */
enum McpError(val message: String) extends Exception(message):
  /** Tool with the given name was not found on the server */
  case ToolNotFound(name: String) extends McpError(s"Tool not found: $name")

  /** Resource with the given URI was not found */
  case ResourceNotFound(uri: String) extends McpError(s"Resource not found: $uri")

  /** Prompt with the given name was not found */
  case PromptNotFound(name: String) extends McpError(s"Prompt not found: $name")

  /** Tool arguments failed validation */
  case InvalidToolArguments(name: String, reason: String)
      extends McpError(s"Invalid arguments for tool '$name': $reason")

  /** Prompt arguments failed validation */
  case InvalidPromptArguments(name: String, reason: String)
      extends McpError(s"Invalid arguments for prompt '$name': $reason")

  /** Client and server protocol versions are incompatible */
  case ProtocolVersionMismatch(requested: String, supported: String)
      extends McpError(s"Protocol version mismatch: requested $requested, supported $supported")

  /** Operation attempted before server initialization */
  case NotInitialized extends McpError("Server not initialized")

  /** Initialization attempted on already-initialized server */
  case AlreadyInitialized extends McpError("Server already initialized")

  /** Generic internal error */
  case InternalError(detail: String) extends McpError(detail)

  /** JSON-RPC method not found */
  case MethodNotFound(method: String) extends McpError(s"Method not found: $method")

  /** Method exists but is not supported by this server */
  case MethodNotSupported(method: String) extends McpError(s"Method not supported: $method")

  /** Request was cancelled by the client */
  case RequestCancelled(requestId: RequestId) extends McpError(s"Request cancelled: $requestId")

  /** Server does not support the required capability */
  case CapabilityNotSupported(capability: String)
      extends McpError(s"Server does not support capability: $capability")

  /** Client does not support sampling capability */
  case SamplingNotSupported extends McpError("Client does not support sampling")

  /** Client does not support elicitation capability */
  case ElicitationNotSupported extends McpError("Client does not support elicitation")

  /** Tool ran but reported an error result (`isError = true`) */
  case ToolExecutionError(name: String, detail: String)
      extends McpError(s"Tool '$name' returned an error: $detail")

object McpError:
  /** Convert a JSON-RPC error to a typed McpError */
  def fromJsonRpcError(error: JsonRpcError): McpError = error.code match
    case JsonRpcErrorCode.MethodNotFound => MethodNotFound(error.message)
    case JsonRpcErrorCode.InvalidParams  => InternalError(error.message)
    case JsonRpcErrorCode.InvalidRequest => InternalError(error.message)
    case -32800                          => InternalError(error.message) // Cancelled
    case _                               => InternalError(error.message)

  def toJsonRpcError(err: McpError): JsonRpcError = err match
    case ToolNotFound(_)               => JsonRpcError.methodNotFound(err.message)
    case ResourceNotFound(_)           => JsonRpcError.invalidParams(err.message)
    case PromptNotFound(_)             => JsonRpcError.invalidParams(err.message)
    case InvalidToolArguments(_, _)    => JsonRpcError.invalidParams(err.message)
    case InvalidPromptArguments(_, _)  => JsonRpcError.invalidParams(err.message)
    case ProtocolVersionMismatch(_, _) => JsonRpcError.invalidRequest(err.message)
    case NotInitialized                => JsonRpcError.invalidRequest(err.message)
    case AlreadyInitialized            => JsonRpcError.invalidRequest(err.message)
    case MethodNotFound(_)             => JsonRpcError.methodNotFound(err.message)
    case MethodNotSupported(_)         => JsonRpcError.methodNotFound(err.message)
    case RequestCancelled(_)           => JsonRpcError(-32800, err.message, None)
    case CapabilityNotSupported(_)     => JsonRpcError.invalidRequest(err.message)
    case SamplingNotSupported          => JsonRpcError.invalidRequest(err.message)
    case ElicitationNotSupported       => JsonRpcError.invalidRequest(err.message)
    case ToolExecutionError(_, _)      => JsonRpcError.internalError(err.message)
    case InternalError(_)              => JsonRpcError.internalError(err.message)
