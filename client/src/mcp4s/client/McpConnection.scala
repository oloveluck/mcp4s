package mcp4s.client

import scala.annotation.targetName
import io.circe.Encoder
import mcp4s.protocol.*
import mcp4s.protocol.ToolName
import mcp4s.protocol.ResourceUri
import mcp4s.protocol.PromptName

/** Represents an active connection to an MCP server.
  *
  * Provides methods to invoke server capabilities:
  *   - List and call tools
  *   - List and read resources
  *   - List and get prompts
  *
  * Connections are created via transport-specific methods like
  * [[mcp4s.transport.http.HttpClientTransport.connect]].
  */
trait McpConnection[F[_]]:

  /** Server info received during initialization */
  def serverInfo: ServerInfo

  /** Server capabilities received during initialization */
  def serverCapabilities: ServerCapabilities

  // === Capability Checks ===

  /** Check if server supports tools capability */
  def supportsTools: Boolean = serverCapabilities.tools.isDefined

  /** Check if server supports resources capability */
  def supportsResources: Boolean = serverCapabilities.resources.isDefined

  /** Check if server supports prompts capability */
  def supportsPrompts: Boolean = serverCapabilities.prompts.isDefined

  // === Tool Operations ===

  /** List available tools from the server */
  def listTools: F[List[Tool]]

  /** Call a tool with the given arguments.
    *
    * Requests are cancellable via fiber cancellation - the library automatically
    * sends a cancellation notification to the server when the fiber is cancelled.
    */
  def callTool[A: Encoder](name: ToolName, arguments: A): F[ToolResult]

  /** Call a tool with string name (convenience overload) */
  @targetName("callToolString")
  def callTool[A: Encoder](name: String, arguments: A): F[ToolResult]

  /** Call a tool only if the server supports tools capability.
    * Returns None if tools are not supported.
    */
  def callToolIfSupported[A: Encoder](name: ToolName, arguments: A): F[Option[ToolResult]]

  // === Resource Operations ===

  /** List available resources from the server */
  def listResources: F[List[Resource]]

  /** List available resource templates */
  def listResourceTemplates: F[List[ResourceTemplate]]

  /** Read a resource by URI.
    *
    * Requests are cancellable via fiber cancellation - the library automatically
    * sends a cancellation notification to the server when the fiber is cancelled.
    */
  def readResource(uri: ResourceUri): F[ResourceContent]

  /** Read a resource with string URI (convenience overload) */
  @targetName("readResourceString")
  def readResource(uri: String): F[ResourceContent]

  /** Read a resource only if the server supports resources capability.
    * Returns None if resources are not supported.
    */
  def readResourceIfSupported(uri: ResourceUri): F[Option[ResourceContent]]

  // === Prompt Operations ===

  /** List available prompts from the server */
  def listPrompts: F[List[Prompt]]

  /** Get a prompt with the given arguments.
    *
    * Requests are cancellable via fiber cancellation - the library automatically
    * sends a cancellation notification to the server when the fiber is cancelled.
    */
  def getPrompt[A: Encoder](name: PromptName, arguments: A): F[GetPromptResult]

  /** Get a prompt with string name (convenience overload) */
  @targetName("getPromptString")
  def getPrompt[A: Encoder](name: String, arguments: A): F[GetPromptResult]

  /** Get a prompt only if the server supports prompts capability.
    * Returns None if prompts are not supported.
    */
  def getPromptIfSupported[A: Encoder](name: PromptName, arguments: A): F[Option[GetPromptResult]]

  // === Lifecycle ===

  /** Ping the server to check connectivity */
  def ping: F[Unit]

  /** Shutdown the connection */
  def shutdown: F[Unit]

  /** Cancel a pending request by ID.
    *
    * This cancels the local fiber waiting for the response (if any) and sends
    * a cancellation notification to the server. The server may or may not
    * honor the cancellation depending on the request's state.
    *
    * @param requestId The ID of the request to cancel
    * @param reason Optional reason for cancellation (for logging/debugging)
    */
  def cancel(requestId: RequestId, reason: Option[String] = None): F[Unit]
