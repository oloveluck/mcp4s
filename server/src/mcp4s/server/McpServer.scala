package mcp4s.server

import cats.effect.Concurrent
import io.circe.Json
import mcp4s.protocol.*

/** Core MCP server abstraction.
  *
  * Implementors provide handlers for tools, resources, and prompts. The server handles protocol
  * lifecycle, capability negotiation, and request routing.
  */
trait McpServer[F[_]]:

  /** Server information returned during initialization */
  def info: ServerInfo

  /** Server capabilities */
  def capabilities: ServerCapabilities

  // === Tool Methods ===

  /** List available tools */
  def listTools: F[List[Tool]]

  /** Call a tool with the given arguments */
  def callTool(name: String, arguments: Json): F[ToolResult]

  // === Resource Methods ===

  /** List available resources */
  def listResources: F[List[Resource]]

  /** List available resource templates */
  def listResourceTemplates: F[List[ResourceTemplate]]

  /** Read a resource by URI */
  def readResource(uri: String): F[ResourceContent]

  // === Prompt Methods ===

  /** List available prompts */
  def listPrompts: F[List[Prompt]]

  /** Get a prompt with the given arguments */
  def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult]

object McpServer:

  /** Create a new builder for constructing an MCP server */
  def builder[F[_]: Concurrent]: McpServerBuilder[F] =
    McpServerBuilder.empty[F]
