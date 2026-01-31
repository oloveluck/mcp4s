package mcp4s.server.testing

import cats.effect.{Async, Resource}
import io.circe.{Encoder, Json}
import io.circe.syntax.*
import mcp4s.protocol.*
import mcp4s.server.McpServer

/** Test client for MCP servers.
  *
  * Provides a simple interface for testing server behavior without network overhead.
  *
  * {{{
  * test("add tool works") {
  *   McpServerTest(myServer).use { client =>
  *     for
  *       result <- client.callTool("add", AddArgs(2, 3))
  *       _ = assertEquals(result.textContent, "5.0")
  *     yield ()
  *   }
  * }
  * }}}
  */
trait McpServerTest[F[_]]:
  /** Server info */
  def serverInfo: ServerInfo

  /** Server capabilities */
  def serverCapabilities: ServerCapabilities

  /** List available tools */
  def listTools: F[List[Tool]]

  /** Call a tool with typed arguments.
    *
    * @param name Tool name
    * @param arguments Arguments (must have Encoder instance)
    */
  def callTool[A: Encoder](name: String, arguments: A): F[ToolResult]

  /** Call a tool with JSON arguments */
  def callToolJson(name: String, arguments: Json): F[ToolResult]

  /** List available resources */
  def listResources: F[List[mcp4s.protocol.Resource]]

  /** List resource templates */
  def listResourceTemplates: F[List[ResourceTemplate]]

  /** Read a resource by URI */
  def readResource(uri: String): F[ResourceContent]

  /** List available prompts */
  def listPrompts: F[List[Prompt]]

  /** Get a prompt with typed arguments */
  def getPrompt[A: Encoder](name: String, arguments: A): F[GetPromptResult]

  /** Get a prompt with string arguments */
  def getPromptMap(name: String, arguments: Map[String, String]): F[GetPromptResult]

object McpServerTest:

  /** Create a test client for an MCP server.
    *
    * The Resource ensures proper cleanup but for most servers no cleanup is needed.
    */
  def apply[F[_]: Async](server: McpServer[F]): Resource[F, McpServerTest[F]] =
    Resource.pure(new McpServerTestImpl(server))

  /** Create a test client directly (no Resource wrapper needed for most cases) */
  def sync[F[_]: Async](server: McpServer[F]): McpServerTest[F] =
    new McpServerTestImpl(server)

  private class McpServerTestImpl[F[_]: Async](server: McpServer[F]) extends McpServerTest[F]:
    def serverInfo: ServerInfo = server.info
    def serverCapabilities: ServerCapabilities = server.capabilities

    def listTools: F[List[Tool]] = server.listTools

    def callTool[A: Encoder](name: String, arguments: A): F[ToolResult] =
      server.callTool(name, arguments.asJson)

    def callToolJson(name: String, arguments: Json): F[ToolResult] =
      server.callTool(name, arguments)

    def listResources: F[List[mcp4s.protocol.Resource]] = server.listResources
    def listResourceTemplates: F[List[ResourceTemplate]] = server.listResourceTemplates

    def readResource(uri: String): F[ResourceContent] =
      server.readResource(uri)

    def listPrompts: F[List[Prompt]] = server.listPrompts

    def getPrompt[A: Encoder](name: String, arguments: A): F[GetPromptResult] =
      // Convert typed args to Map[String, String] via JSON
      val json = arguments.asJson
      val map = json.asObject.map { obj =>
        obj.toMap.collect { case (k, v) if v.isString => k -> v.asString.get }
      }.getOrElse(Map.empty)
      server.getPrompt(name, map)

    def getPromptMap(name: String, arguments: Map[String, String]): F[GetPromptResult] =
      server.getPrompt(name, arguments)
