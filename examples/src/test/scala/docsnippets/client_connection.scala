// GENERATED from client/connection.md — do not edit; regenerate with snippet_harness.py
package docsnippets.client_connection

import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import io.circe.Json
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.server.{McpServer, Prompts, Resources, Server, ServiceRoutes, ToolContext, Tools}
import mcp4s.server.transport.{HttpConfig, SessionConfig, WebSocketConfig}
import mcp4s.client.{McpClient, McpClientBuilder, McpConnection}
import mcp4s.client.transport.{HttpTransportConfig, McpAuth, StdioTransportConfig, WebSocketTransportConfig}
import mcp4s.transport.Timeouts

object stubs:
  def conn: McpConnection[IO]                  = ???
  def connection: McpConnection[IO]            = ???
  def httpClient: org.http4s.client.Client[IO] = ???
  def server: Server[IO]                       = ???
  def client: McpClient[IO]                    = ???

object scope_1:
  import stubs.{*, given}
  // ---- snippet at line 10
  conn.serverInfo          // ServerInfo
  conn.serverCapabilities  // ServerCapabilities
  conn.supportsTools       // Boolean
  conn.supportsResources   // Boolean
  conn.supportsPrompts     // Boolean

  // ---- snippet at line 22
  val args = Json.obj("query" -> Json.fromString("scala"))

  conn.listAllTools                        // IO[List[Tool]] (follows pagination)
  conn.listTools(cursor = None)            // IO[(List[Tool], Option[String])] one page
  conn.callTool("name", args)              // IO[ToolResult]
  conn.callToolIfSupported(ToolName("name"), args)  // IO[Option[ToolResult]]

  // ---- snippet at line 35
  import mcp4s.client.TypedClient.*
  import mcp4s.schema.{Prompt as PromptDef, Schema, Tool as ToolDef}

  case class AddArgs(a: Double, b: Double) derives Schema
  case class AddResult(sum: Double) derives Schema
  case class GreetArgs(name: String) derives Schema

  val add      = ToolDef("add").input[AddArgs].output[AddResult]
  val greeting = PromptDef("greeting").input[GreetArgs]

  conn.call(add)(AddArgs(1, 2))              // IO[AddResult]
  conn.getPrompt(greeting)(GreetArgs("Ada")) // IO[GetPromptResult]

  // ---- snippet at line 56
  conn.listAllResources                // IO[List[Resource]]
  conn.listAllResourceTemplates        // IO[List[ResourceTemplate]]
  conn.readResource("uri")             // IO[ResourceContent]
  conn.readResourceIfSupported(ResourceUri("uri"))  // IO[Option[ResourceContent]]

  // ---- snippet at line 67
  val promptArgs = Map("name" -> "Ada")

  conn.listAllPrompts                     // IO[List[Prompt]]
  conn.getPrompt("name", promptArgs)      // IO[GetPromptResult]
  conn.getPromptIfSupported(PromptName("name"), promptArgs) // IO[Option[GetPromptResult]]

  // ---- snippet at line 80
  conn.callTool("index", args, p => IO.println(s"${p.progress}/${p.total.getOrElse("?")}"))

  // ---- snippet at line 86
  conn.ping                             // IO[Unit]
  conn.shutdown                         // IO[Unit]
  conn.cancel(RequestId.NumberId(42))   // IO[Unit]

