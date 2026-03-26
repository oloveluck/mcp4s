package mcp4s.examples

import scala.annotation.targetName
import cats.effect.{IO, IOApp, Ref}
import io.circe.{Encoder, Json}
import mcp4s.agent.*
import mcp4s.client.McpConnection
import mcp4s.protocol.*
import mcp4s.server.{mcp => dsl}

/** Demonstrates exposing an agent as an MCP server.
  *
  * - A "status" tool (plain, no agent context)
  * - A "chat" tool using `withAgentTools` that interacts with the agent's LLM
  * - Exercises both tools via direct method calls (no transport)
  *
  * Run with:
  *   mill examples.runMain mcp4s.examples.AgentAsServer
  */
object AgentAsServer extends IOApp.Simple:

  /** Mock LLM that echoes the last user message. */
  private val mockLlm: LlmClient[IO] =
    new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        val lastUser = request.messages.reverse.collectFirst { case Message.User(text) => text }
        IO.pure(LlmResponse.Text(s"Echo: ${lastUser.getOrElse("?")}"))

  /** Mock MCP connection (no tools needed for this demo). */
  private def mockConnection: IO[McpConnection[IO]] =
    Ref.of[IO, Map[RequestId, ProgressParams => IO[Unit]]](Map.empty).map { pHandlers =>
      new McpConnection[IO]:
        def serverInfo: ServerInfo = ServerInfo("mock-server", "0.0.0")
        def serverCapabilities: ServerCapabilities = ServerCapabilities()
        def progressHandlers: Ref[IO, Map[RequestId, ProgressParams => IO[Unit]]] = pHandlers
        def listTools: IO[List[Tool]] = IO.pure(Nil)
        def callTool[A: Encoder](name: ToolName, arguments: A): IO[ToolResult] =
          IO.raiseError(McpError.ToolNotFound(name.value))
        @targetName("callToolString")
        def callTool[A: Encoder](name: String, arguments: A): IO[ToolResult] =
          IO.raiseError(McpError.ToolNotFound(name))
        def callTool[A: Encoder](name: ToolName, arguments: A, onProgress: ProgressParams => IO[Unit]): IO[ToolResult] =
          IO.raiseError(McpError.ToolNotFound(name.value))
        @targetName("callToolStringWithProgress")
        def callTool[A: Encoder](name: String, arguments: A, onProgress: ProgressParams => IO[Unit]): IO[ToolResult] =
          IO.raiseError(McpError.ToolNotFound(name))
        def callToolIfSupported[A: Encoder](name: ToolName, arguments: A): IO[Option[ToolResult]] = IO.pure(None)
        def listResources: IO[List[Resource]] = IO.pure(Nil)
        def listResourceTemplates: IO[List[ResourceTemplate]] = IO.pure(Nil)
        def readResource(uri: ResourceUri): IO[ResourceContent] =
          IO.raiseError(McpError.ResourceNotFound(uri.value))
        @targetName("readResourceString")
        def readResource(uri: String): IO[ResourceContent] =
          IO.raiseError(McpError.ResourceNotFound(uri))
        def readResourceIfSupported(uri: ResourceUri): IO[Option[ResourceContent]] = IO.pure(None)
        def listPrompts: IO[List[Prompt]] = IO.pure(Nil)
        def getPrompt[A: Encoder](name: PromptName, arguments: A): IO[GetPromptResult] =
          IO.raiseError(McpError.PromptNotFound(name.value))
        @targetName("getPromptString")
        def getPrompt[A: Encoder](name: String, arguments: A): IO[GetPromptResult] =
          IO.raiseError(McpError.PromptNotFound(name))
        def getPromptIfSupported[A: Encoder](name: PromptName, arguments: A): IO[Option[GetPromptResult]] = IO.pure(None)
        def ping: IO[Unit] = IO.unit
        def shutdown: IO[Unit] = IO.unit
        def cancel(requestId: RequestId, reason: Option[String]): IO[Unit] = IO.unit
    }

  // A "chat" tool that uses AgentContext to interact with the agent's LLM
  private val chatToolFactory: AgentContext[IO] => mcp4s.server.Tools[IO] = ctx =>
    dsl.Tool[IO]("chat", "Chat with the agent") {
      // For this demo, we use a fixed message; a real tool would parse args
      val message = "Hello!"
      for
        _ <- ctx.messages.update(_ :+ Message.User(message))
        msgs <- ctx.messages.get
        response <- ctx.llmClient.complete(LlmRequest(msgs, Nil, ctx.config))
        text = response match
          case LlmResponse.Text(content, _, _) => content
          case other                            => other.toString
        _ <- ctx.messages.update(_ :+ Message.Assistant(text))
      yield dsl.ok(text)
    }

  def run: IO[Unit] =
    for
      conn <- mockConnection
      agent <- Agent.builder[IO](mockLlm, conn)
        .withToolSchemas(Nil)
        .withServerTools(dsl.Tool.text[IO]("status", "Get agent status") { "running" })
        .withAgentTools(chatToolFactory)
        .build
      server <- agent.toServer

      _ <- IO.println("=== Agent as Server ===")
      _ <- IO.println(s"Server: ${server.info.name} v${server.info.version}")

      // List tools
      tools <- server.listTools
      _ <- IO.println(s"Tools: ${tools.map(_.name).mkString(", ")}")

      // Call the status tool
      statusResult <- server.callTool("status", Json.obj())
      _ <- IO.println(s"status -> ${statusResult.content.head.asInstanceOf[TextContent].text}")

      // Call the chat tool
      chatResult <- server.callTool("chat", Json.obj("message" -> Json.fromString("Hello!")))
      _ <- IO.println(s"chat -> ${chatResult.content.head.asInstanceOf[TextContent].text}")

      _ <- IO.println("=== Done ===")
    yield ()
