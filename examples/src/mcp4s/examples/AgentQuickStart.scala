package mcp4s.examples

import scala.annotation.targetName
import cats.effect.{IO, IOApp, Ref}
import io.circe.{Encoder, Json}
import mcp4s.agent.*
import mcp4s.client.McpConnection
import mcp4s.protocol.*

/** Minimal agent example: a mock LLM calls an "add" tool once, then finishes.
  *
  * Run with:
  *   mill examples.runMain mcp4s.examples.AgentQuickStart
  */
object AgentQuickStart extends IOApp.Simple:

  /** Mock LLM: first call returns a tool-use, second call returns text. */
  private def mockLlm: IO[LlmClient[IO]] =
    Ref.of[IO, Int](0).map { counter =>
      new LlmClient[IO]:
        def complete(request: LlmRequest): IO[LlmResponse] =
          counter.getAndUpdate(_ + 1).map { n =>
            if n == 0 then
              LlmResponse.ToolUse(
                ToolCall("call-1", "add", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))),
                stopReason = Some("toolUse")
              )
            else
              LlmResponse.Text("The result is 5.", stopReason = Some("endTurn"))
          }
    }

  /** Mock MCP connection with an "add" tool. */
  private def mockConnection: IO[McpConnection[IO]] =
    Ref.of[IO, Map[RequestId, ProgressParams => IO[Unit]]](Map.empty).map { pHandlers =>
      new McpConnection[IO]:
        def serverInfo: ServerInfo = ServerInfo("mock-server", "0.0.0")
        def serverCapabilities: ServerCapabilities = ServerCapabilities()
        def progressHandlers: Ref[IO, Map[RequestId, ProgressParams => IO[Unit]]] = pHandlers
        def listTools: IO[List[Tool]] = IO.pure(List(
          Tool(name = "add", description = Some("Add two numbers"), inputSchema = JsonSchema.empty)
        ))
        def callTool[A: Encoder](name: ToolName, arguments: A): IO[ToolResult] =
          val json = Encoder[A].apply(arguments)
          val a = json.hcursor.get[Double]("a").getOrElse(0.0)
          val b = json.hcursor.get[Double]("b").getOrElse(0.0)
          IO.pure(ToolResult.text(s"${a + b}"))
        @targetName("callToolString")
        def callTool[A: Encoder](name: String, arguments: A): IO[ToolResult] =
          callTool(ToolName(name), arguments)
        def callTool[A: Encoder](name: ToolName, arguments: A, onProgress: ProgressParams => IO[Unit]): IO[ToolResult] =
          callTool(name, arguments)
        @targetName("callToolStringWithProgress")
        def callTool[A: Encoder](name: String, arguments: A, onProgress: ProgressParams => IO[Unit]): IO[ToolResult] =
          callTool(ToolName(name), arguments)
        def callToolIfSupported[A: Encoder](name: ToolName, arguments: A): IO[Option[ToolResult]] =
          callTool(name, arguments).map(Some(_))
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

  def run: IO[Unit] =
    for
      llm  <- mockLlm
      conn <- mockConnection
      agent <- Agent.builder[IO](llm, conn)
        .withConfig(LlmConfig.default.withModel("mock-llm").withMaxTurns(5))
        .build
      _ <- IO.println("=== Agent Quick Start ===")
      _ <- agent.run("What is 2 + 3?")
        .evalTap {
          case AgentEvent.ToolCalled(call)                => IO.println(s"  [ToolCalled] ${call.name}(${call.arguments.noSpaces})")
          case AgentEvent.ToolResultReceived(_, name, json) => IO.println(s"  [ToolResult] $name -> ${json.noSpaces}")
          case AgentEvent.Finished(content)               => IO.println(s"  [Finished] $content")
          case event                                      => IO.println(s"  [Event] $event")
        }
        .compile
        .drain
      _ <- IO.println("=== Done ===")
    yield ()
