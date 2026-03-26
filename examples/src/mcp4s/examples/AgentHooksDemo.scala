package mcp4s.examples

import scala.annotation.targetName
import cats.effect.{IO, IOApp, Ref}
import io.circe.{Encoder, Json}
import mcp4s.agent.*
import mcp4s.client.McpConnection
import mcp4s.protocol.*

/** Demonstrates composable hooks: chain-of-thought, reflection, and context management.
  *
  * The mock LLM does 3 rounds of tool calls, then finishes. Hooks inject
  * thinking and reflection steps. The event stream shows all event types.
  *
  * Run with:
  *   mill examples.runMain mcp4s.examples.AgentHooksDemo
  */
object AgentHooksDemo extends IOApp.Simple:

  /** Mock LLM: tool calls for 3 rounds, thinking/reflection when no tools. */
  private def mockLlm: IO[LlmClient[IO]] =
    Ref.of[IO, Int](0).map { counter =>
      new LlmClient[IO]:
        def complete(request: LlmRequest): IO[LlmResponse] =
          counter.getAndUpdate(_ + 1).map { n =>
            if request.tools.isEmpty then
              // Thinking or reflection call (no tools provided)
              LlmResponse.Text(s"[internal reasoning step $n]")
            else if n < 6 then
              // Tool-calling rounds (interleaved with hook calls)
              LlmResponse.ToolUse(
                ToolCall(s"call-$n", "echo", Json.obj("msg" -> Json.fromString(s"round-$n")))
              )
            else
              LlmResponse.Text("All done!", stopReason = Some("endTurn"), usage = Some(Usage(Some(200L), Some(100L))))
          }
    }

  /** Mock MCP connection with an "echo" tool. */
  private def mockConnection: IO[McpConnection[IO]] =
    Ref.of[IO, Map[RequestId, ProgressParams => IO[Unit]]](Map.empty).map { pHandlers =>
      new McpConnection[IO]:
        def serverInfo: ServerInfo = ServerInfo("mock-server", "0.0.0")
        def serverCapabilities: ServerCapabilities = ServerCapabilities()
        def progressHandlers: Ref[IO, Map[RequestId, ProgressParams => IO[Unit]]] = pHandlers
        def listTools: IO[List[Tool]] = IO.pure(List(
          Tool(name = "echo", description = Some("Echo back"), inputSchema = JsonSchema.empty)
        ))
        def callTool[A: Encoder](name: ToolName, arguments: A): IO[ToolResult] =
          val json = Encoder[A].apply(arguments)
          val msg = json.hcursor.get[String]("msg").getOrElse("?")
          IO.pure(ToolResult.text(s"echo: $msg"))
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
        .withConfig(LlmConfig.default.withModel("mock-llm").withMaxTurns(10))
        .withChainOfThought(ChainOfThoughtConfig(thinkBeforeEveryTurn = true))
        .withReflection(ReflectionConfig(reflectEveryNTurns = 2))
        .withContextWindow(TokenBudget(4000, 500), _ => ContextPolicy.keepSystemAndRecent)
        .build
      _ <- IO.println("=== Agent Hooks Demo ===")
      _ <- IO.println("Hooks: ChainOfThought + Reflection + ContextManagement")
      _ <- IO.println("")
      _ <- agent.run("Run the echo tool a few times.")
        .evalTap {
          case AgentEvent.Thinking(content)               => IO.println(s"  [Thinking]    $content")
          case AgentEvent.Reflection(content)              => IO.println(s"  [Reflection]  $content")
          case AgentEvent.ToolCalled(call)                 => IO.println(s"  [ToolCalled]  ${call.name}(${call.arguments.noSpaces})")
          case AgentEvent.ToolResultReceived(_, name, json) => IO.println(s"  [ToolResult]  $name -> ${json.noSpaces}")
          case AgentEvent.ContextCompressed(before, after, msgBefore, msgAfter) =>
            IO.println(s"  [Compressed]  ${before.value} -> ${after.value} tokens, $msgBefore -> $msgAfter messages")
          case AgentEvent.Finished(content)                => IO.println(s"  [Finished]    $content")
        }
        .compile
        .drain
      _ <- IO.println("")
      _ <- IO.println("=== Done ===")
    yield ()
