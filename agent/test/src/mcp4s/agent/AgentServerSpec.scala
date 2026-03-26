package mcp4s.agent

import scala.annotation.targetName
import cats.effect.{IO, Ref}
import io.circe.{Encoder, Json}
import mcp4s.client.McpConnection
import mcp4s.protocol.*
import mcp4s.server.Tools
import mcp4s.server.{mcp => dsl}
import munit.CatsEffectSuite

class AgentServerSpec extends CatsEffectSuite:

  private val dummyTool = Tool(name = "echo", description = Some("Echo"), inputSchema = JsonSchema.empty)
  private val dummyTools = List(dummyTool)

  private def mockConnection(handler: (String, Json) => IO[ToolResult]): IO[McpConnection[IO]] =
    Ref.of[IO, Map[RequestId, ProgressParams => IO[Unit]]](Map.empty).map { pHandlers =>
      new McpConnection[IO]:
        def serverInfo: ServerInfo = ServerInfo("mock", "0.0.0")
        def serverCapabilities: ServerCapabilities = ServerCapabilities()
        def progressHandlers: Ref[IO, Map[RequestId, ProgressParams => IO[Unit]]] = pHandlers
        def listTools: IO[List[Tool]] = IO.pure(dummyTools)
        def callTool[A: Encoder](name: ToolName, arguments: A): IO[ToolResult] =
          handler(name.value, Encoder[A].apply(arguments))
        @targetName("callToolString")
        def callTool[A: Encoder](name: String, arguments: A): IO[ToolResult] =
          handler(name, Encoder[A].apply(arguments))
        def callTool[A: Encoder](name: ToolName, arguments: A, onProgress: ProgressParams => IO[Unit]): IO[ToolResult] =
          handler(name.value, Encoder[A].apply(arguments))
        @targetName("callToolStringWithProgress")
        def callTool[A: Encoder](name: String, arguments: A, onProgress: ProgressParams => IO[Unit]): IO[ToolResult] =
          handler(name, Encoder[A].apply(arguments))
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

  private def textOnlyLlm(response: String): LlmClient[IO] =
    new LlmClient[IO]:
      def complete(request: LlmRequest): IO[LlmResponse] =
        IO.pure(LlmResponse.Text(response))

  test("toServer exposes registered tools") {
    val statusTool = dsl.Tool.text[IO]("status", "Get status") { "ok" }
    mockConnection((_, _) => IO.pure(ToolResult.text("unused"))).flatMap { conn =>
      Agent.builder[IO](textOnlyLlm("hi"), conn)
        .withToolSchemas(ToolSchema.fromTools(dummyTools))
        .withServerTools(statusTool)
        .build
        .flatMap(_.toServer)
        .flatMap { server =>
          for
            tools <- server.listTools
            _ = assertEquals(tools.map(_.name), List("status"))
            result <- server.callTool("status", Json.obj())
          yield assertEquals(result.content.head.asInstanceOf[TextContent].text, "ok")
        }
    }
  }

  test("toServer with no registrations returns empty tool list") {
    mockConnection((_, _) => IO.pure(ToolResult.text("unused"))).flatMap { conn =>
      Agent.builder[IO](textOnlyLlm("hi"), conn)
        .withToolSchemas(ToolSchema.fromTools(dummyTools))
        .build
        .flatMap(_.toServer)
        .flatMap { server =>
          server.listTools.map { tools =>
            assertEquals(tools, Nil)
          }
        }
    }
  }

  test("toServer derives ServerInfo from ClientInfo") {
    mockConnection((_, _) => IO.pure(ToolResult.text("unused"))).flatMap { conn =>
      Agent.builder[IO](textOnlyLlm("hi"), conn)
        .withInfo(ClientInfo("my-agent", "2.0.0"))
        .withToolSchemas(ToolSchema.fromTools(dummyTools))
        .build
        .flatMap(_.toServer)
        .map { server =>
          assertEquals(server.info.name, "my-agent")
          assertEquals(server.info.version, "2.0.0")
        }
    }
  }

  test("toServer uses explicit ServerInfo") {
    mockConnection((_, _) => IO.pure(ToolResult.text("unused"))).flatMap { conn =>
      Agent.builder[IO](textOnlyLlm("hi"), conn)
        .withInfo(ClientInfo("agent-name", "1.0.0"))
        .withServerInfo(ServerInfo("custom-server", "3.0.0"))
        .withToolSchemas(ToolSchema.fromTools(dummyTools))
        .build
        .flatMap(_.toServer)
        .map { server =>
          assertEquals(server.info.name, "custom-server")
          assertEquals(server.info.version, "3.0.0")
        }
    }
  }

  test("withAgentTools provides AgentContext to handler") {
    val factory: AgentContext[IO] => Tools[IO] = ctx =>
      dsl.Tool[IO]("model-info", "Get model info") {
        IO.pure(dsl.ok(ctx.config.model.getOrElse("none")))
      }

    mockConnection((_, _) => IO.pure(ToolResult.text("unused"))).flatMap { conn =>
      Agent.builder[IO](textOnlyLlm("hi"), conn)
        .withConfig(LlmConfig(model = Some("gpt-4")))
        .withToolSchemas(ToolSchema.fromTools(dummyTools))
        .withAgentTools(factory)
        .build
        .flatMap(_.toServer)
        .flatMap { server =>
          server.callTool("model-info", Json.obj()).map { result =>
            assertEquals(result.content.head.asInstanceOf[TextContent].text, "gpt-4")
          }
        }
    }
  }

  test("AgentContext.messages allows read/write") {
    val factory: AgentContext[IO] => Tools[IO] = ctx =>
      dsl.Tool[IO]("memo", "Read/write messages") {
        // Always write then read — simpler than parameterized
        ctx.messages.update(_ :+ Message.User("stored-msg")) *>
          ctx.messages.get.map(msgs => dsl.ok(msgs.size.toString))
      }

    mockConnection((_, _) => IO.pure(ToolResult.text("unused"))).flatMap { conn =>
      Agent.builder[IO](textOnlyLlm("hi"), conn)
        .withToolSchemas(ToolSchema.fromTools(dummyTools))
        .withAgentTools(factory)
        .build
        .flatMap(_.toServer)
        .flatMap { server =>
          for
            r1 <- server.callTool("memo", Json.obj())
            _ = assertEquals(r1.content.head.asInstanceOf[TextContent].text, "1")
            r2 <- server.callTool("memo", Json.obj())
          yield assertEquals(r2.content.head.asInstanceOf[TextContent].text, "2")
        }
    }
  }

  test("AgentContext.run executes the agent loop") {
    val llm = textOnlyLlm("Hello from agent")

    val factory: AgentContext[IO] => Tools[IO] = ctx =>
      dsl.Tool[IO]("ask", "Ask agent") {
        ctx.run("hello")
          .collect { case AgentEvent.Finished(content) => content }
          .compile
          .lastOrError
          .map(dsl.ok)
      }

    mockConnection((_, _) => IO.pure(ToolResult.text("unused"))).flatMap { conn =>
      Agent.builder[IO](llm, conn)
        .withToolSchemas(ToolSchema.fromTools(dummyTools))
        .withAgentTools(factory)
        .build
        .flatMap(_.toServer)
        .flatMap { server =>
          server.callTool("ask", Json.obj()).map { result =>
            assertEquals(result.content.head.asInstanceOf[TextContent].text, "Hello from agent")
          }
        }
    }
  }

  test("asTool exposes the agent loop as a callable tool") {
    val llm = textOnlyLlm("Hello!")

    mockConnection((_, _) => IO.pure(ToolResult.text("unused"))).flatMap { conn =>
      Agent.builder[IO](llm, conn)
        .withToolSchemas(ToolSchema.fromTools(dummyTools))
        .asTool("ask", "Ask the agent")
        .build
        .flatMap(_.toServer)
        .flatMap { server =>
          for
            tools <- server.listTools
            _ = assert(tools.exists(_.name == "ask"))
            result <- server.callTool("ask", Json.obj("prompt" -> Json.fromString("hi")))
          yield assertEquals(result.content.head.asInstanceOf[TextContent].text, "Hello!")
        }
    }
  }

  test("withServerTools composes multiple calls") {
    val tool1 = dsl.Tool.text[IO]("ping", "Ping") { "pong" }
    val tool2 = dsl.Tool.text[IO]("status", "Status") { "ok" }

    mockConnection((_, _) => IO.pure(ToolResult.text("unused"))).flatMap { conn =>
      Agent.builder[IO](textOnlyLlm("hi"), conn)
        .withToolSchemas(ToolSchema.fromTools(dummyTools))
        .withServerTools(tool1)
        .withServerTools(tool2)
        .build
        .flatMap(_.toServer)
        .flatMap { server =>
          server.listTools.map { tools =>
            val names = tools.map(_.name).toSet
            assert(names.contains("ping"))
            assert(names.contains("status"))
          }
        }
    }
  }
