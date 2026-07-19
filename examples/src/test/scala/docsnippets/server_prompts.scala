// GENERATED from server/prompts.md — do not edit; regenerate with snippet_harness.py
package docsnippets.server_prompts

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
  // ---- snippet at line 14
  import mcp4s.server.dsl.*

  // Static — always returns the same messages
  Prompt("help")
    .withDescription("Get help")
    .messages[IO](
      user("How can I help?"),
      assistant("I can assist with calculations.")
    )

  // Static with a described result
  Prompt("intro")
    .withDescription("Introduction")
    .static[IO](messages("A short intro")(user("Hello!")))

  // With arguments — customizable via parameters
  case class GreetArgs(name: String, excitement: Int = 1) derives Schema

  Prompt("greet")
    .withDescription("Greet someone")
    .input[GreetArgs]
    .handle[IO](args => IO.pure(messages(user(s"Hello, ${args.name}${"!" * args.excitement}"))))

object scope_2:
  import stubs.{*, given}
  // ---- snippet at line 42
  import mcp4s.server.dsl.*

  @description("Greet someone")
  case class GreetArgs(name: String) derives Schema

  Prompt.from[GreetArgs].handle[IO](args => IO.pure(messages(user(s"Hello, ${args.name}!"))))

  // ---- snippet at line 59
  user("text")
  assistant("text")
  messages(user("Hello"), assistant("Hi"))
  messages("Description")(user("Hello"))

  // ---- snippet at line 68
  val helpPrompt     = Prompt("help").messages[IO](user("How can I help?"))
  val greetPrompt    = Prompt.from[GreetArgs].handle[IO](args => IO.pure(messages(user(s"Hi, ${args.name}!"))))
  val tutorialPrompt = Prompt("tutorial").messages[IO](user("Walk me through the basics."))

  val prompts = helpPrompt |+| greetPrompt |+| tutorialPrompt

  // ---- snippet at line 78
  val server = McpServer[IO](ServerInfo("my-server", "1.0.0")).withPrompts(prompts)

