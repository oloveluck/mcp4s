// GENERATED from advanced/bidirectional.md — do not edit; regenerate with snippet_harness.py
package docsnippets.advanced_bidirectional

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
  // ---- snippet at line 20
  import cats.effect.IO
  import mcp4s.client.McpClientBuilder
  import mcp4s.client.mcp.*
  import mcp4s.protocol.{ClientInfo, SamplingMessage}

  case class LlmReply(text: String, model: String)
  def myLlm(messages: List[SamplingMessage], maxTokens: Int): IO[LlmReply] = ???   // your LLM

  val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))
    .withSampling(Sampling[IO](params =>
      myLlm(params.messages, params.maxTokens).map(r => message(r.text, r.model))
    ))

  // ---- snippet at line 38
  import mcp4s.server.dsl.*

  case class Args(query: String) derives Schema

  Tool("smart").withDescription("AI tool").input[Args].handleWith[IO] { (args, ctx) =>
    ctx.sampling
      .createMessage(CreateMessageParams(
        messages = List(SamplingMessage(Role.User, SamplingTextContent(args.query))),
        maxTokens = 500
      ))
      .map(r => ok(r.content.toString))
  }

object scope_2:
  import stubs.{*, given}
  // ---- snippet at line 59
  import cats.effect.IO
  import io.circe.Json
  import mcp4s.client.McpClientBuilder
  import mcp4s.client.mcp.*
  import mcp4s.protocol.{ClientInfo, ElicitFormParams, ElicitUrlParams}

  case class Answer(confirmed: Boolean, data: Map[String, Json])
  def askUser(message: String): IO[Answer] = ???   // your UI integration

  val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))
    .withElicitation(Elicitation[IO] {
      case form: ElicitFormParams =>
        askUser(form.message).map(r => if r.confirmed then accept(r.data) else decline)
      case _: ElicitUrlParams => IO.pure(decline)
    })

  // ---- snippet at line 78
  import mcp4s.server.dsl.*

  case class Args(path: String) derives Schema
  case class Confirm(confirmed: Boolean) derives Schema
  def deleteFile(path: String): IO[Unit] = ???

  Tool("delete").withDescription("Delete file").input[Args].handleWith[IO] { (args, ctx) =>
    ctx.elicitation
      .elicit(ElicitFormParams(s"Delete ${args.path}?", Schema[Confirm].jsonSchema))
      .flatMap { result =>
        result.action match
          case ElicitAction.Accept => deleteFile(args.path).as(ok("Deleted"))
          case _                   => IO.pure(ok("Cancelled"))
      }
  }

  // ---- snippet at line 100
  def doWork(): IO[ToolResult] = ???

  Tool("work").withDescription("Do work").input[Args].handleWith[IO] { (args, ctx) =>
    ctx.log(LogLevel.Info, "Starting") *>
      ctx.progress(0.5, Some(100)) *>
      doWork()
  }

