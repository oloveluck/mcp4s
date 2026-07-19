# Bidirectional Communication

Most protocols are one-directional: clients request, servers respond. MCP is different — servers can also make requests *back* to the client. This enables two powerful features:

- **Sampling** — The server asks the client's AI model to generate a completion
- **Elicitation** — The server asks the user for input (confirmation, choices, free text)

> For the full protocol details, see [Sampling](https://spec.modelcontextprotocol.io/specification/2025-03-26/client/sampling/) in the MCP specification.

This means a tool can *think* (by requesting an LLM completion) and *ask* (by prompting the user) during execution.

Bidirectional communication works over **both network transports**: on Streamable HTTP, server-initiated requests ride the SSE response stream and the client answers them on the same connection; on WebSocket they use the duplex socket directly. The client answers server-initiated requests on every network transport — the same shared connection runner drives both. Only **stdio** remains plain request/response.

## Sampling

Sampling lets a server request LLM completions from the client. This is useful for "AI-in-the-loop" tools — the server can use the client's AI to analyze data, generate text, or make decisions as part of a tool call.

**Client** — Register a handler that delegates to your LLM:
```scala
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
```

Adding the handler is what advertises the `sampling` capability — nothing else to configure.

**Server** — Request a completion from within a tool, using a `handleWith` (context) handler:
```scala
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
```

## Elicitation

Elicitation lets a server ask the user for input before proceeding. This is essential for destructive actions (confirming a deletion) or when the server needs information it can't infer.

**Client** — Register a handler that prompts the user:
<!-- doc-snippet: reset -->
```scala
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
```

**Server** — Ask the user for confirmation:
```scala
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
```

## Progress & Logging

Servers can also push progress updates and log messages to the client during tool execution:

```scala
def doWork(): IO[ToolResult] = ???

Tool("work").withDescription("Do work").input[Args].handleWith[IO] { (args, ctx) =>
  ctx.log(LogLevel.Info, "Starting") *>
    ctx.progress(0.5, Some(100)) *>
    doWork()
}
```

Streaming tools get the same context via `.streamWith[IO]((args, ctx) => ...)`.
