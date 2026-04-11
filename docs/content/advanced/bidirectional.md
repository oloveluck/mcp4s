# Bidirectional Communication

Most protocols are one-directional: clients request, servers respond. MCP is different — servers can also make requests *back* to the client. This enables two powerful features:

- **Sampling** — The server asks the client's AI model to generate a completion
- **Elicitation** — The server asks the user for input (confirmation, choices, free text)

> For the full protocol details, see [Sampling](https://spec.modelcontextprotocol.io/specification/2025-03-26/client/sampling/) in the MCP specification.

This means a tool can *think* (by requesting an LLM completion) and *ask* (by prompting the user) during execution. Bidirectional communication requires HTTP (SSE) or WebSocket transport.

## Sampling

Sampling lets a server request LLM completions from the client. This is useful for "AI-in-the-loop" tools — the server can use the client's AI to analyze data, generate text, or make decisions as part of a tool call.

**Client** — Register a handler that delegates to your LLM:
```scala
val client = McpClient.builder[IO]
  .withSamplingHandler { params =>
    myLlm.complete(params.messages, params.maxTokens).map(r => message(r.text, r.model))
  }
  .build
```

**Server** — Request a completion from within a tool:
```scala
Tool.withContext[IO, Args]("smart", "AI tool") { (args, ctx) =>
  ctx.sampling.createMessage(CreateMessageParams(
    messages = List(SamplingMessage(Role.User, SamplingTextContent(args.query))),
    maxTokens = 500
  )).map(r => ok(r.content.toString))
}
```

## Elicitation

Elicitation lets a server ask the user for input before proceeding. This is essential for destructive actions (confirming a deletion) or when the server needs information it can't infer.

**Client** — Register a handler that prompts the user:
```scala
val client = McpClient.builder[IO]
  .withElicitationHandler { params =>
    askUser(params.message).map(r => if r.confirmed then accept(r.data) else decline)
  }
  .build
```

**Server** — Ask the user for confirmation:
```scala
Tool.withContext[IO, Args]("delete", "Delete file") { (args, ctx) =>
  ctx.elicitation.elicit(ElicitParams(s"Delete ${args.path}?")).flatMap {
    case ElicitResult.Accepted(_) => deleteFile(args.path).map(_ => ok("Deleted"))
    case _ => IO.pure(ok("Cancelled"))
  }
}
```

## Progress & Logging

Servers can also push progress updates and log messages to the client during tool execution:

```scala
Tool.withContext[IO, Args]("work", "Do work") { (args, ctx) =>
  ctx.log(LogLevel.Info, "Starting") *>
    ctx.progress(0.5, Some(100)) *>
    doWork()
}
```
