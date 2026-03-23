# Bidirectional Communication

Server-initiated requests to client. Requires HTTP (SSE) or WebSocket transport.

## Sampling

Server requests LLM completions from client.

**Client:**
```scala
val client = McpClient.builder[IO]
  .withSamplingHandler { params =>
    myLlm.complete(params.messages, params.maxTokens).map(r => message(r.text, r.model))
  }
  .build
```

**Server:**
```scala
Tool.withContext[IO, Args]("smart", "AI tool") { (args, ctx) =>
  ctx.sampling.createMessage(CreateMessageParams(
    messages = List(SamplingMessage(Role.User, SamplingTextContent(args.query))),
    maxTokens = 500
  )).map(r => ok(r.content.toString))
}
```

## Elicitation

Server prompts user for input.

**Client:**
```scala
val client = McpClient.builder[IO]
  .withElicitationHandler { params =>
    askUser(params.message).map(r => if r.confirmed then accept(r.data) else decline)
  }
  .build
```

**Server:**
```scala
Tool.withContext[IO, Args]("delete", "Delete file") { (args, ctx) =>
  ctx.elicitation.elicit(ElicitParams(s"Delete ${args.path}?")).flatMap {
    case ElicitResult.Accepted(_) => deleteFile(args.path).map(_ => ok("Deleted"))
    case _ => IO.pure(ok("Cancelled"))
  }
}
```

## Progress & Logging

```scala
Tool.withContext[IO, Args]("work", "Do work") { (args, ctx) =>
  ctx.log(LogLevel.Info, "Starting") *>
    ctx.progress(0.5, Some(100)) *>
    doWork()
}
```
