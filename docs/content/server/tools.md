# Tools

Tools are the most important MCP primitive. A **tool** is a function that an AI model can call — search a database, send an email, create a file, run a calculation. When you register a tool on a server, AI clients can discover it, see its parameter schema, and invoke it.

> For the full protocol details, see [Tools](https://spec.modelcontextprotocol.io/specification/2025-03-26/server/tools/) in the MCP specification.

Tools have a **name**, a **description** (so the AI knows when to use it), and a typed **input schema** (so the AI knows what arguments to pass). The description matters — it's the primary way the AI decides whether to call your tool.

## Type-Safe Arguments

Define tool inputs as case classes. The `derives ToolInput` macro generates a JSON schema and decoder automatically:

```scala
import mcp4s.protocol.*

@description("Search documents")
case class SearchArgs(
  query: String,
  @description("Max results") limit: Option[Int]
) derives ToolInput
```

The class-level `@description` becomes the tool description. Field-level `@description` annotations add documentation to the schema — this helps the AI understand what each parameter does. `Option` fields become optional in the schema.

## Constructors

### Derived name + description (recommended)

The tool name is derived from the class name (e.g. `SearchArgs` → `"search"`) and the description from the class-level `@description`:

```scala
import mcp4s.server.mcp.*

// Effectful — name and description derived from SearchArgs
Tool[IO, SearchArgs] { args => IO.pure(ok("result")) }

// Pure text result
Tool.text[IO, SearchArgs] { args => "result" }

// With context (sampling, progress, logging)
Tool.withContext[IO, SearchArgs] { (args, ctx) =>
  for
    _ <- ctx.log(LogLevel.Info, "Processing")
    _ <- ctx.progress(0.5, Some(100))
    response <- ctx.sampling.createMessage(params)
  yield ok(response.content.toString)
}
```

### Explicit name + description

Use explicit names when the derived name doesn't match what you need:

```scala
// Full explicit
Tool[IO, Args]("name", "desc") { args => IO.pure(ok("result")) }

// Custom name, description derived from @description
Tool[IO, Args]("custom-name") { args => IO.pure(ok("result")) }

// No arguments
Tool.text[IO]("ping", "Ping") { "pong" }

// With context, explicit name
Tool.withContext[IO, Args]("smart", "AI tool") { (args, ctx) => ... }
```

The **context** variant gives your tool access to the MCP session: it can report progress to the client, write logs, and even request LLM completions from the client via **sampling** (a bidirectional MCP feature where the server asks the client's AI model for help).

## Streaming Tools

Tools can stream results incrementally using `fs2.Stream`:

```scala
Tool.streaming[IO, QueryArgs]("search", "Search documents") { args =>
  database.search(args.query).map(r => ok(r.toString))
}

// No arguments
Tool.streaming[IO]("events", "Stream events") {
  eventSource.subscribe.map(e => ok(e.toString))
}
```

Streaming tools require `Concurrent[F]`. The client receives results as they're produced via SSE or WebSocket.

### More Streaming Constructors

```scala
Tool.streaming[IO, Args]("search", "Search") { args =>
  database.search(args.query).map(r => ok(r.toString))
}

Tool.streamingWithContext[IO, Args]("smart-search", "Search with progress") { (args, ctx) =>
  database.search(args.query).evalTap(_ => ctx.progress(0.5, None))
    .map(r => ok(r.toString))
}

Tool.streaming[IO]("feed", "Live feed") {
  liveFeed.subscribe.map(e => ok(e.toString))
}
```

## Typed Output

Tools can declare structured output schemas using `ToolOutput`:

```scala
import mcp4s.protocol.ToolOutput
import io.circe.Encoder

case class CalcResult(
  @description("The computed value") result: Double,
  @description("Operation performed") operation: String
) derives ToolOutput, Encoder.AsObject

Tool.typed[IO, CalcArgs, CalcResult]("calculate", "Calculate") { args =>
  IO.pure(CalcResult(args.a + args.b, "add"))
}
```

The output schema is included in the tool definition, letting clients know the shape of the response. See [Type Derivation](../reference/type-derivation.md) for details.

## Results

```scala
ok("success")                    // Text result
error("failed")                  // Error result
content(textContent("a"), textContent("b"))  // Multiple items
```

## Composition

Tools compose with the `|+|` operator. This is the standard way to build up a tool set:

```scala
val tools = addTool |+| multiplyTool |+| divideTool
```

---
**Next:** [Resources](resources.md)
