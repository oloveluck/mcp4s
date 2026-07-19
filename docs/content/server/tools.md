# Tools

Tools are the most important MCP primitive. A **tool** is a function that an AI model can call — search a database, send an email, create a file, run a calculation. When you register a tool on a server, AI clients can discover it, see its parameter schema, and invoke it.

> For the full protocol details, see [Tools](https://spec.modelcontextprotocol.io/specification/2025-03-26/server/tools/) in the MCP specification.

Tools have a **name**, a **description** (so the AI knows when to use it), and a typed **input schema** (so the AI knows what arguments to pass). The description matters — it's the primary way the AI decides whether to call your tool.

## Type-Safe Arguments

Define tool inputs as case classes. `derives Schema` generates the JSON schema, encoder, and decoder from a single description of the type:

```scala
import mcp4s.server.dsl.*

@description("Search documents")
case class SearchArgs(
  query: String,
  @description("Max results") limit: Option[Int]
) derives Schema
```

The class-level `@description` becomes the tool description. Field-level `@description` annotations add documentation to the schema — this helps the AI understand what each parameter does. `Option` fields and fields with constructor defaults become optional in the schema. Nested case classes, Scala 3 enums, sealed traits, and `Map[String, V]` are all supported — see [Type Derivation](../reference/type-derivation.md).

## Defining a Tool

A tool is an **endpoint definition** (`ToolEndpoint`) plus exactly one **handler**. Build the endpoint fluently, then attach the handler:

```scala
import mcp4s.server.dsl.*

val search = Tool("search")
  .withDescription("Search the document index")
  .input[SearchArgs]
  .handle[IO] { args =>
    IO.pure(ok(s"Searching for ${args.query}"))
  }
```

### Derived name + description

`Tool.from[Args]` derives the name from the input class (snake_case, with `Args`/`Input`/`Params`/`Request` suffixes stripped) and the description from the class-level `@description`:

```scala
// name = "search", description = "Search documents"
Tool.from[SearchArgs].handle[IO](args => IO.pure(ok("result")))
```

### No-input tools

A `Tool("name")` without `.input[...]` takes `Unit`:

```scala
Tool("version").withDescription("Get server version").handle[IO](_ => IO.pure(ok("1.0.0")))
```

## The Four Handler Shapes

Every tool attaches exactly one of four handlers:

```scala
val endpoint = Tool("search_docs").withDescription("Search the docs").input[SearchArgs]

def params: CreateMessageParams = ???
object database:
  def search(query: String): Stream[IO, String] = ???

// 1. Effectful
endpoint.handle[IO](args => IO.pure(ok("done")))

// 2. Effectful + ToolContext (sampling, elicitation, progress, logging)
endpoint.handleWith[IO] { (args, ctx) =>
  for
    _        <- ctx.log(LogLevel.Info, "Processing")
    _        <- ctx.progress(0.5, Some(100))
    response <- ctx.sampling.createMessage(params)
  yield ok(response.content.toString)
}

// 3. Streaming — on the plain request/response path the last emitted value is the result
endpoint.stream[IO](args => database.search(args.query).map(r => ok(r.toString)))

// 4. Streaming + ToolContext
endpoint.streamWith[IO] { (args, ctx) =>
  database.search(args.query).evalTap(_ => ctx.progress(0.5, None)).map(r => ok(r.toString))
}
```

The **context** variants give your tool access to the MCP session: it can report progress to the client, write logs, request user input (**elicitation**), and even request LLM completions from the client via **sampling** (a bidirectional MCP feature where the server asks the client's AI model for help). See [Bidirectional Communication](../advanced/bidirectional.md).

Streaming handlers require `Concurrent[F]` (all handlers do). The client receives progress and notifications as they're produced via SSE or WebSocket.

## Typed Output

Declare a structured output with `.output[B]`. The output schema is advertised as `outputSchema` and results are encoded as `structuredContent`:

```scala
case class CalcArgs(a: Double, b: Double) derives Schema

case class CalcResult(
  @description("The computed value") result: Double,
  @description("Operation performed") operation: String
) derives Schema

val calculate = Tool("calculate")
  .withDescription("Calculate")
  .input[CalcArgs]
  .output[CalcResult]
  .handle[IO](args => IO.pure(CalcResult(args.a + args.b, "add")))
```

With `.output[B]` the handler returns `B` directly — no manual `ok(...)` wrapping. Primitive outputs (`String`, `Int`, `Double`, ...) are wrapped on the wire as `{"result": ...}`. Without `.output`, handlers return a raw `ToolResult` (built with `ok` / `error` / `content`).

Typed outputs pair with the [typed client](services.md), which decodes `structuredContent` back into `B`.

## Annotations

Attach MCP tool annotations (hints like read-only or destructive) with `.withAnnotations`:

```scala
case class DeleteArgs(path: String) derives Schema

Tool("delete_file")
  .withDescription("Delete a file")
  .input[DeleteArgs]
  .withAnnotations(ToolAnnotations(destructiveHint = Some(true)))
  .handle[IO](args => IO.pure(ok(s"Deleted ${args.path}")))
```

## Results

```scala
ok("success")                    // Text result
error("failed")                  // Error result
content(textContent("a"), textContent("b"))  // Multiple items
```

## Composition

Attaching a handler produces a `Tools[F]` value. Tools compose with the `|+|` operator — this is the standard way to build up a tool set:

```scala
val tools = search |+| calculate

val server = McpServer[IO](ServerInfo("calc", "1.0.0")).withTools(tools)
```

---
**Next:** [Resources](resources.md)
