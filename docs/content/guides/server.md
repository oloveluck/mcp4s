# Server guide

A server is assembled from three kinds of routes and bound to a transport. This page covers each
kind, how routes compose, and the shared-endpoint pattern (services) that makes clients typed.

## Tools

A tool is an endpoint definition plus exactly one handler. The definition carries the name, the
description and the typed input (and optionally output); the handler is one of four shapes.

```scala
import mcp4s.server.dsl.*

@description("Search the index")
case class SearchArgs(
  @description("Free-text query") query: String,
  @description("Maximum number of hits") limit: Int = 10
) derives Schema

case class Hits(items: List[String]) derives Schema

object index:
  def search(q: String, n: Int): IO[List[String]]      = ???
  def stream(q: String): fs2.Stream[IO, String]        = ???

// 1. handle: arguments in, result out
val search = Tool.from[SearchArgs].handle[IO](a => index.search(a.query, a.limit).map(h => ok(h.mkString("\n"))))

// 2. .output[B]: the handler returns B, advertised as outputSchema and sent as structuredContent
val searchTyped = Tool("search_typed").withDescription("Search, structured").input[SearchArgs].output[Hits]
  .handleWith[IO] { (a, ctx) =>                                   // handleWith: plus the ToolContext
    ctx.log(LogLevel.Debug, "searching") *> index.search(a.query, a.limit).map(Hits(_))
  }

// 3. stream / streamWith: emit as you go; the last value is the result on the plain call path
val searchStream = Tool("search_stream").input[SearchArgs].stream[IO](a => index.stream(a.query).map(ok(_)))
```

Guidance that is easy to miss:

- The description is the model's only guide to *when* to call the tool. Field descriptions are
  its guide to *what* to pass. Write both.
- Return `error("…")` (an `isError` result) for failures the model should see and may correct,
  such as bad input or a missing record. Throw for failures that are the server's problem;
  those become JSON-RPC errors.
- `withAnnotations(ToolAnnotations(...))` tells hosts whether a tool is read-only, destructive or
  idempotent. `withTaskSupport` opts a long-running tool into [tasks](../concepts/server-to-client.md#tasks).
- Use `handleWith` only when you need the context; `handle` keeps the handler a plain function.

Tools built from a `Resource` (a connection pool, an HTTP client) use `McpLifecycleTool`, which
yields a `Resource[F, Tools[F]]` that acquires on start and releases on shutdown.

## Resources

Resources are data addressed by URI. A static resource has a fixed URI; a template matches a
URI pattern and reads whatever the client asks for.

```scala
import mcp4s.server.dsl.{Resource, *}   // the DSL's Resource shadows cats.effect.Resource

def loadUser(id: String): IO[String] = ???
def configChanges: fs2.Stream[IO, Unit] = ???

val resources =
  Resource.text[IO]("file:///readme", "README")("Usage notes.") |+|
    Resource.template[IO]("user://{id}", "User", "A user record")(uri => loadUser(uri.split("/").last).map(text(uri, _))) |+|
    Resource.subscribable[IO]("config://app", "Configuration", configChanges)(uri => IO.pure(text(uri, "{}")))
```

A subscribable resource makes the server advertise `resources.subscribe`; clients that subscribe
to its URI receive `notifications/resources/updated` when the change stream emits. The stream is
subscribed once per session, so derive it from an fs2 `Topic` or `SignallingRef`, not a
single-consumer queue.

## Prompts

Prompts are message templates. Static ones are a list of messages; parameterised ones take a
typed input exactly like tools, except that arguments arrive as strings and are parsed by the
schema. Enum-typed arguments get argument completion for free.

```scala
enum Tone derives Schema:
  case Formal, Casual

case class GreetArgs(name: String, tone: Tone = Tone.Casual) derives Schema

val prompts =
  Prompt("help").withDescription("How to use this server").messages[IO](user("How do I use this?")) |+|
    Prompt.from[GreetArgs].withDescription("Greet someone").handle[IO](a => IO.pure(messages(user(s"Hello, ${a.name} (${a.tone})"))))
```

## Assembling and running

```scala
import mcp4s.protocol.ServerInfo
import mcp4s.server.*

val server = McpServer[IO](ServerInfo("search", "1.0.0"))
  .withTools(search |+| searchTyped |+| searchStream)
  .withResources(resources)
  .withPrompts(prompts)
  .withInstructions("Call search before answering questions about the index.")

// server.stdio.run                  stdin/stdout, for hosts that spawn the server
// server.http().run                 Streamable HTTP, :3000/mcp
// server.http(config).routes        raw http4s routes to embed behind your own middleware
// server.webSocket().run            WebSocket, :3000/ws
```

`withInstructions` sets the text the server returns at `initialize` for the host's system
prompt. Capabilities are derived from what you registered. `McpServer(...).toServer` gives the
plain `Server[F]`, which is also what you implement directly if the DSL does not fit.

Servers compose: `searchServer |+| adminServer` serves both sets of routes, the left one winning
on a name clash, with the left server's identity. Tools behind a composed server keep their
context, so sampling and progress work through composition.

## Services

A service declares endpoints once so that the server can be checked for completeness and the
client can call them typed. It is the mcp4s analogue of a smithy4s service.

```scala
import mcp4s.schema.{McpService, Tool as Endpoint}
import mcp4s.protocol.description

case class AddArgs(a: Double, b: Double) derives Schema
case class AddResult(sum: Double) derives Schema

object Calculator extends McpService("calculator", "1.0.0"):
  val add = Endpoint("add").withDescription("Add two numbers").input[AddArgs].output[AddResult]
  def endpoints = List(add)
```

On the server, `ServiceRoutes(Calculator)(Calculator.add.handle[IO](a => IO.pure(AddResult(a.a + a.b))))`
binds handlers and throws at construction if an endpoint is missing, duplicated or undeclared.
On the client, `conn.call(Calculator.add)(AddArgs(1, 2))` returns an `IO[AddResult]`; an
`isError` result becomes a raised `McpError.ToolExecutionError`, so typed calls never hand back
a failure as a value. The wire format stays plain MCP; any other client can still call `add`
with JSON.
