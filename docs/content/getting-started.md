# Getting Started

This page builds a small server with all three MCP primitives, connects a client to it, and runs
both. Everything after this page is about *why* things are shaped the way they are.

## A server

A server is a bag of **tools** (functions the model may call), **resources** (data it may read)
and **prompts** (message templates it may reuse), plus a transport to reach it over.

```scala
import cats.effect.*
import mcp4s.protocol.ServerInfo
import mcp4s.server.*
import mcp4s.server.dsl.{Resource, *}   // the DSL's Resource, not cats.effect.Resource

@description("Add two numbers")
case class AddArgs(a: Double, b: Double) derives Schema

@description("Greet someone")
case class GreetArgs(name: String) derives Schema

object Calculator extends IOApp.Simple:
  val tools =
    Tool.from[AddArgs].handle[IO](args => IO.pure(ok(s"${args.a + args.b}"))) |+|
      Tool("version").withDescription("Server version").handle[IO](_ => IO.pure(ok("1.0.0")))

  val resources =
    Resource.text[IO]("file:///readme", "README")("A calculator.") |+|
      Resource.template[IO]("user://{id}", "User", "Look up a user")(uri =>
        IO.pure(text(uri, s"""{"id":"${uri.split("/").last}"}"""))
      )

  val prompts =
    Prompt.from[GreetArgs].handle[IO](args => IO.pure(messages(user(s"Hello, ${args.name}!"))))

  val server = McpServer[IO](ServerInfo("calculator", "1.0.0"))
    .withTools(tools)
    .withResources(resources)
    .withPrompts(prompts)

  def run = server.http().run
```

What to notice:

- `Tool.from[AddArgs]` takes the tool's name from the class (`AddArgs` becomes `add`) and its
  description from the class-level `@description`. `Tool("version")` spells both out. Either way a
  tool is a *definition* until you attach exactly one handler with `handle`.
- The `description` matters more than anything else you write: it is how the model decides whether
  to call the tool.
- `|+|` combines routes of the same kind. The first route that matches a name wins.
- You never declare capabilities. A server with tools and prompts but no resources advertises
  tools and prompts.

Swap the last line for `server.stdio.run` to speak over stdin/stdout (what Claude Desktop expects)
or `server.webSocket().run` for WebSocket. See [Transports](concepts/transports.md).

## A client

A client is a `Resource`: acquiring it opens the connection and performs the MCP handshake,
releasing it tears the session down.

```scala
import cats.effect.*
import io.circe.Json, io.circe.syntax.*
import mcp4s.client.*
import mcp4s.client.syntax.*   // JVM: http(url) builds an Ember client for you
import mcp4s.protocol.*

object Client extends IOApp.Simple:
  val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))

  def run = client.http("http://localhost:3000/mcp").use: conn =>
    for
      tools  <- conn.listAllTools
      sum    <- conn.callTool("add", Json.obj("a" -> 5.asJson, "b" -> 3.asJson))
      readme <- conn.readResource("file:///readme")
      hello  <- conn.getPrompt("greet", Map("name" -> "Ada"))
      _      <- IO.println(s"${tools.map(_.name)} / ${sum.textContent} / ${readme.text}")
    yield ()
```

The URL is the full MCP endpoint, path included. `conn.serverInfo`, `conn.serverCapabilities` and
`conn.instructions` hold what the server said during the handshake.

If you would rather not write JSON by hand, define the endpoints once in an `McpService` and
call them typed from the client: `conn.call(Calculator.add)(AddArgs(5, 3))` returns the decoded
result. See [Services](guides/server.md#services).

## Run it

Start the server, then either run the client or poke the server with the MCP Inspector:

```bash
npx @modelcontextprotocol/inspector --transport http --server-url http://localhost:3000/mcp
```

## Next

- [Architecture](concepts/architecture.md): how a request travels from the wire to your handler
  and back, and why the pieces are split the way they are.
- [Server guide](guides/server.md) and [Client guide](guides/client.md): the full feature set on
  each side.
