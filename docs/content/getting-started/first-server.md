# Build Your First Server

This walkthrough builds a calculator server with tools, resources, and prompts — the three core MCP primitives.

## Define Tools

Tools are functions that AI clients can call. Each tool needs a name, a description (so the AI knows when to use it), and typed arguments:

```scala
import cats.effect.*
import mcp4s.server.mcp.*
import mcp4s.protocol.*

@description("Add two numbers")
case class AddArgs(a: Double, b: Double) derives ToolInput

@description("Multiply two numbers")
case class MultiplyArgs(a: Double, b: Double) derives ToolInput

val tools =
  Tool[IO, AddArgs] { args =>
    IO.pure(ok(s"${args.a + args.b}"))
  } |+|
  Tool[IO, MultiplyArgs] { args =>
    IO.pure(ok(s"${args.a * args.b}"))
  }
```

The tool name is derived from the class name (`AddArgs` → `"add"`, `MultiplyArgs` → `"multiply"`) and the description comes from the class-level `@description` annotation.

## Add Resources

Resources expose data that AI clients can read. They're addressed by URI:

```scala
val resources =
  Resource.text[IO]("file:///readme", "README") {
    "Calculator Server v1.0"
  } |+|
  Resource.template[IO]("api://users/{id}", "User", "Get user by ID") { uri =>
    val id = uri.split("/").last
    IO.pure(mcp.text(uri, s"""{"id":"$id"}"""))
  }
```

## Define Prompts

Prompts are reusable message templates. Clients inject them into the AI's conversation:

```scala
case class GreetArgs(name: String) derives PromptInput

val prompts =
  Prompt[IO]("help", "Get help")(user("How do I use this?")) |+|
  Prompt[IO, GreetArgs]("greet", "Greet someone") { args =>
    IO.pure(messages(user(s"Hello, ${args.name}!")))
  }
```

## Build and Run

```scala
val server = Server.from[IO](
  info = ServerInfo("calculator", "1.0.0"),
  tools = tools,
  resources = resources,
  prompts = prompts
)

// HTTP (production)
server.serveHttp(3000)

// Stdio (Claude Desktop)
server.runStdio

// WebSocket
WebSocketTransport.serve[IO](server, WebSocketConfig(port = 3000))
```

## Builder Alternative

The builder API is more concise for simple servers:

```scala
val server = Server.builder[IO]
  .withInfo(ServerInfo("calculator", "1.0.0"))
  .tool[AddArgs]("add", "Add") { args => IO.pure(ok(s"${args.a + args.b}")) }
  .resource("file:///readme", "README") { "Calculator v1.0" }
  .prompt("help", "Help")(user("How can I help?"))
  .build
```

## Test with MCP Inspector

The MCP Inspector lets you interactively test your server:

```bash
npx @modelcontextprotocol/inspector --transport http --server-url http://localhost:3000/mcp
```

---
**Next:** [Build Your First Client](first-client.md)
