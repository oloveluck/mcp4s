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
  Tool[IO, AddArgs](args => IO.pure(ok(s"${args.a + args.b}"))) |+|
    Tool[IO, MultiplyArgs](args => IO.pure(ok(s"${args.a * args.b}")))
```

The tool name is derived from the class name (`AddArgs` → `"add"`, `MultiplyArgs` → `"multiply"`) and the description comes from the class-level `@description` annotation.

## Add Resources

Resources expose data that AI clients can read. They're addressed by URI:

```scala
val resources =
  Resource.text[IO]("file:///readme", "README")("Calculator Server v1.0") |+|
    Resource.template[IO]("api://users/{id}", "User", "Get user by ID")(uri =>
      IO.pure(mcp.text(uri, s"""{"id":"${uri.split("/").last}"}"""))
    )
```

## Define Prompts

Prompts are reusable message templates. Clients inject them into the AI's conversation:

```scala
case class GreetArgs(name: String) derives PromptInput

val prompts =
  Prompt[IO]("help", "Get help")(user("How do I use this?")) |+|
    Prompt[IO, GreetArgs]("greet", "Greet someone")(args =>
      IO.pure(messages(user(s"Hello, ${args.name}!")))
    )
```

## Build and Run

```scala
val server = Server.from[IO](
  info = ServerInfo("calculator", "1.0.0"),
  tools = tools,
  resources = resources,
  prompts = prompts
)

// HTTP (production) — defaults to port 3000, path /mcp
server.serveHttp()

// HTTP on a custom port
server.serveHttp(port"8080")

// Stdio (Claude Desktop)
server.runStdio

// WebSocket — defaults to port 3000, path /ws
server.serveWebSocket()
server.serveWebSocket(port"3001")
```

All of these come from `import mcp4s.server.syntax.*`.

Everything is built from the composable DSL (`import mcp4s.server.mcp.*`): `Tool`,
`Resource`, and `Prompt` values compose with `|+|`, and `Server.from` assembles them.
For raw, untyped handlers use `Tools.single` / `Resources.single` / `Prompts.single`.

## Test with MCP Inspector

The MCP Inspector lets you interactively test your server:

```bash
npx @modelcontextprotocol/inspector --transport http --server-url http://localhost:3000/mcp
```

---
**Next:** [Build Your First Client](first-client.md)
