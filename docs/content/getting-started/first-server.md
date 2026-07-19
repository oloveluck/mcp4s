# Build Your First Server

This walkthrough builds a calculator server with tools, resources, and prompts — the three core MCP primitives.

## Define Tools

Tools are functions that AI clients can call. Each tool needs a name, a description (so the AI knows when to use it), and typed arguments:

```scala
import cats.effect.IO
import mcp4s.server.dsl.*

@description("Add two numbers")
case class AddArgs(a: Double, b: Double) derives Schema

@description("Multiply two numbers")
case class MultiplyArgs(a: Double, b: Double) derives Schema

val tools =
  Tool.from[AddArgs].handle[IO](args => IO.pure(ok(s"${args.a + args.b}"))) |+|
    Tool.from[MultiplyArgs].handle[IO](args => IO.pure(ok(s"${args.a * args.b}")))
```

`Tool.from[AddArgs]` derives the tool name from the class name (`AddArgs` → `"add"`, `MultiplyArgs` → `"multiply"`) and the description from the class-level `@description` annotation. For explicit control, spell it out:

```scala
Tool("add").withDescription("Add two numbers").input[AddArgs].handle[IO] { args =>
  IO.pure(ok(s"${args.a + args.b}"))
}
```

## Add Resources

Resources expose data that AI clients can read. They're addressed by URI:

```scala
import mcp4s.server.dsl.Resource   // the DSL's resource constructors, not cats.effect.Resource

val resources =
  Resource.text[IO]("file:///readme", "README")("Calculator Server v1.0") |+|
    Resource.template[IO]("api://users/{id}", "User", "Get user by ID")(uri =>
      IO.pure(text(uri, s"""{"id":"${uri.split("/").last}"}"""))
    )
```

## Define Prompts

Prompts are reusable message templates. Clients inject them into the AI's conversation:

```scala
case class GreetArgs(name: String) derives Schema

val prompts =
  Prompt("help").withDescription("Get help").messages[IO](user("How do I use this?")) |+|
    Prompt("greet").withDescription("Greet someone").input[GreetArgs].handle[IO](args =>
      IO.pure(messages(user(s"Hello, ${args.name}!")))
    )
```

## Build and Run

```scala
import mcp4s.protocol.ServerInfo
import mcp4s.server.*

val server = McpServer[IO](ServerInfo("calculator", "1.0.0"))
  .withTools(tools)
  .withResources(resources)
  .withPrompts(prompts)

// HTTP (production) — defaults to port 3000, path /mcp
server.http().resource.useForever

// HTTP on a custom port
import mcp4s.server.transport.HttpConfig
import com.comcast.ip4s.*
server.http(HttpConfig(port = port"8080")).resource.useForever

// Stdio (Claude Desktop)
server.stdio.run

// WebSocket — defaults to port 3000, path /ws
server.webSocket().resource.useForever
```

The same transport verbs are available on any `Server[F]` value (no import needed), so `Server.from(info, tools, resources, prompts)` — the low-level constructor — binds the same way.

Capabilities are derived from what you register — a server with no resources won't advertise the `resources` capability. See the [Server Guide](../server/README.md) for the full construction and composition story.

## Test with MCP Inspector

The MCP Inspector lets you interactively test your server:

```bash
npx @modelcontextprotocol/inspector --transport http --server-url http://localhost:3000/mcp
```

---
**Next:** [Build Your First Client](first-client.md)
