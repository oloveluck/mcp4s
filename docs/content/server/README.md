# Server Guide

An MCP server is a program that exposes capabilities — **tools**, **resources**, and **prompts** — to AI clients. When a client connects, it discovers what your server offers and can start using it. You write the logic; MCP handles the protocol.

Servers can run over HTTP, WebSocket, or Stdio (for Claude Desktop integration). A single server can serve multiple clients simultaneously.

### Common Patterns

- **Claude Desktop tool servers** — Expose tools via stdio so Claude Desktop can call them locally
- **HTTP microservices** — Run MCP servers as standalone services that AI agents call over the network
- **Composite servers** — Combine tools from multiple domains into a single server with `<+>`

## Construction

```scala
import cats.effect.*
import mcp4s.server.*
import mcp4s.server.mcp.*

// Declarative
val server = Server.from[IO](
  info = ServerInfo("my-server", "1.0.0"),
  tools = myTools,
  resources = myResources,
  prompts = myPrompts
)

// Builder
val server = Server.builder[IO]
  .withInfo(ServerInfo("my-server", "1.0.0"))
  .tool("ping", "Ping") { IO.pure(ok("pong")) }
  .resource("file:///readme", "README") { "Hello" }
  .prompt("greet", "Greet")(user("Hello!"))
  .build
```

## Composition

Servers compose with `<+>` (left takes precedence on conflicts):

```scala
val combined = calculatorServer <+> utilityServer
```

## Running

```scala
server.serveHttp(3000)           // HTTP on /mcp
server.runStdio                  // Stdio for Claude Desktop
WebSocketTransport.serve[IO](server, WebSocketConfig(port = 3000))
```

## DSL Reference

```scala
import mcp4s.server.mcp.*

// Tools — functions the AI can call
Tool[IO, Args] { args => IO.pure(ok("result")) }                 // derived name + desc
Tool[IO, Args]("name", "desc") { args => IO.pure(ok("result")) } // explicit
Tool.withContext[IO, Args] { (args, ctx) => ... }                 // with context

// Resources — data the AI can read
Resource.text[IO]("uri", "name") { "content" }
Resource.template[IO]("uri/{id}", "name", "desc") { uri => ... }

// Prompts — reusable message templates
Prompt[IO]("name", "desc")(user("Hello"))
Prompt[IO, Args] { args => IO.pure(messages(...)) }  // derived name + desc

// Results
ok("success")
error("failed")
user("text")
assistant("text")
```

## Guide Contents

- [Tools](tools.md) — Callable functions for AI clients
- [Resources](resources.md) — Data exposed via URI
- [Prompts](prompts.md) — Reusable message templates
- [HTTP Security](auth.md) — Securing your server

---
**Next:** [Tools](tools.md)
