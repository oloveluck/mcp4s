# Server Guide

## Construction

```scala
import cats.effect.*
import mcp4s.server.*
import mcp4s.server.mcp.*

// Declarative
val server = McpServer.from[IO](
  info = ServerInfo("my-server", "1.0.0"),
  tools = myTools,
  resources = myResources,
  prompts = myPrompts
)

// Builder
val server = McpServer.builder[IO]
  .withInfo(ServerInfo("my-server", "1.0.0"))
  .tool("ping", "Ping") { IO.pure(ok("pong")) }
  .resource("file:///readme", "README") { "Hello" }
  .prompt("greet", "Greet")(user("Hello!"))
  .build
```

## Composition

Servers compose with `<+>` (left takes precedence):

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

// Tools
Tool.text[IO, Args]("name", "desc") { args => "result" }
Tool[IO, Args]("name", "desc") { args => IO.pure(ok("result")) }
Tool.withContext[IO, Args]("name", "desc") { (args, ctx) => ... }

// Resources
Resource.text[IO]("uri", "name") { "content" }
Resource.template[IO]("uri/{id}", "name", "desc") { uri => ... }

// Prompts
Prompt[IO]("name", "desc")(user("Hello"))
Prompt[IO, Args]("name", "desc") { args => IO.pure(messages(...)) }

// Results
ok("success")
error("failed")
user("text")
assistant("text")
```

## Guide Contents

- [Tools](tools.md) — Tool DSL
- [Resources](resources.md) — Resource handlers
- [Prompts](prompts.md) — Prompt definitions
- [Middleware](middleware.md) — Cross-cutting concerns
- [Authentication](auth.md) — Security
