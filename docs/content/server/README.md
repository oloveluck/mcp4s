# Server Guide

An MCP server is a program that exposes capabilities — **tools**, **resources**, and **prompts** — to AI clients. When a client connects, it discovers what your server offers and can start using it. You write the logic; MCP handles the protocol.

Servers can run over HTTP, WebSocket, or Stdio (for Claude Desktop integration). A single server can serve multiple clients simultaneously.

## Common Patterns

- **Claude Desktop tool servers** — Expose tools via stdio so Claude Desktop can call them locally
- **HTTP microservices** — Run MCP servers as standalone services that AI agents call over the network
- **Composite servers** — Combine tools from multiple domains into a single server with `|+|`

## Construction

`McpServer` is the one entry point for assembling a server:

```scala
import cats.effect.IO
import mcp4s.server.*
import mcp4s.server.dsl.{Resource, *}
import mcp4s.protocol.ServerInfo

case class EchoArgs(text: String) derives Schema

val myTools = Tool("echo").withDescription("Echo back").input[EchoArgs]
  .handle[IO](args => IO.pure(ok(args.text)))
val myResources = Resource.text[IO]("file:///readme", "README")("Hello")
val myPrompts   = Prompt("help").withDescription("Get help").messages[IO](user("How can I help?"))

val server = McpServer[IO](ServerInfo("my-server", "1.0.0"))
  .withTools(myTools)
  .withResources(myResources)
  .withPrompts(myPrompts)
```

Build `myTools` / `myResources` / `myPrompts` from the DSL (`Tool`, `Resource`, `Prompt`
composed with `|+|`), or use `Tools.single` / `Resources.single` / `Prompts.single` for
raw, untyped handlers. Each `with*` method may be called multiple times; routes combine
first-match-wins.

Capabilities are **derived from what you register**: a tools-only server advertises only tools, and `resources.subscribe` is `true` only if a subscribable resource is registered.

`Server.from(info, tools, resources, prompts)` remains as the low-level constructor when you want a plain `Server[F]` value.

## Composition

Servers compose with `|+|` (left takes precedence on conflicts):

```scala
val calculatorServer = McpServer[IO](ServerInfo("calc", "1.0.0")).withTools(myTools).toServer
val utilityServer    = McpServer[IO](ServerInfo("util", "1.0.0")).withPrompts(myPrompts).toServer

val combined = calculatorServer |+| utilityServer
```

## Running

Bind a transport directly on the builder (or on any `Server[F]` — no import needed):

```scala
val config = HttpConfig[IO](port = port"8080")

server.stdio.run                          // Stdio for Claude Desktop
server.http().resource.useForever         // HTTP on /mcp, port 3000
server.http(config).resource              // HTTP on a custom port
server.http(config).routes                // embed in an existing http4s app
server.webSocket().resource.useForever    // WebSocket on /ws, port 3000
```

`HttpConfig` / `WebSocketConfig` live in `mcp4s.server.transport`. For custom http4s
routes/middleware (CORS, auth, embedding in an existing app), use `server.http(config).routes` —
see [HTTP Security](auth.md).

## DSL Reference

```scala
import mcp4s.server.dsl.*

@description("Summarize text")
case class SummarizeArgs(text: String) derives Schema

// Tools — an endpoint definition plus exactly one handler
Tool.from[SummarizeArgs]
  .handle[IO](args => IO.pure(ok("result")))                    // derived name + desc
Tool("name").withDescription("desc").input[SummarizeArgs]
  .handle[IO](args => IO.pure(ok("result")))                    // explicit
Tool("name").input[SummarizeArgs]
  .handleWith[IO]((args, ctx) => IO.pure(ok("result")))         // with context
Tool("name").input[SummarizeArgs]
  .stream[IO](args => fs2.Stream(ok("chunk")))                  // streaming

// Resources — data the AI can read
Resource.text[IO]("uri", "name")("content")
Resource.template[IO]("uri/{id}", "name", "desc")(uri => IO.pure(text(uri, "content")))

// Prompts — reusable message templates
Prompt("name").withDescription("desc").messages[IO](user("Hello"))
Prompt.from[SummarizeArgs]
  .handle[IO](args => IO.pure(messages(user("Hello"))))         // derived name + desc

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
- [Services](services.md) — Define endpoints once, share them between server and client
- [HTTP Security](auth.md) — Securing your server

---
**Next:** [Tools](tools.md)
