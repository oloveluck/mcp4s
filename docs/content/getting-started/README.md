# Getting Started

> New to MCP? Read the [MCP introduction](https://modelcontextprotocol.io/introduction) for background on the protocol.

## What You're Building

In MCP, a **server** exposes tools, resources, and prompts. A **client** connects to servers and uses them.

This guide gets you running with both. For deeper walkthroughs, see [Your First Server](first-server.md) and [Your First Client](first-client.md).

## Installation

<!-- doc-snippet: skip -->
```scala
libraryDependencies ++= Seq(
  "io.github.oloveluck" %%% "mcp4s-core" % "0.2.0",
  "io.github.oloveluck" %%% "mcp4s-server" % "0.2.0",
  "io.github.oloveluck" %%% "mcp4s-client" % "0.2.0"
)
```

## Minimal Server

A server exposes capabilities that AI clients can discover and use. Here's a calculator server with one tool:

```scala
import cats.effect.*
import mcp4s.protocol.ServerInfo
import mcp4s.server.*
import mcp4s.server.dsl.*

@description("Add two numbers")
case class AddArgs(a: Double, b: Double) derives Schema

object MyServer extends IOApp.Simple:
  val add = Tool.from[AddArgs].handle[IO](args => IO.pure(ok(s"${args.a + args.b}")))

  val server = McpServer[IO](ServerInfo("calculator", "1.0.0")).withTools(add)

  def run = server.http().run
```

`derives Schema` generates a JSON schema from the case class, so AI clients know what arguments the tool accepts. `Tool.from[AddArgs]` derives the tool name from the class name (`AddArgs` → `"add"`) and takes the description from the class-level `@description` annotation. Capabilities are derived automatically — a tools-only server advertises only tools.

## Minimal Client

A client connects to a server, discovers its capabilities, and calls tools:

```scala
import cats.effect.*
import io.circe.Json, io.circe.syntax.*
import mcp4s.client.*
import mcp4s.client.syntax.*
import mcp4s.protocol.*

object MyClient extends IOApp.Simple:
  val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))

  def run = client.http("http://localhost:3000/mcp").use: conn =>
    conn
      .callTool("add", Json.obj("a" -> 5.asJson, "b" -> 3.asJson))
      .flatMap(r => IO.println(s"Result: $r"))
```

The URI is the **full MCP endpoint**, including the path (`/mcp`). Distributed tracing defaults
to noop; bring a `given Tracer[IO]` into scope to enable it.

## Key Concepts

**Type-safe arguments** — Derive JSON schemas from case classes:
```scala
case class Args(query: String, limit: Option[Int]) derives Schema
```

**Composable APIs** — Combine with `|+|`:
<!-- doc-snippet: reset -->
```scala
import mcp4s.server.dsl.*

val version = Tool("version").withDescription("Server version").handle[IO](_ => IO.pure(ok("1.0.0")))
val ping    = Tool("ping").withDescription("Health check").handle[IO](_ => IO.pure(ok("pong")))
val tools   = version |+| ping
```

**Resource safety** — Connections clean up automatically:
```scala
import io.circe.syntax.*
import mcp4s.client.syntax.*

client.http("http://localhost:3000/mcp").use: conn =>
  conn.callTool("add", Json.obj("a" -> 5.asJson, "b" -> 3.asJson))
```

---
**Next:** [Build Your First Server](first-server.md)
