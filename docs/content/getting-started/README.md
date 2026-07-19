# Getting Started

> New to MCP? Read the [MCP introduction](https://modelcontextprotocol.io/introduction) for background on the protocol.

## What You're Building

In MCP, a **server** exposes tools, resources, and prompts. A **client** connects to servers and uses them.

This guide gets you running with both.

## Installation

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
import mcp4s.server.*
import mcp4s.server.mcp.*
import mcp4s.server.syntax.*
import mcp4s.protocol.*

@description("Add two numbers")
case class AddArgs(a: Double, b: Double) derives ToolInput

object MyServer extends IOApp.Simple:
  val add = Tool[IO, AddArgs](args => IO.pure(ok(s"${args.a + args.b}")))

  val server = Server.fromTools[IO](ServerInfo("calculator", "1.0.0"), add)

  def run = server.serveHttp().useForever
```

The `derives ToolInput` generates a JSON schema from the case class, so AI clients know what arguments the tool accepts. The `@description` annotation on the class becomes the tool description, and the tool name is derived from the class name (`AddArgs` → `"add"`).

## Minimal Client

A `Tracer[IO]` is needed as a type-class instance. Use `Tracer.noop` to disable tracing, or provide a real tracer for distributed observability.

A client connects to a server, discovers its capabilities, and calls tools:

```scala
import cats.effect.*
import io.circe.Json, io.circe.syntax.*
import mcp4s.client.*
import mcp4s.client.syntax.*
import org.typelevel.otel4s.trace.Tracer

object MyClient extends IOApp.Simple:
  given Tracer[IO] = Tracer.noop[IO]

  val client = McpClient.from[IO](ClientInfo("my-client", "1.0.0"))

  def run = client.connectHttp("http://localhost:3000").use: conn =>
    conn
      .callTool("add", Json.obj("a" -> 5.asJson, "b" -> 3.asJson))
      .flatMap(r => IO.println(s"Result: $r"))
```

## Key Concepts

**Type-safe arguments** — Derive JSON schemas from case classes:
```scala
case class Args(query: String, limit: Option[Int]) derives ToolInput
```

**Composable APIs** — Combine with `|+|`:
```scala
val tools = addTool |+| multiplyTool |+| divideTool
```

**Resource safety** — Connections clean up automatically:
```scala
client.connectHttp("http://localhost:3000").use: conn =>
  conn.callTool("add", args)
```

---
**Next:** [Build Your First Server](first-server.md)
