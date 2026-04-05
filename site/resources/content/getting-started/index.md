# Getting Started

> New to MCP? Read the [MCP introduction](https://modelcontextprotocol.io/introduction) for background on the protocol.

## What You're Building

In MCP, a **server** exposes tools, resources, and prompts. A **client** connects to servers and uses them.

This guide gets you running with both.

## Installation

```scala
// Mill
mvn"io.github.oloveluck::mcp4s-server::0.1.5"
mvn"io.github.oloveluck::mcp4s-client::0.1.5"

// SBT
"io.github.oloveluck" %% "mcp4s-server" % "0.1.5"
"io.github.oloveluck" %% "mcp4s-client" % "0.1.5"
```

## Minimal Server

A server exposes capabilities that AI clients can discover and use. Here's a calculator server with one tool:

```scala
import cats.effect.*
import mcp4s.server.*
import mcp4s.server.mcp.*
import mcp4s.protocol.ToolInput

case class AddArgs(a: Double, b: Double) derives ToolInput

object MyServer extends IOApp.Simple:
  val server = Server.builder[IO]
    .withInfo(ServerInfo("calculator", "1.0.0"))
    .tool[AddArgs]("add", "Add two numbers") { args =>
      IO.pure(ok(s"${args.a + args.b}"))
    }
    .build

  def run: IO[Unit] = server.serveHttp(3000)
```

The `derives ToolInput` generates a JSON schema from the case class, so AI clients know what arguments the tool accepts.

## Minimal Client

The `Tracer[IO]` is required for [OpenTelemetry](https://opentelemetry.io/) trace propagation. Use `Tracer.noop` to disable tracing, or provide a real tracer for distributed observability across MCP client-server interactions.

A client connects to a server, discovers its capabilities, and calls tools:

```scala
import cats.effect.*
import io.circe.Json
import mcp4s.client.*
import mcp4s.client.transport.*
import org.typelevel.otel4s.trace.Tracer

object MyClient extends IOApp.Simple:
  given Tracer[IO] = Tracer.noop[IO]

  val client = McpClient.builder[IO]
    .withInfo(ClientInfo("my-client", "1.0.0"))
    .build

  def run: IO[Unit] =
    HttpClientTransport.connect[IO](client, HttpClientConfig("http://localhost:3000")).use { conn =>
      conn.callTool("add", Json.obj("a" -> Json.fromDouble(5).get, "b" -> Json.fromDouble(3).get))
        .flatMap(r => IO.println(s"Result: $r"))
    }
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
transport.connect(client, config).use { conn => ... }
```

---
**Next:** [Build Your First Server](first-server)
