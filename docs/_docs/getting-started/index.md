# Getting Started

## Installation

```scala
// Mill
mvn"io.github.mcp4s::mcp4s-server::0.1.3"
mvn"io.github.mcp4s::mcp4s-client::0.1.3"

// SBT
"io.github.mcp4s" %% "mcp4s-server" % "0.1.3"
"io.github.mcp4s" %% "mcp4s-client" % "0.1.3"
```

## Minimal Server

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

## Minimal Client

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
