# mcp4s

A Scala 3 implementation of the [Model Context Protocol](https://modelcontextprotocol.io/) for the
Typelevel stack: cats-effect, fs2, http4s and circe. It runs on the JVM, Scala.js and Scala Native.

MCP is how AI applications talk to the outside world. A **server** offers *tools* the model can
call, *resources* it can read and *prompts* it can reuse; a **client** (Claude Desktop, an IDE
agent, your own app) connects to servers and uses what they offer. mcp4s lets you write either
side, or both.

## Thirty seconds

```scala
import cats.effect.*
import mcp4s.protocol.ServerInfo
import mcp4s.server.*
import mcp4s.server.dsl.*

@description("Add two numbers")
case class AddArgs(a: Double, b: Double) derives Schema

object Calculator extends IOApp.Simple:
  val add    = Tool.from[AddArgs].handle[IO](args => IO.pure(ok(s"${args.a + args.b}")))
  val server = McpServer[IO](ServerInfo("calculator", "1.0.0")).withTools(add)

  def run = server.http().run   // Streamable HTTP on :3000/mcp
```

Three ideas carry the whole library, and this one example shows all of them:

- **Describe a type once.** `derives Schema` is the only description of `AddArgs`; the JSON Schema
  the model sees, the decoder that validates its arguments and the prompt-argument metadata are all
  derived from it, so they cannot disagree.
- **An endpoint is a value.** `Tool.from[AddArgs]` is a definition with no behaviour. Attach a
  handler and it becomes a route; share the same value with a client and calls become typed.
- **Everything composes.** Routes combine with `|+|`, servers combine with `|+|`, and the server
  advertises exactly the capabilities of what you registered.

## Where to go

| If you want to… | Read |
|---|---|
| Build a server and a client and run them | [Getting Started](getting-started.md) |
| Understand how the pieces fit before writing much code | [Architecture](concepts/architecture.md) |
| Know what `derives Schema` does and does not handle | [Schema](concepts/schema.md) |
| Pick a transport, or understand how one works | [Transports](concepts/transports.md) |
| Let a tool talk back to the client (sampling, elicitation, progress, tasks) | [Server-to-client traffic](concepts/server-to-client.md) |
| Secure, test or benchmark a server | [Guides](guides/README.md) |

The pages here explain the design and the decisions behind it. For exact signatures, the Scaladoc
and the source are the reference; every Scala snippet in these pages is compiled against the real
API on each build.

## Installation

<!-- doc-snippet: skip -->
```scala
libraryDependencies ++= Seq(
  "io.github.oloveluck" %%% "mcp4s-core"   % "0.3.1",
  "io.github.oloveluck" %%% "mcp4s-server" % "0.3.1",   // servers
  "io.github.oloveluck" %%% "mcp4s-client" % "0.3.1"    // clients
)
```

`%%%` resolves the artifact for the JVM, Scala.js or Scala Native; use `%%` for a JVM-only build.
`mcp4s-testkit` adds compliance and performance suites for any MCP server
([Testing](guides/testing.md)).
