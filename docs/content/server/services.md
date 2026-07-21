# Services

An `McpService` is a named group of endpoint definitions shared between server and client — the mcp4s analogue of a smithy4s service. You define each tool endpoint **once**; the same object then drives server routing (with construction-time completeness checking) and a fully typed client (no stringly-typed tool names, no hand-rolled JSON).

## Define the Service

Declare each endpoint as a `val` and list them in `endpoints`:

```scala
import mcp4s.schema.{McpService, Prompt, Schema, Tool}
import mcp4s.protocol.description

case class AddArgs(a: Double, b: Double) derives Schema
case class AddResult(sum: Double) derives Schema

@description("Greet someone by name")
case class GreetArgs(name: String) derives Schema

object Calculator extends McpService("calculator", "1.0.0"):
  val add   = Tool("add").withDescription("Add two numbers").input[AddArgs].output[AddResult]
  val greet = Tool.from[GreetArgs]   // name + description derived from the type

  def endpoints = List(add, greet)
```

Because `add` declares `.output[AddResult]`, the server advertises an `outputSchema` and encodes results as `structuredContent`; the client decodes them back into `AddResult`.

## Implement It on the Server

`ServiceRoutes` binds one handler per declared endpoint and **fails fast at construction** — a missing, duplicate, or undeclared handler is an immediate `IllegalArgumentException` rather than a runtime `ToolNotFound`:

```scala
import cats.effect.IO
import mcp4s.server.*
import mcp4s.server.dsl.*

val routes: Tools[IO] = ServiceRoutes(Calculator)(
  Calculator.add.handle[IO](args => IO.pure(AddResult(args.a + args.b))),
  Calculator.greet.handle[IO](args => IO.pure(ok(s"Hello, ${args.name}!")))
)

val server = McpServer[IO](Calculator.info).withTools(routes)
```

`Calculator.info` is the `ServerInfo` derived from the service's name and version. The handlers use the same four shapes as any tool (`handle` / `handleWith` / `stream` / `streamWith`).

If you forget an endpoint:

```scala
ServiceRoutes(Calculator)(
  Calculator.add.handle[IO](args => IO.pure(AddResult(args.a + args.b)))
)
// IllegalArgumentException: Service 'calculator': no handler for endpoints: greet
```

## Call It from the Client

`TypedClient` adds endpoint-based calls to any `McpConnection[F]`. The input encodes via the endpoint's schema and the result decodes via its output schema:

```scala
import mcp4s.client.TypedClient.*

conn.call(Calculator.add)(AddArgs(19, 23))      // : IO[AddResult]
conn.call(Calculator.greet)(GreetArgs("Ada"))   // : IO[ToolResult]  (no .output declared)
```

- Endpoints with `.output[O]` decode from `structuredContent` (falling back to the first text content parsed as JSON).
- Endpoints without `.output` return the raw `ToolResult`.
- A result with `isError` set raises `McpError.ToolExecutionError` — typed calls never silently return errors.

Prompt endpoints work the same way:

```scala
val greeting = Prompt("greeting").input[GreetArgs]   // a PromptEndpoint, shareable like a val

conn.getPrompt(greeting)(GreetArgs("Ada"))  // : IO[GetPromptResult]
```

## End to End

```scala
import com.comcast.ip4s.port
import mcp4s.client.McpClientBuilder
import mcp4s.client.syntax.*
import mcp4s.client.TypedClient.*
import mcp4s.server.transport.HttpConfig

McpServer[IO](Calculator.info)
  .withTools(routes)
  .http(HttpConfig(port = port"3000"))
  .resource
  .use: _ =>
    McpClientBuilder[IO](ClientInfo("svc-client", "1.0.0"))
      .http("http://localhost:3000/mcp")
      .use: conn =>
        for
          sum   <- conn.call(Calculator.add)(AddArgs(19, 23))
          greet <- conn.call(Calculator.greet)(GreetArgs("Ada"))
        yield (sum.sum, greet.textContent)  // (42.0, "Hello, Ada!")
```

The wire format stays plain MCP — any other MCP client can still call `add` with raw JSON; the service layer is purely a compile-time convenience for Scala clients and a completeness check for servers.

---
**Next:** [HTTP Security](auth.md)
