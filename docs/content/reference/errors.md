# Error Handling

MCP uses JSON-RPC error codes to communicate failures. Errors are structured values — you can pattern match on them or handle them per-tool.

> For the full protocol specification, see [modelcontextprotocol.io](https://modelcontextprotocol.io/specification/2025-11-25/).

## McpError

`McpError` is an `enum` of the protocol failure modes; each case carries a human-readable
`message`. Map one to a JSON-RPC code with `McpError.toJsonRpcError(e).code`.

<!-- doc-snippet: skip -->
```scala
enum McpError(val message: String) extends Exception(message):
  case ToolNotFound(name: String)
  case ResourceNotFound(uri: String)
  case InvalidToolArguments(name: String, reason: String)
  case ToolExecutionError(name: String, detail: String)
  case MethodNotSupported(method: String)
  case NotInitialized
  // … and more
```

## Standard Codes

These are the JSON-RPC standard error codes used by MCP:

| Code | Name | Meaning |
|------|------|---------|
| -32700 | Parse error | Invalid JSON |
| -32600 | Invalid request | Malformed JSON-RPC |
| -32601 | Method not found | Unknown method |
| -32602 | Invalid params | Wrong arguments |
| -32603 | Internal error | Server-side failure |

## Server-Side

Return errors from tools using `attempt`:

```scala
import mcp4s.server.dsl.*

case class RiskyArgs(input: String) derives Schema
def doWork(args: RiskyArgs): IO[String] = ???

val risky = Tool("risky").withDescription("May fail").input[RiskyArgs].handle[IO] { args =>
  doWork(args).attempt.map:
    case Right(r) => ok(r)
    case Left(e)  => error(e.getMessage)
}
```

## Client-Side

```scala
conn.callTool("tool", Json.obj()).attempt.flatMap:
  case Right(result) if result.isError.getOrElse(false) => IO.println("Tool error")
  case Right(result)     => IO.println(s"Success: $result")
  case Left(e: McpError) => IO.println(s"Protocol error: ${e.message}")
  case Left(e)           => IO.println(s"Connection error: ${e.getMessage}")
```

Typed calls (`conn.call(endpoint)(input)` via `TypedClient`) never return an `isError` result silently — they raise `McpError.ToolExecutionError(name, detail)` instead, so tool failures surface in the same channel as protocol errors.

## Retry and Timeouts

For HTTP, compose standard http4s retry middleware on the `Client[F]` you pass to the transport — see [HTTP transport: Retry](../transports/http.md#retry). Request and initialization timeouts are built into every transport config via `timeouts = Timeouts(request = 5.minutes, init = 30.seconds)` (`mcp4s.transport.Timeouts`).
