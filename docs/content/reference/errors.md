# Error Handling

MCP uses JSON-RPC error codes to communicate failures. Errors are structured values — you can pattern match on them or handle them per-tool.

> For the full protocol specification, see [spec.modelcontextprotocol.io](https://spec.modelcontextprotocol.io/specification/2025-03-26/).

## McpError

`McpError` is an `enum` of the protocol failure modes; each case carries a human-readable
`message`. Map one to a JSON-RPC code with `McpError.toJsonRpcError(e).code`.

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
Tool("risky").withDescription("May fail").input[Args].handle[IO] { args =>
  doWork(args).attempt.map:
    case Right(r) => ok(r)
    case Left(e)  => error(e.getMessage)
}
```

## Client-Side

```scala
conn.callTool("tool", args).attempt.flatMap:
  case Right(result) if result.isError.getOrElse(false) => IO.println("Tool error")
  case Right(result)     => IO.println(s"Success: $result")
  case Left(e: McpError) => IO.println(s"Protocol error: ${e.message}")
  case Left(e)           => IO.println(s"Connection error: ${e.getMessage}")
```

Typed calls (`conn.call(endpoint)(input)` via `TypedClient`) never return an `isError` result silently — they raise `McpError.ToolExecutionError(name, detail)` instead, so tool failures surface in the same channel as protocol errors.

## With http4s Middleware

For HTTP transport, compose retry and timeout middleware on your `Client[F]`:

```scala
import org.http4s.client.middleware.{Retry, RetryPolicy, Timeout}
import mcp4s.client.transport.HttpTransportConfig
import scala.concurrent.duration.*

val retryPolicy = RetryPolicy[IO](RetryPolicy.exponentialBackoff(maxWait = 10.seconds, maxRetry = 3))
val resilientClient = Timeout(30.seconds)(Retry(retryPolicy)(rawHttpClient))

client.http(HttpTransportConfig[IO]("http://localhost:3000/mcp"), resilientClient).use: conn =>
  conn.callTool("tool", args)
```

Request and initialization timeouts are also built into every transport config via `timeouts = Timeouts(request = 5.minutes, init = 30.seconds)` (`mcp4s.transport.Timeouts`).
