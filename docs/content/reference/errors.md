# Error Handling

MCP uses JSON-RPC error codes to communicate failures. Errors are structured values — you can pattern match on them or handle them per-tool.

> For the full protocol specification, see [spec.modelcontextprotocol.io](https://spec.modelcontextprotocol.io/specification/2025-03-26/).

## McpError

```scala
case class McpError(code: Int, message: String, data: Option[Json]) extends Exception
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
Tool[IO, Args]("risky", "May fail") { args =>
  doWork(args).attempt.map {
    case Right(r) => ok(r)
    case Left(e) => error(e.getMessage)
  }
}
```

## Client-Side

```scala
conn.callTool("tool", args).attempt.flatMap {
  case Right(result) if result.isError => IO.println("Tool error")
  case Right(result) => IO.println(s"Success: $result")
  case Left(e: McpError) => IO.println(s"Protocol error ${e.code}")
  case Left(e) => IO.println(s"Connection error: ${e.getMessage}")
}
```

## With http4s Middleware

For HTTP transport, compose retry and timeout middleware on your `Client[F]`:

```scala
import org.http4s.client.middleware.{Retry, RetryPolicy, Timeout}
import scala.concurrent.duration.*

val retryPolicy = RetryPolicy[IO](RetryPolicy.exponentialBackoff(maxWait = 10.seconds, maxRetry = 3))
val resilientClient = Timeout(30.seconds)(Retry(retryPolicy)(rawHttpClient))

HttpClientTransport.connect[IO](client, config, resilientClient).use { conn =>
  conn.callTool("tool", args)
}
```
