# Error Handling

## McpError

```scala
case class McpError(code: Int, message: String, data: Option[Json]) extends Exception
```

## Standard Codes

| Code | Name |
|------|------|
| -32700 | Parse error |
| -32600 | Invalid request |
| -32601 | Method not found |
| -32602 | Invalid params |
| -32603 | Internal error |

## Server-Side

```scala
// Return error result
Tool[IO, Args]("risky", "May fail") { args =>
  doWork(args).attempt.map {
    case Right(r) => ok(r)
    case Left(e) => error(e.getMessage)
  }
}

// Middleware catches exceptions
myTools.withMiddleware(Middleware.catchErrors[IO])
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

## With Resilience

```scala
// Automatic retry on transient errors
conn.withResilience(ResilienceConfig.default).flatMap { resilient =>
  resilient.callTool("tool", args)
}
```
