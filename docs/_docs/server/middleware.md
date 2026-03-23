# Middleware

Middleware intercepts tool calls for cross-cutting concerns.

## Built-in

```scala
import mcp4s.server.McpMiddleware

// Logging
McpMiddleware.logging[IO](msg => IO.println(msg))

// Timing
McpMiddleware.timed[IO] { (name, duration) =>
  IO.println(s"$name: ${duration.toMillis}ms")
}

// Error handling
McpMiddleware.catchErrors[IO]
McpMiddleware.catchErrorsPartial[IO] {
  case e: IllegalArgumentException => s"Invalid: ${e.getMessage}"
}

// Validation
McpMiddleware.validate[IO] { (name, args) =>
  IO.pure(if valid(args) then None else Some("Invalid"))
}
```

## Custom

```scala
val custom = new McpMiddleware[IO]:
  def apply(name: String, args: Json)(next: => IO[ToolResult]): IO[ToolResult] =
    IO.println(s"Before: $name") *> next <* IO.println(s"After: $name")
```

## Applying

```scala
val tools = myTools.withMiddleware(logging, timed, catchErrors)
```

Order matters — first is outermost:
1. **logging** — sees all calls
2. **timed** — measures execution
3. **catchErrors** — catches exceptions
