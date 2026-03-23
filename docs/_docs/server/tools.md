# Tools

## Type-Safe Arguments

```scala
import mcp4s.protocol.ToolInput
import mcp4s.protocol.annotations.description

case class SearchArgs(
  query: String,
  @description("Max results") limit: Option[Int]
) derives ToolInput
```

## Constructors

```scala
import mcp4s.server.mcp.*

// Pure text result
Tool.text[IO, Args]("name", "desc") { args => "result" }

// Effectful
Tool[IO, Args]("name", "desc") { args => IO.pure(ok("result")) }

// No arguments
Tool.text[IO]("ping", "Ping") { "pong" }

// With context (sampling, progress, logging)
Tool.withContext[IO, Args]("smart", "AI tool") { (args, ctx) =>
  for
    _ <- ctx.log(LogLevel.Info, "Processing")
    _ <- ctx.progress(0.5, Some(100))
    response <- ctx.sampling.createMessage(params)
  yield ok(response.content.toString)
}
```

## Convenience Constructors

```scala
import mcp4s.server.McpTool

McpTool.singleString[IO]("echo", "Echo", "message") { msg => IO.pure(ToolResult.text(msg)) }
McpTool.singleNumber[IO]("double", "Double") { n => IO.pure(ToolResult.text(s"${n * 2}")) }
McpTool.twoNumbers[IO]("add", "Add", "a", "b") { (a, b) => IO.pure(ToolResult.text(s"${a + b}")) }
McpTool.noArgs[IO]("ping", "Ping") { IO.pure(ToolResult.text("pong")) }

// Pure variants
McpTool.singleStringPure[IO]("upper", "Uppercase") { _.toUpperCase }
McpTool.twoNumbersPure[IO]("add", "Add") { (a, b) => s"${a + b}" }

// Error handling
McpTool.attempt[IO, Args]("risky", "May fail") { args => riskyOp(args) }
```

## Results

```scala
ok("success")                    // Text result
error("failed")                  // Error result
content(textContent("a"), textContent("b"))  // Multiple items
```

## Composition

```scala
val tools = addTool |+| multiplyTool |+| divideTool
```

## Middleware

```scala
val tools = myTools.withMiddleware(
  McpMiddleware.logging[IO](println),
  McpMiddleware.catchErrors[IO]
)
```
