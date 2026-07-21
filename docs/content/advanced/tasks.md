# Long-Running Tools

For operations that take more than a few seconds — data processing, file generation, complex
queries — keep the connection responsive by reporting progress and, where it fits, streaming
intermediate results.

## Progress Reporting

Report progress from a context-aware handler; the client receives `notifications/progress` while
the tool runs (over HTTP/SSE or WebSocket):

```scala
import mcp4s.server.dsl.*

case class ProcessArgs(items: List[String]) derives Schema
def processItem(item: String): IO[String] = ???

val process = Tool("process").withDescription("Process data").input[ProcessArgs].handleWith[IO] {
  (args, ctx) =>
    for results <- args.items.zipWithIndex.traverse { case (item, idx) =>
        ctx.progress(idx.toDouble / args.items.size, Some(args.items.size.toDouble)) *>
          processItem(item)
      }
    yield ok(results.mkString(", "))
}
```

The `progress` call sends a notification with:
- `progress`: how far along the operation is
- `total`: an optional total for display

On the client, pass an `onProgress` callback to receive them:

```scala
conn.callTool(
  "process",
  Json.obj("items" -> Json.arr(Json.fromString("a"), Json.fromString("b"))),
  p => IO.println(s"${p.progress}/${p.total.getOrElse("?")}")
)
```

## Streaming Handlers

A streaming handler emits intermediate values while it works; on the plain request/response call
path the last emitted value is the tool result:

```scala
val ticker = Tool("count").withDescription("Count up").stream[IO] { _ =>
  Stream.range(1, 6).map(n => ok(s"count: $n"))
}
```

## MCP Async Tasks

The MCP specification also defines an async-task protocol (`tasks/get`, `tasks/result`,
`tasks/cancel`) where a tool call returns a task ID that the client polls. mcp4s declares the
wire-level capability types (`ServerTasksCapability`, `ClientTasksCapability`) but does **not yet
implement** task execution on either side — for long-running work today, use progress
notifications and streaming as shown above.
