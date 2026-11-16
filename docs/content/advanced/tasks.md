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

## MCP Tasks

For work that outlives a single request/response, MCP 2025-11-25 defines **tasks** (SEP-1686,
experimental): a task-augmented `tools/call` returns a task id immediately, and the client polls
`tasks/get` and fetches the outcome with `tasks/result`.

Declare which tools may run as tasks with `withTaskSupport`; a server registering such a tool
advertises the `tasks` capability automatically:

```scala
import mcp4s.protocol.TaskSupport

case class ExportArgs(table: String) derives Schema
def exportTable(table: String): IO[String] = ???

val exportTool = Tool("export")
  .withDescription("Export a table to a file")
  .input[ExportArgs]
  .withTaskSupport(TaskSupport.Optional)      // or Required / Forbidden (the default)
  .handleWith[IO] { (args, ctx) =>
    ctx.progress(0, Some(1)) *> exportTable(args.table).map(path => ok(path))
  }
```

The handler is unchanged — the dispatcher runs it in a background fiber, tracks the task's status
(`working` → `completed`/`failed`/`cancelled`), retains the result for the task's `ttl`
(10 minutes by default, configurable through `Dispatcher.Config`), and answers `tasks/get`,
`tasks/result`, `tasks/cancel` and `tasks/list`. A tool that returns `isError` is reported as a
`failed` task, as the spec requires. Progress and log notifications the tool emits after the
original call returned are delivered on the session's standalone stream (HTTP `GET`, or the
WebSocket/stdio connection).

On the client:

```scala
import mcp4s.protocol.TaskStatus

conn.callToolTask("export", Json.obj("table" -> Json.fromString("orders"))).flatMap { task =>
  conn.taskResult(task.taskId).map(_.textContent)   // the server answers once the task is terminal
}

conn.getTask("task-id")                                // IO[Task]: status, timestamps, ttl, pollInterval
conn.cancelTask("task-id")                             // IO[Task]
conn.listTasks()                                       // IO[(List[Task], Option[String])]
```

`conn.supportsToolTasks` tells you whether the server accepts task-augmented calls; tools advertise
their own level in `Tool.taskSupport`.
