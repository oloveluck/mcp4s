# Async Tasks

MCP tasks let you run long-running tool operations asynchronously. Instead of blocking until completion, the client receives a task ID and can poll for progress, retrieve results, or cancel.

> For the full protocol details, see the [MCP specification](https://spec.modelcontextprotocol.io/specification/2025-03-26/).

Tasks are useful for operations that take more than a few seconds — data processing, file generation, complex queries — where you want to keep the connection responsive.

## Client Usage

```scala
// Start async
val taskId: IO[TaskId] = conn.callToolAsTask("long-query", args)

// Poll status
conn.getTask(taskId)       // IO[TaskInfo]
conn.listTasks()           // IO[TasksListResult]
conn.cancelTask(taskId)    // IO[Unit]
conn.getTaskResult(taskId) // IO[TaskResult]
```

## Server-Side Progress

Report progress during long operations using the tool context:

```scala
Tool.withContext[IO, Args]("process", "Process data") { (args, ctx) =>
  for
    data <- loadData(args)
    results <- data.zipWithIndex.traverse { case (item, idx) =>
      ctx.progress(idx.toDouble / data.size, Some(data.size)) *>
        processItem(item)
    }
  yield ok(results.mkString(", "))
}
```

The `progress` call sends a notification to the client with:
- `progress`: a `Double` between 0.0 and 1.0
- `total`: an optional total count for display

## Task Lifecycle

```
Pending ──> Running ──> Completed
                   ├──> Failed
                   └──> Cancelled
```

- **Pending** — Task created but not yet started
- **Running** — Actively executing, may emit progress
- **Completed** — Finished successfully, result available via `getTaskResult`
- **Failed** — Terminated with an error
- **Cancelled** — Stopped by client via `cancelTask`

## Transport Requirements

Tasks require a persistent connection for progress notifications. They work with all transports (HTTP with SSE, WebSocket, Stdio), but progress updates are only delivered while the connection is active.

Both client and server must declare task support in their capabilities during initialization.
