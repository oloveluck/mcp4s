# Async Tasks

For long-running tool operations.

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

## Server Progress

```scala
Tool.withContext[IO, Args]("process", "Process data") { (args, ctx) =>
  data.zipWithIndex.traverse { case (item, idx) =>
    ctx.progress(idx.toDouble / data.size, Some(data.size)) *>
      processItem(item)
  }
}
```

## Task States

`Pending` → `Running` → `Completed` | `Failed` | `Cancelled`
