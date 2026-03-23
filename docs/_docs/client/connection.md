# McpConnection

## Server Info

```scala
conn.serverInfo          // ServerInfo
conn.serverCapabilities  // ServerCapabilities
conn.supportsTools       // Boolean
conn.supportsResources   // Boolean
conn.supportsPrompts     // Boolean
```

## Tools

```scala
conn.listTools                           // IO[List[Tool]]
conn.callTool("name", args)              // IO[ToolResult]
conn.callToolIfSupported("name", args)   // IO[Option[ToolResult]]
conn.callToolAsTask("name", args)        // IO[TaskId] (async)
```

## Resources

```scala
conn.listResources              // IO[List[Resource]]
conn.listResourceTemplates      // IO[List[ResourceTemplate]]
conn.readResource("uri")        // IO[ResourceContent]
conn.readResourceIfSupported("uri")  // IO[Option[ResourceContent]]
```

## Prompts

```scala
conn.listPrompts                        // IO[List[Prompt]]
conn.getPrompt("name", args)            // IO[GetPromptResult]
conn.getPromptIfSupported("name", args) // IO[Option[GetPromptResult]]
```

## Tasks

```scala
conn.getTask(taskId)           // IO[TaskInfo]
conn.listTasks(cursor)         // IO[TasksListResult]
conn.cancelTask(taskId)        // IO[Unit]
conn.getTaskResult(taskId)     // IO[TaskResult]
```

## Lifecycle

```scala
conn.ping      // IO[Unit]
conn.shutdown  // IO[Unit]
conn.cancel(requestId)  // IO[Unit]
```
