# McpConnection

An `McpConnection[F]` is the result of connecting a client to a server. It provides methods for all MCP operations. The connection handles request/response framing, session management, and capability checks automatically.

## Server Info

After connecting, you can inspect what the server supports:

```scala
conn.serverInfo          // ServerInfo
conn.serverCapabilities  // ServerCapabilities
conn.supportsTools       // Boolean
conn.supportsResources   // Boolean
conn.supportsPrompts     // Boolean
```

## Tools

Tools are the primary way AI clients interact with servers — calling functions and getting results:

```scala
conn.listTools                           // IO[List[Tool]]
conn.callTool("name", args)              // IO[ToolResult]
conn.callToolIfSupported("name", args)   // IO[Option[ToolResult]]
conn.callToolAsTask("name", args)        // IO[TaskId] (async)
```

## Resources

Resources provide read access to server-side data via URIs:

```scala
conn.listResources              // IO[List[Resource]]
conn.listResourceTemplates      // IO[List[ResourceTemplate]]
conn.readResource("uri")        // IO[ResourceContent]
conn.readResourceIfSupported("uri")  // IO[Option[ResourceContent]]
```

## Prompts

Prompts return reusable message templates for the AI to use:

```scala
conn.listPrompts                        // IO[List[Prompt]]
conn.getPrompt("name", args)            // IO[GetPromptResult]
conn.getPromptIfSupported("name", args) // IO[Option[GetPromptResult]]
```

## Tasks

For long-running operations, tools can run asynchronously as tasks:

```scala
conn.getTask(taskId)           // IO[TaskInfo]
conn.listTasks(cursor)         // IO[TasksListResult]
conn.cancelTask(taskId)        // IO[Unit]
conn.getTaskResult(taskId)     // IO[TaskResult]
```

## Streaming

For tools and resources that produce incremental results, use the streaming variants:

```scala
// Stream tool results as they arrive
conn.callToolStreaming("search", args): Stream[F, ToolResult]

// Stream resource content
conn.readResourceStreaming("events://live"): Stream[F, ResourceContent]
```

Streaming requires a persistent transport (HTTP with SSE or WebSocket).

## Lifecycle

```scala
conn.ping      // IO[Unit]
conn.shutdown  // IO[Unit]
conn.cancel(requestId)  // IO[Unit]
```

---
**Next:** [Resilience Patterns](resilience)
