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
conn.listAllTools                        // IO[List[Tool]] (follows pagination)
conn.listTools(cursor)                   // IO[(List[Tool], Option[String])] one page
conn.callTool("name", args)              // IO[ToolResult]
conn.callToolIfSupported("name", args)   // IO[Option[ToolResult]]
```

## Resources

Resources provide read access to server-side data via URIs:

```scala
conn.listAllResources                // IO[List[Resource]]
conn.listAllResourceTemplates        // IO[List[ResourceTemplate]]
conn.readResource("uri")             // IO[ResourceContent]
conn.readResourceIfSupported("uri")  // IO[Option[ResourceContent]]
```

## Prompts

Prompts return reusable message templates for the AI to use:

```scala
conn.listAllPrompts                     // IO[List[Prompt]]
conn.getPrompt("name", args)            // IO[GetPromptResult]
conn.getPromptIfSupported("name", args) // IO[Option[GetPromptResult]]
```

## Progress

For long-running tools, pass an `onProgress` callback to receive `notifications/progress` as the
server reports them (over HTTP/SSE or WebSocket):

```scala
conn.callTool("index", args, p => IO.println(s"${p.progress}/${p.total.getOrElse("?")}"))
```

## Lifecycle

```scala
conn.ping      // IO[Unit]
conn.shutdown  // IO[Unit]
conn.cancel(requestId)  // IO[Unit]
```

---
**Next:** [Transports](../transports/README.md)
