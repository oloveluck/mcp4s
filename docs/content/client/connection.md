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
val args = Json.obj("query" -> Json.fromString("scala"))

conn.listAllTools                        // IO[List[Tool]] (follows pagination)
conn.listTools(cursor = None)            // IO[(List[Tool], Option[String])] one page
conn.callTool("name", args)              // IO[ToolResult]
conn.callToolIfSupported(ToolName("name"), args)  // IO[Option[ToolResult]]
```

### Typed Calls

With an endpoint definition (see [Services](../server/services.md)), calls are typed both ways — the input encodes via the endpoint's schema and the result decodes via its output schema:

```scala
import mcp4s.client.TypedClient.*
import mcp4s.schema.{Prompt as PromptDef, Schema, Tool as ToolDef}

case class AddArgs(a: Double, b: Double) derives Schema
case class AddResult(sum: Double) derives Schema
case class GreetArgs(name: String) derives Schema

val add      = ToolDef("add").input[AddArgs].output[AddResult]
val greeting = PromptDef("greeting").input[GreetArgs]

conn.call(add)(AddArgs(1, 2))              // IO[AddResult]
conn.getPrompt(greeting)(GreetArgs("Ada")) // IO[GetPromptResult]
```

An `isError` result raises `McpError.ToolExecutionError` instead of being returned silently.

## Resources

Resources provide read access to server-side data via URIs:

```scala
conn.listAllResources                // IO[List[Resource]]
conn.listAllResourceTemplates        // IO[List[ResourceTemplate]]
conn.readResource("uri")             // IO[ResourceContent]
conn.readResourceIfSupported(ResourceUri("uri"))  // IO[Option[ResourceContent]]
```

## Prompts

Prompts return reusable message templates for the AI to use:

```scala
val promptArgs = Map("name" -> "Ada")

conn.listAllPrompts                     // IO[List[Prompt]]
conn.getPrompt("name", promptArgs)      // IO[GetPromptResult]
conn.getPromptIfSupported(PromptName("name"), promptArgs) // IO[Option[GetPromptResult]]
```

## Progress

For long-running tools, pass an `onProgress` callback to receive `notifications/progress` as the
server reports them (over HTTP/SSE or WebSocket):

```scala
conn.callTool("index", args, p => IO.println(s"${p.progress}/${p.total.getOrElse("?")}"))
```

## Lifecycle

```scala
conn.ping                             // IO[Unit]
conn.shutdown                         // IO[Unit]
conn.cancel(RequestId.NumberId(42))   // IO[Unit]
```

---
**Next:** [Transports](../transports/README.md)
