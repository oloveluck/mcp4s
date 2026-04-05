# Protocol Reference

MCP uses **JSON-RPC 2.0** for all communication. Every message is a JSON-RPC request, response, error, or notification. This page documents the core protocol types as they appear in mcp4s.

> For the full protocol specification, see [spec.modelcontextprotocol.io](https://spec.modelcontextprotocol.io/specification/2025-03-26/).

## JSON-RPC

```scala
case class JsonRpcRequest(jsonrpc: String, method: String, params: Json, id: RequestId)
case class JsonRpcResponse(jsonrpc: String, result: Json, id: RequestId)
case class JsonRpcError(jsonrpc: String, error: ErrorObject, id: RequestId)
case class JsonRpcNotification(jsonrpc: String, method: String, params: Json)
```

Requests expect a response. Notifications are fire-and-forget. The `id` field correlates responses with their requests.

## Core Types

### Tools

Tools are functions the AI can call. Each has a name, description, and a JSON schema for its input:

```scala
case class Tool(name: String, description: Option[String], inputSchema: JsonSchema)
case class ToolResult(content: List[Content], isError: Boolean)
```

### Resources

Resources expose data via URI. Templates use `{param}` placeholders:

```scala
case class Resource(uri: String, name: String, description: Option[String], mimeType: Option[String])
case class ResourceContent(uri: String, mimeType: Option[String], text: Option[String], blob: Option[String])
```

### Prompts

Prompts are reusable message templates with optional arguments:

```scala
case class Prompt(name: String, description: Option[String], arguments: List[PromptArgument])
case class GetPromptResult(description: Option[String], messages: List[PromptMessage])
```

## Capabilities

During initialization, client and server declare what they support:

```scala
case class ClientCapabilities(roots: Option[_], sampling: Option[_], elicitation: Option[_])
case class ServerCapabilities(tools: Option[_], resources: Option[_], prompts: Option[_], tasks: Option[_])
```

## Content

Tool results and resource content can contain text, images, or audio:

```scala
sealed trait Content
case class TextContent(text: String) extends Content
case class ImageContent(data: String, mimeType: String) extends Content
case class AudioContent(data: String, mimeType: String) extends Content
```

## Sampling

Sampling lets the server request LLM completions from the client:

```scala
case class CreateMessageParams(
  messages: List[SamplingMessage],
  maxTokens: Int,
  temperature: Option[Double],
  modelPreferences: Option[ModelPreferences]
)

case class CreateMessageResult(role: Role, content: Content, model: String)
```

## Tasks

Tasks represent long-running operations that run asynchronously:

```scala
enum TaskStatus { case Pending, Running, Completed, Failed, Cancelled }
case class TaskInfo(id: TaskId, status: TaskStatus, progress: Option[Progress])
```
