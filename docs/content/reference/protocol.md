# Protocol Reference

MCP uses **JSON-RPC 2.0** for all communication. Every message is a JSON-RPC request, response, error, or notification. This page documents the core protocol types as they appear in mcp4s.

> For the full protocol specification, see [spec.modelcontextprotocol.io](https://spec.modelcontextprotocol.io/specification/2025-03-26/).

## JSON-RPC

<!-- doc-snippet: skip -->
```scala
case class JsonRpcRequest(id: RequestId, method: String, params: Option[Json])
case class JsonRpcResponse(id: RequestId, result: Json)
case class JsonRpcErrorResponse(id: RequestId, error: JsonRpcError)
case class JsonRpcNotification(method: String, params: Option[Json])
```

Requests expect a response. Notifications are fire-and-forget. The `id` field correlates responses with their requests.

## Core Types

### Tools

Tools are functions the AI can call. Each has a name, description, and a JSON schema for its input:

<!-- doc-snippet: skip -->
```scala
case class Tool(name: String, description: Option[String], inputSchema: JsonSchema,
                outputSchema: Option[JsonSchema], annotations: Option[ToolAnnotations])
case class ToolResult(content: List[Content], isError: Option[Boolean],
                      structuredContent: Option[Json])
```

### Resources

Resources expose data via URI. Templates use `{param}` placeholders:

<!-- doc-snippet: skip -->
```scala
case class Resource(uri: String, name: String, description: Option[String], mimeType: Option[String])
case class ResourceContent(uri: String, mimeType: Option[String], text: Option[String], blob: Option[String])
```

### Prompts

Prompts are reusable message templates with optional arguments:

<!-- doc-snippet: skip -->
```scala
case class Prompt(name: String, description: Option[String], arguments: List[PromptArgument])
case class GetPromptResult(description: Option[String], messages: List[PromptMessage])
```

## Capabilities

During initialization, client and server declare what they support:

<!-- doc-snippet: skip -->
```scala
case class ClientCapabilities(roots: Option[RootsCapability], sampling: Option[SamplingCapability],
                              elicitation: Option[ElicitationCapability])
case class ServerCapabilities(tools: Option[ToolsCapability], resources: Option[ResourcesCapability],
                              prompts: Option[PromptsCapability], logging: Option[LoggingCapability])
```

mcp4s derives these from what you register: a tools-only server advertises only `tools`, and a
client advertises `sampling` only when a sampling handler is present.

## Content

Tool results and resource content can contain text, images, or audio:

<!-- doc-snippet: skip -->
```scala
sealed trait Content
case class TextContent(text: String) extends Content
case class ImageContent(data: String, mimeType: String) extends Content
case class AudioContent(data: String, mimeType: String) extends Content
```

## Sampling

Sampling lets the server request LLM completions from the client:

<!-- doc-snippet: skip -->
```scala
case class CreateMessageParams(
  messages: List[SamplingMessage],
  maxTokens: Int,
  modelPreferences: Option[ModelPreferences],
  systemPrompt: Option[String],
  temperature: Option[Double]
)

case class CreateMessageResult(role: Role, content: SamplingContent, model: String,
                               stopReason: Option[String])
```

## Tasks

The spec defines an async-task protocol; mcp4s currently declares only the capability types
(`ServerTasksCapability`, `ClientTasksCapability`) — see
[Long-Running Tools](../advanced/tasks.md) for what to use today.
