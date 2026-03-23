# Protocol Reference

## JSON-RPC

```scala
case class JsonRpcRequest(jsonrpc: String, method: String, params: Json, id: RequestId)
case class JsonRpcResponse(jsonrpc: String, result: Json, id: RequestId)
case class JsonRpcError(jsonrpc: String, error: ErrorObject, id: RequestId)
case class JsonRpcNotification(jsonrpc: String, method: String, params: Json)
```

## Core Types

```scala
// Tools
case class Tool(name: String, description: Option[String], inputSchema: JsonSchema)
case class ToolResult(content: List[Content], isError: Boolean)

// Resources
case class Resource(uri: String, name: String, description: Option[String], mimeType: Option[String])
case class ResourceContent(uri: String, mimeType: Option[String], text: Option[String], blob: Option[String])

// Prompts
case class Prompt(name: String, description: Option[String], arguments: List[PromptArgument])
case class GetPromptResult(description: Option[String], messages: List[PromptMessage])
```

## Capabilities

```scala
case class ClientCapabilities(roots: Option[_], sampling: Option[_], elicitation: Option[_])
case class ServerCapabilities(tools: Option[_], resources: Option[_], prompts: Option[_], tasks: Option[_])
```

## Content

```scala
sealed trait Content
case class TextContent(text: String) extends Content
case class ImageContent(data: String, mimeType: String) extends Content
case class AudioContent(data: String, mimeType: String) extends Content
```

## Sampling

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

```scala
enum TaskStatus { case Pending, Running, Completed, Failed, Cancelled }
case class TaskInfo(id: TaskId, status: TaskStatus, progress: Option[Progress])
```
