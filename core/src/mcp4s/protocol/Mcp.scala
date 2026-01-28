package mcp4s.protocol

import io.circe.{Encoder, Json}
import io.circe.syntax.*
import java.util.Base64

// === Opaque Types for Type Safety ===

/** Type-safe tool name */
opaque type ToolName = String

object ToolName:
  def apply(value: String): ToolName = value

  extension (name: ToolName)
    def value: String = name

/** Type-safe resource URI */
opaque type ResourceUri = String

object ResourceUri:
  def apply(value: String): ResourceUri = value

  extension (uri: ResourceUri)
    def value: String = uri

/** Type-safe prompt name */
opaque type PromptName = String

object PromptName:
  def apply(value: String): PromptName = value

  extension (name: PromptName)
    def value: String = name

/** MCP Protocol version
  * Spec ref: https://modelcontextprotocol.io/specification/2025-03-26/basic/lifecycle
  */
object McpVersion:
  /** Current MCP protocol version */
  val Current: String = "2025-03-26"

// === Common Types ===

/** Icon for UI display
  * Spec ref: schema.ts Icon interface
  */
final case class Icon(
    src: String,
    mimeType: Option[String] = None,
    sizes: Option[List[String]] = None,
    theme: Option[String] = None
)

object Icon:
  /** Create an icon with just a source URL */
  def simple(src: String): Icon = Icon(src)

/** Annotations for content and resources
  * Spec ref: schema.ts Annotations interface
  */
final case class Annotations(
    audience: Option[List[String]] = None,  // "user" | "assistant"
    priority: Option[Double] = None,         // 0.0-1.0
    lastModified: Option[String] = None      // ISO 8601 datetime
)

/** Server information
  * Spec ref: schema.ts Implementation interface
  */
final case class ServerInfo(
    name: String,
    version: String,
    title: Option[String] = None,
    description: Option[String] = None,
    websiteUrl: Option[String] = None,
    icons: Option[List[Icon]] = None
)

object ServerInfo:
  /** Create server info with just name and version */
  def minimal(name: String, version: String): ServerInfo =
    ServerInfo(name, version)

/** Client information
  * Spec ref: schema.ts Implementation interface
  */
final case class ClientInfo(
    name: String,
    version: String,
    title: Option[String] = None,
    description: Option[String] = None,
    websiteUrl: Option[String] = None,
    icons: Option[List[Icon]] = None
)

object ClientInfo:
  /** Create client info with just name and version */
  def minimal(name: String, version: String): ClientInfo =
    ClientInfo(name, version)

/** Implementation info for capabilities (alias for backward compatibility) */
final case class Implementation(
    name: String,
    version: String,
    title: Option[String] = None,
    description: Option[String] = None,
    websiteUrl: Option[String] = None,
    icons: Option[List[Icon]] = None
)

// === Capabilities ===
// Spec ref: schema.ts ServerCapabilities, ClientCapabilities

final case class ToolsCapability(
    listChanged: Option[Boolean] = None
)

final case class ResourcesCapability(
    subscribe: Option[Boolean] = None,
    listChanged: Option[Boolean] = None
)

final case class PromptsCapability(
    listChanged: Option[Boolean] = None
)

final case class LoggingCapability()

final case class CompletionsCapability()

// Server tasks capability for async operations
final case class ServerTasksCapability(
    list: Option[Json] = None,
    cancel: Option[Json] = None,
    requests: Option[ServerTaskRequests] = None
)

final case class ServerTaskRequests(
    tools: Option[ToolTaskRequests] = None
)

final case class ToolTaskRequests(
    call: Option[Json] = None
)

/** Server capabilities
  * Spec ref: schema.ts ServerCapabilities
  */
final case class ServerCapabilities(
    tools: Option[ToolsCapability] = None,
    resources: Option[ResourcesCapability] = None,
    prompts: Option[PromptsCapability] = None,
    logging: Option[LoggingCapability] = None,
    completions: Option[CompletionsCapability] = None,
    tasks: Option[ServerTasksCapability] = None,
    experimental: Option[Json] = None
)

object ServerCapabilities:
  val empty: ServerCapabilities = ServerCapabilities()

  def withTools: ServerCapabilities =
    ServerCapabilities(tools = Some(ToolsCapability()))

  def withResources: ServerCapabilities =
    ServerCapabilities(resources = Some(ResourcesCapability()))

  def withPrompts: ServerCapabilities =
    ServerCapabilities(prompts = Some(PromptsCapability()))

// Client tasks capability
final case class ClientTasksCapability(
    list: Option[Json] = None,
    cancel: Option[Json] = None,
    requests: Option[ClientTaskRequests] = None
)

final case class ClientTaskRequests(
    sampling: Option[SamplingTaskRequests] = None,
    elicitation: Option[ElicitationTaskRequests] = None
)

final case class SamplingTaskRequests(
    createMessage: Option[Json] = None
)

final case class ElicitationTaskRequests(
    create: Option[Json] = None
)

final case class ElicitationCapability(
    form: Option[Json] = None,
    url: Option[Json] = None
)

/** Client capabilities
  * Spec ref: schema.ts ClientCapabilities
  */
final case class ClientCapabilities(
    roots: Option[RootsCapability] = None,
    sampling: Option[SamplingCapability] = None,
    elicitation: Option[ElicitationCapability] = None,
    tasks: Option[ClientTasksCapability] = None,
    experimental: Option[Json] = None
)

final case class RootsCapability(
    listChanged: Option[Boolean] = None
)

final case class SamplingCapability(
    context: Option[Json] = None,
    tools: Option[Json] = None
)

// === Tools ===
// Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/server/tools

/** JSON Schema for tool input/output */
final case class JsonSchema(
    `type`: String,
    properties: Option[Map[String, JsonSchemaProperty]] = None,
    required: Option[List[String]] = None,
    `$schema`: Option[String] = None
)

final case class JsonSchemaProperty(
    `type`: String,
    description: Option[String] = None,
    `enum`: Option[List[String]] = None,
    // SEP-1034: Default values
    default: Option[Json] = None,
    // SEP-1330: Titled enums
    oneOf: Option[List[Json]] = None,      // [{const: "val", title: "Label"}, ...]
    enumNames: Option[List[String]] = None, // Legacy titled enums
    // SEP-1330: Array types with enums
    items: Option[JsonSchemaProperty] = None,
    anyOf: Option[List[Json]] = None        // For titled multi-select
)

object JsonSchema:
  /** Empty schema that accepts any input */
  val empty: JsonSchema = JsonSchema("object", None, None)

  /** Create an object schema from a map of properties */
  def obj(
      properties: Map[String, JsonSchemaProperty],
      required: List[String] = Nil
  ): JsonSchema =
    JsonSchema("object", Some(properties), if (required.isEmpty) None else Some(required))

  /** Create an object schema from varargs of property tuples */
  def obj(properties: (String, JsonSchemaProperty)*): JsonSchema =
    JsonSchema("object", Some(properties.toMap), None)

  // === Property constructors with Option description ===

  def string(description: Option[String] = None): JsonSchemaProperty =
    JsonSchemaProperty("string", description, None)

  def number(description: Option[String] = None): JsonSchemaProperty =
    JsonSchemaProperty("number", description, None)

  def boolean(description: Option[String] = None): JsonSchemaProperty =
    JsonSchemaProperty("boolean", description, None)

  def integer(description: Option[String] = None): JsonSchemaProperty =
    JsonSchemaProperty("integer", description, None)

  def stringEnum(values: List[String], description: Option[String] = None): JsonSchemaProperty =
    JsonSchemaProperty("string", description, Some(values))

  // === Property constructors with String description (convenience) ===

  def string(description: String): JsonSchemaProperty =
    JsonSchemaProperty("string", Some(description), None)

  def number(description: String): JsonSchemaProperty =
    JsonSchemaProperty("number", Some(description), None)

  def boolean(description: String): JsonSchemaProperty =
    JsonSchemaProperty("boolean", Some(description), None)

  def integer(description: String): JsonSchemaProperty =
    JsonSchemaProperty("integer", Some(description), None)

  def stringEnum(values: List[String], description: String): JsonSchemaProperty =
    JsonSchemaProperty("string", Some(description), Some(values))

  // === SEP-1034: Property constructors with default values ===

  def stringWithDefault(description: String, default: String): JsonSchemaProperty =
    JsonSchemaProperty("string", Some(description), None, Some(Json.fromString(default)))

  def integerWithDefault(description: String, default: Int): JsonSchemaProperty =
    JsonSchemaProperty("integer", Some(description), None, Some(Json.fromInt(default)))

  def booleanWithDefault(description: String, default: Boolean): JsonSchemaProperty =
    JsonSchemaProperty("boolean", Some(description), None, Some(Json.fromBoolean(default)))

  def numberWithDefault(description: String, default: Double): JsonSchemaProperty =
    JsonSchemaProperty("number", Some(description), None, Some(Json.fromDoubleOrNull(default)))

  // === SEP-1330: Titled enum constructors ===

  /** Create a titled enum property using oneOf with const/title pairs */
  def titledEnum(values: List[(String, String)], description: Option[String] = None): JsonSchemaProperty =
    val oneOfList = values.map { case (value, title) =>
      Json.obj("const" -> Json.fromString(value), "title" -> Json.fromString(title))
    }
    JsonSchemaProperty("string", description, None, None, Some(oneOfList))

  /** Create a titled enum property using oneOf with const/title pairs (convenience overload) */
  def titledEnum(description: String, values: (String, String)*): JsonSchemaProperty =
    titledEnum(values.toList, Some(description))

  /** Create an array property with items schema */
  def array(items: JsonSchemaProperty, description: Option[String] = None): JsonSchemaProperty =
    JsonSchemaProperty("array", description, None, None, None, None, Some(items))

  /** Create an array property with items schema (convenience overload) */
  def array(description: String, items: JsonSchemaProperty): JsonSchemaProperty =
    array(items, Some(description))

  /** Create a string enum with a default value */
  def stringEnumWithDefault(values: List[String], default: String, description: Option[String] = None): JsonSchemaProperty =
    JsonSchemaProperty("string", description, Some(values), Some(Json.fromString(default)))

  /** Create a legacy titled enum using enumNames (deprecated but needed for conformance) */
  def legacyTitledEnum(values: List[String], names: List[String], description: Option[String] = None): JsonSchemaProperty =
    JsonSchemaProperty("string", description, Some(values), None, None, Some(names))

  /** Create an array with titled multi-select using anyOf */
  def titledMultiSelect(values: List[(String, String)], description: Option[String] = None): JsonSchemaProperty =
    val anyOfList = values.map { case (value, title) =>
      Json.obj("const" -> Json.fromString(value), "title" -> Json.fromString(title))
    }
    JsonSchemaProperty("array", description, None, None, None, None,
      Some(JsonSchemaProperty("string", None, None, None, None, None, None, Some(anyOfList)))
    )

  // === Extension for fluent required fields ===

  extension (schema: JsonSchema)
    /** Add required fields to a schema */
    def required(fields: String*): JsonSchema =
      schema.copy(required = Some(fields.toList))

/** Tool annotations for AI safety hints
  * Spec ref: schema.ts ToolAnnotations
  * CRITICAL for AI safety - describes tool behavior to LLMs
  */
final case class ToolAnnotations(
    title: Option[String] = None,
    readOnlyHint: Option[Boolean] = None,      // Default: false - tool doesn't modify state
    destructiveHint: Option[Boolean] = None,   // Default: true - may perform destructive ops
    idempotentHint: Option[Boolean] = None,    // Default: false - safe to retry
    openWorldHint: Option[Boolean] = None      // Default: true - may interact with external world
)

object ToolAnnotations:
  /** Tool that only reads data, doesn't modify state */
  def readOnly(): ToolAnnotations =
    ToolAnnotations(readOnlyHint = Some(true), destructiveHint = Some(false))

  /** Tool that may perform destructive operations */
  def destructive(): ToolAnnotations =
    ToolAnnotations(destructiveHint = Some(true))

  /** Tool that is safe to retry (idempotent) */
  def idempotent(): ToolAnnotations =
    ToolAnnotations(idempotentHint = Some(true))

  /** Tool that only operates locally, no external interactions */
  def localOnly(): ToolAnnotations =
    ToolAnnotations(openWorldHint = Some(false))

/** Task support for async tool execution
  * Spec ref: schema.ts ToolExecution
  */
enum TaskSupport:
  case Forbidden, Optional, Required

final case class ToolExecution(
    taskSupport: Option[TaskSupport] = None
)

/** Tool definition
  * Spec ref: schema.ts Tool interface
  */
final case class Tool(
    name: String,
    description: Option[String] = None,
    inputSchema: JsonSchema,
    title: Option[String] = None,
    outputSchema: Option[JsonSchema] = None,
    execution: Option[ToolExecution] = None,
    annotations: Option[ToolAnnotations] = None,
    icons: Option[List[Icon]] = None,
    _meta: Option[Json] = None
)

/** Tool call result content types
  * Spec ref: schema.ts ContentBlock union
  */
enum ContentType:
  case Text, Image, Audio, Resource, ResourceLink

/** Content in tool results
  * Spec ref: schema.ts ContentBlock
  */
sealed trait Content:
  def `type`: ContentType

/** Text content
  * Spec ref: schema.ts TextContent
  */
final case class TextContent(
    text: String,
    annotations: Option[Annotations] = None,
    _meta: Option[Json] = None
) extends Content:
  def `type`: ContentType = ContentType.Text

/** Image content (base64 encoded)
  * Spec ref: schema.ts ImageContent
  */
final case class ImageContent(
    data: String,
    mimeType: String,
    annotations: Option[Annotations] = None,
    _meta: Option[Json] = None
) extends Content:
  def `type`: ContentType = ContentType.Image

/** Audio content (base64 encoded)
  * Spec ref: schema.ts AudioContent
  */
final case class AudioContent(
    data: String,
    mimeType: String,
    annotations: Option[Annotations] = None,
    _meta: Option[Json] = None
) extends Content:
  def `type`: ContentType = ContentType.Audio

/** Embedded resource content
  * Spec ref: schema.ts EmbeddedResource
  */
final case class ResourceContentRef(
    uri: String,
    mimeType: Option[String] = None,
    text: Option[String] = None,
    annotations: Option[Annotations] = None,
    _meta: Option[Json] = None
) extends Content:
  def `type`: ContentType = ContentType.Resource

/** Link to a resource without embedding
  * Spec ref: schema.ts ResourceLink
  */
final case class ResourceLinkContent(
    uri: String,
    name: String,
    title: Option[String] = None,
    description: Option[String] = None,
    mimeType: Option[String] = None,
    annotations: Option[Annotations] = None,
    size: Option[Long] = None,
    icons: Option[List[Icon]] = None
) extends Content:
  def `type`: ContentType = ContentType.ResourceLink

/** Result of a tool call
  * Spec ref: schema.ts CallToolResult
  */
final case class ToolResult(
    content: List[Content],
    isError: Boolean = false,
    structuredContent: Option[Json] = None
):
  /** Extract the first text content, if any */
  def asText: Option[String] =
    content.collectFirst { case TextContent(text, _, _) => text }

  /** Extract text content or fallback to toString */
  def textContent: String = asText.getOrElse(toString)

  /** Extract the first image content, if any */
  def asImage: Option[ImageContent] =
    content.collectFirst { case i: ImageContent => i }

  /** Extract the first audio content, if any */
  def asAudio: Option[AudioContent] =
    content.collectFirst { case a: AudioContent => a }

object ToolResult:
  /** Create a text result */
  def text(s: String): ToolResult =
    ToolResult(List(TextContent(s)))

  /** Alias for text */
  def ok(s: String): ToolResult = text(s)

  /** Create a JSON result by encoding the value */
  def json[A: Encoder](value: A): ToolResult =
    ToolResult(List(TextContent(value.asJson.noSpaces)))

  /** Create an error result */
  def error(message: String): ToolResult =
    ToolResult(List(TextContent(message)), isError = true)

  /** Create an image result from raw bytes */
  def image(data: Array[Byte], mimeType: String): ToolResult =
    ToolResult(List(ImageContent(Base64.getEncoder.encodeToString(data), mimeType)))

// === Resources ===
// Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/server/resources

/** Resource definition
  * Spec ref: schema.ts Resource
  */
final case class Resource(
    uri: String,
    name: String,
    description: Option[String] = None,
    mimeType: Option[String] = None,
    title: Option[String] = None,
    annotations: Option[Annotations] = None,
    size: Option[Long] = None,
    icons: Option[List[Icon]] = None,
    _meta: Option[Json] = None
)

/** Resource template for dynamic resources
  * Spec ref: schema.ts ResourceTemplate
  */
final case class ResourceTemplate(
    uriTemplate: String,
    name: String,
    description: Option[String] = None,
    mimeType: Option[String] = None,
    title: Option[String] = None,
    annotations: Option[Annotations] = None,
    icons: Option[List[Icon]] = None,
    _meta: Option[Json] = None
)

/** Content of a resource
  * Spec ref: schema.ts TextResourceContents | BlobResourceContents
  */
final case class ResourceContent(
    uri: String,
    mimeType: Option[String] = None,
    text: Option[String] = None,
    blob: Option[String] = None
)

object ResourceContent:
  /** Create a text resource */
  def text(uri: String, content: String, mimeType: Option[String] = None): ResourceContent =
    ResourceContent(uri, mimeType.orElse(Some("text/plain")), Some(content), None)

  /** Create a JSON resource by encoding the value */
  def json[A: Encoder](uri: String, value: A): ResourceContent =
    ResourceContent(uri, Some("application/json"), Some(value.asJson.noSpaces), None)

  /** Create a blob resource from a base64-encoded string */
  def blob(uri: String, base64: String, mimeType: Option[String] = None): ResourceContent =
    ResourceContent(uri, mimeType, None, Some(base64))

  /** Create a binary resource from raw bytes */
  def binary(uri: String, data: Array[Byte], mimeType: String): ResourceContent =
    ResourceContent(uri, Some(mimeType), None, Some(Base64.getEncoder.encodeToString(data)))

// === Prompts ===
// Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/server/prompts

/** Prompt argument definition
  * Spec ref: schema.ts PromptArgument
  */
final case class PromptArgument(
    name: String,
    description: Option[String] = None,
    required: Boolean = false,
    title: Option[String] = None
)

/** Prompt definition
  * Spec ref: schema.ts Prompt
  */
final case class Prompt(
    name: String,
    description: Option[String] = None,
    arguments: List[PromptArgument] = Nil,
    title: Option[String] = None,
    icons: Option[List[Icon]] = None,
    _meta: Option[Json] = None
)

/** Role in prompt messages */
enum Role:
  case User, Assistant

/** Prompt message
  * Spec ref: schema.ts PromptMessage
  */
final case class PromptMessage(
    role: Role,
    content: Content
)

/** Result of getting a prompt
  * Spec ref: schema.ts GetPromptResult
  */
final case class GetPromptResult(
    description: Option[String] = None,
    messages: List[PromptMessage]
)

// === Lifecycle ===
// Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/basic/lifecycle

/** Initialize request params
  * Spec ref: schema.ts InitializeRequest params
  */
final case class InitializeParams(
    protocolVersion: String,
    capabilities: ClientCapabilities,
    clientInfo: ClientInfo
)

/** Initialize response result
  * Spec ref: schema.ts InitializeResult
  */
final case class InitializeResult(
    protocolVersion: String,
    capabilities: ServerCapabilities,
    serverInfo: ServerInfo,
    instructions: Option[String] = None
)

// === Logging ===

enum LogLevel:
  case Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency

final case class LogMessage(
    level: LogLevel,
    logger: Option[String] = None,
    data: Json
)

// === Pagination ===

final case class Cursor(value: String) extends AnyVal

// === Notifications ===

/** Parameters for cancellation notification
  * Spec ref: schema.ts CancelledNotification
  */
final case class CancelledParams(
    requestId: RequestId,
    reason: Option[String] = None
)

/** Parameters for progress notification
  * Spec ref: schema.ts ProgressNotification
  */
final case class ProgressParams(
    progressToken: RequestId,
    progress: Double,
    total: Option[Double] = None
)

// === Roots (Client Feature) ===
// Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/client/roots

/** Root represents an accessible filesystem boundary
  * Spec ref: schema.ts Root interface
  */
final case class Root(
    uri: String,                    // MUST be file:// URI
    name: Option[String] = None,    // Human-readable display name
    _meta: Option[Json] = None
)

/** Result of listing roots
  * Spec ref: schema.ts ListRootsResult
  */
final case class ListRootsResult(
    roots: List[Root]
)

// === MCP Method Names ===
// Spec ref: schema.ts method constants

object McpMethod:
  // Lifecycle
  val Initialize: String  = "initialize"
  val Initialized: String = "notifications/initialized"
  val Shutdown: String    = "shutdown"
  val Ping: String        = "ping"

  // Tools
  val ToolsList: String        = "tools/list"
  val ToolsCall: String        = "tools/call"
  val ToolsListChanged: String = "notifications/tools/list_changed"

  // Resources
  val ResourcesList: String          = "resources/list"
  val ResourcesRead: String          = "resources/read"
  val ResourcesTemplatesList: String = "resources/templates/list"
  val ResourcesSubscribe: String     = "resources/subscribe"
  val ResourcesUnsubscribe: String   = "resources/unsubscribe"
  val ResourcesListChanged: String   = "notifications/resources/list_changed"
  val ResourcesUpdated: String       = "notifications/resources/updated"

  // Prompts
  val PromptsList: String        = "prompts/list"
  val PromptsGet: String         = "prompts/get"
  val PromptsListChanged: String = "notifications/prompts/list_changed"

  // Logging
  val LoggingSetLevel: String = "logging/setLevel"
  val LoggingMessage: String  = "notifications/message"

  // Completions
  val CompletionComplete: String = "completion/complete"

  // Tasks
  val TasksGet: String      = "tasks/get"
  val TasksResult: String   = "tasks/result"
  val TasksCancel: String   = "tasks/cancel"
  val TasksList: String     = "tasks/list"
  val TasksStatus: String   = "notifications/tasks/status"

  // Sampling (client feature)
  val SamplingCreateMessage: String = "sampling/createMessage"

  // Elicitation (client feature)
  val ElicitationCreate: String   = "elicitation/create"
  val ElicitationComplete: String = "notifications/elicitation/complete"

  // Roots (client feature)
  val RootsList: String        = "roots/list"
  val RootsListChanged: String = "notifications/roots/list_changed"

  // Utilities
  val Cancelled: String = "notifications/cancelled"
  val Progress: String  = "notifications/progress"
