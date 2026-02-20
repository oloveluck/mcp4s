package mcp4s.protocol

import io.circe.*
import io.circe.syntax.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import mcp4s.protocol.Codecs.given

class PropertySpec extends ScalaCheckSuite:

  // ============================================================================
  // JSON-RPC Core Generators
  // ============================================================================

  val genStringId: Gen[RequestId] = Gen.alphaNumStr.map(RequestId.StringId.apply)

  val genNumberId: Gen[RequestId] = Gen.long.map(RequestId.NumberId.apply)

  val genNullId: Gen[RequestId] = Gen.const(RequestId.NullId)

  // All request IDs including NullId (for standalone RequestId tests)
  given Arbitrary[RequestId] = Arbitrary(
    Gen.oneOf(genStringId, genNumberId, genNullId)
  )

  // Non-null request IDs (for message types where NullId may not roundtrip)
  val genNonNullRequestId: Gen[RequestId] = Gen.oneOf(genStringId, genNumberId)

  val genMethod: Gen[String] = Gen.oneOf(
    "initialize",
    "tools/list",
    "tools/call",
    "resources/list",
    "resources/read",
    "prompts/list",
    "prompts/get",
    "notifications/initialized",
    "notifications/cancelled"
  )

  // Json.Null excluded since Some(Json.Null) doesn't roundtrip (becomes None)
  val genJsonPrimitive: Gen[Json] = Gen.oneOf(
    Gen.alphaNumStr.map(Json.fromString),
    Gen.long.map(Json.fromLong),
    Gen.choose(-1000.0, 1000.0).map(Json.fromDoubleOrNull),
    Gen.const(Json.True),
    Gen.const(Json.False)
  )

  val genJsonObject: Gen[Json] = for {
    keys <- Gen.listOfN(3, Gen.alphaStr.suchThat(_.nonEmpty))
    values <- Gen.listOfN(3, genJsonPrimitive)
  } yield Json.obj(keys.zip(values)*)

  val genJsonRpcRequest: Gen[JsonRpcRequest] = for {
    id <- genNonNullRequestId
    method <- genMethod
    params <- Gen.option(genJsonObject)
  } yield JsonRpcRequest(id, method, params)

  given Arbitrary[JsonRpcRequest] = Arbitrary(genJsonRpcRequest)

  val genJsonRpcNotification: Gen[JsonRpcNotification] = for {
    method <- genMethod
    params <- Gen.option(genJsonObject)
  } yield JsonRpcNotification(method, params)

  given Arbitrary[JsonRpcNotification] = Arbitrary(genJsonRpcNotification)

  val genJsonRpcResponse: Gen[JsonRpcResponse] = for {
    id <- genNonNullRequestId
    result <- genJsonObject
  } yield JsonRpcResponse(id, result)

  given Arbitrary[JsonRpcResponse] = Arbitrary(genJsonRpcResponse)

  val genJsonRpcError: Gen[JsonRpcError] = for {
    code <- Gen.oneOf(-32700, -32600, -32601, -32602, -32603)
    message <- Gen.alphaNumStr
    data <- Gen.option(genJsonPrimitive)
  } yield JsonRpcError(code, message, data)

  given Arbitrary[JsonRpcError] = Arbitrary(genJsonRpcError)

  val genJsonRpcErrorResponse: Gen[JsonRpcErrorResponse] = for {
    id <- genNonNullRequestId
    error <- genJsonRpcError
  } yield JsonRpcErrorResponse(id, error)

  given Arbitrary[JsonRpcErrorResponse] = Arbitrary(genJsonRpcErrorResponse)

  // ============================================================================
  // Common Types Generators
  // ============================================================================

  val genRole: Gen[Role] = Gen.oneOf(Role.User, Role.Assistant)

  given Arbitrary[Role] = Arbitrary(genRole)

  val genLogLevel: Gen[LogLevel] = Gen.oneOf(
    LogLevel.Debug,
    LogLevel.Info,
    LogLevel.Notice,
    LogLevel.Warning,
    LogLevel.Error,
    LogLevel.Critical,
    LogLevel.Alert,
    LogLevel.Emergency
  )

  given Arbitrary[LogLevel] = Arbitrary(genLogLevel)

  val genCursor: Gen[Cursor] = Gen.alphaNumStr.map(Cursor.apply)

  given Arbitrary[Cursor] = Arbitrary(genCursor)

  // ============================================================================
  // Icon and Annotations Generators
  // ============================================================================

  val genIcon: Gen[Icon] = for {
    src <- Gen.alphaNumStr.map(s => s"https://example.com/$s.png")
    mimeType <- Gen.option(Gen.oneOf("image/png", "image/svg+xml", "image/jpeg"))
    sizes <- Gen.option(Gen.listOfN(2, Gen.oneOf("16x16", "32x32", "64x64")))
    theme <- Gen.option(Gen.oneOf("light", "dark"))
  } yield Icon(src, mimeType, sizes, theme)

  given Arbitrary[Icon] = Arbitrary(genIcon)

  val genAnnotations: Gen[Annotations] = for {
    audience <- Gen.option(Gen.listOfN(2, Gen.oneOf("user", "assistant")))
    priority <- Gen.option(Gen.choose(0.0, 1.0))
    lastModified <- Gen.option(Gen.const("2024-01-15T10:30:00Z"))
  } yield Annotations(audience, priority, lastModified)

  given Arbitrary[Annotations] = Arbitrary(genAnnotations)

  // ============================================================================
  // Content Types Generators (Phase 1)
  // ============================================================================

  val genTextContent: Gen[TextContent] = for {
    text <- Gen.alphaNumStr
    annotations <- Gen.option(genAnnotations)
    meta <- Gen.option(genJsonObject)
  } yield TextContent(text, annotations, meta)

  given Arbitrary[TextContent] = Arbitrary(genTextContent)

  val genImageContent: Gen[ImageContent] = for {
    data <- Gen.alphaNumStr.filter(_.nonEmpty)
    mimeType <- Gen.oneOf("image/png", "image/jpeg", "image/gif", "image/webp")
    annotations <- Gen.option(genAnnotations)
    meta <- Gen.option(genJsonObject)
  } yield ImageContent(data, mimeType, annotations, meta)

  given Arbitrary[ImageContent] = Arbitrary(genImageContent)

  val genAudioContent: Gen[AudioContent] = for {
    data <- Gen.alphaNumStr.filter(_.nonEmpty)
    mimeType <- Gen.oneOf("audio/wav", "audio/mp3", "audio/ogg", "audio/mpeg")
    annotations <- Gen.option(genAnnotations)
    meta <- Gen.option(genJsonObject)
  } yield AudioContent(data, mimeType, annotations, meta)

  given Arbitrary[AudioContent] = Arbitrary(genAudioContent)

  val genResourceContentRef: Gen[ResourceContentRef] = for {
    uri <- Gen.alphaNumStr.map(s => s"file:///path/to/$s")
    mimeType <- Gen.option(Gen.oneOf("text/plain", "application/json", "text/markdown"))
    text <- Gen.option(Gen.alphaNumStr)
    annotations <- Gen.option(genAnnotations)
    meta <- Gen.option(genJsonObject)
  } yield ResourceContentRef(uri, mimeType, text, annotations, meta)

  given Arbitrary[ResourceContentRef] = Arbitrary(genResourceContentRef)

  val genResourceLinkContent: Gen[ResourceLinkContent] = for {
    uri <- Gen.alphaNumStr.map(s => s"file:///path/to/$s")
    name <- Gen.alphaNumStr.filter(_.nonEmpty)
    title <- Gen.option(Gen.alphaNumStr)
    description <- Gen.option(Gen.alphaNumStr)
    mimeType <- Gen.option(Gen.oneOf("text/plain", "application/json"))
    annotations <- Gen.option(genAnnotations)
    size <- Gen.option(Gen.choose(0L, 1000000L))
    icons <- Gen.option(Gen.listOfN(2, genIcon))
  } yield ResourceLinkContent(uri, name, title, description, mimeType, annotations, size, icons)

  given Arbitrary[ResourceLinkContent] = Arbitrary(genResourceLinkContent)

  val genContent: Gen[Content] = Gen.oneOf(
    genTextContent,
    genImageContent,
    genAudioContent,
    genResourceContentRef,
    genResourceLinkContent
  )

  given Arbitrary[Content] = Arbitrary(genContent)

  // ============================================================================
  // Sampling Types Generators (Phase 2)
  // ============================================================================

  val genSamplingTextContent: Gen[SamplingTextContent] =
    Gen.alphaNumStr.map(SamplingTextContent(_))

  given Arbitrary[SamplingTextContent] = Arbitrary(genSamplingTextContent)

  val genSamplingImageContent: Gen[SamplingImageContent] = for {
    data <- Gen.alphaNumStr.filter(_.nonEmpty)
    mimeType <- Gen.oneOf("image/png", "image/jpeg", "image/gif")
  } yield SamplingImageContent(data, mimeType)

  given Arbitrary[SamplingImageContent] = Arbitrary(genSamplingImageContent)

  val genSamplingAudioContent: Gen[SamplingAudioContent] = for {
    data <- Gen.alphaNumStr.filter(_.nonEmpty)
    mimeType <- Gen.oneOf("audio/wav", "audio/mp3", "audio/ogg")
  } yield SamplingAudioContent(data, mimeType)

  given Arbitrary[SamplingAudioContent] = Arbitrary(genSamplingAudioContent)

  val genToolUseContent: Gen[ToolUseContent] = for {
    id <- Gen.alphaNumStr.filter(_.nonEmpty)
    name <- Gen.alphaNumStr.filter(_.nonEmpty)
    input <- genJsonObject
  } yield ToolUseContent(id, name, input)

  given Arbitrary[ToolUseContent] = Arbitrary(genToolUseContent)

  val genToolResultContent: Gen[ToolResultContent] = for {
    toolUseId <- Gen.alphaNumStr.filter(_.nonEmpty)
    content <- Gen.listOfN(2, genTextContent.map(identity[Content]))
    isError <- Gen.oneOf(true, false)
  } yield ToolResultContent(toolUseId, content, isError)

  given Arbitrary[ToolResultContent] = Arbitrary(genToolResultContent)

  val genSamplingContent: Gen[SamplingContent] = Gen.oneOf(
    genSamplingTextContent,
    genSamplingImageContent,
    genSamplingAudioContent,
    genToolUseContent,
    genToolResultContent
  )

  given Arbitrary[SamplingContent] = Arbitrary(genSamplingContent)

  val genSamplingMessage: Gen[SamplingMessage] = for {
    role <- genRole
    content <- genSamplingContent
    meta <- Gen.option(genJsonObject)
  } yield SamplingMessage(role, content, meta)

  given Arbitrary[SamplingMessage] = Arbitrary(genSamplingMessage)

  val genModelHint: Gen[ModelHint] = for {
    name <- Gen.option(Gen.oneOf("claude-3-opus", "claude-3-sonnet", "gpt-4"))
  } yield ModelHint(name)

  given Arbitrary[ModelHint] = Arbitrary(genModelHint)

  val genModelPreferences: Gen[ModelPreferences] = for {
    hints <- Gen.option(Gen.listOfN(2, genModelHint))
    costPriority <- Gen.option(Gen.choose(0.0, 1.0))
    speedPriority <- Gen.option(Gen.choose(0.0, 1.0))
    intelligencePriority <- Gen.option(Gen.choose(0.0, 1.0))
  } yield ModelPreferences(hints, costPriority, speedPriority, intelligencePriority)

  given Arbitrary[ModelPreferences] = Arbitrary(genModelPreferences)

  val genToolChoice: Gen[ToolChoice] = Gen.oneOf(
    Gen.const(ToolChoice.Auto),
    Gen.const(ToolChoice.None),
    Gen.alphaNumStr.filter(_.nonEmpty).map(ToolChoice.Specific(_))
  )

  given Arbitrary[ToolChoice] = Arbitrary(genToolChoice)

  // Simplified CreateMessageParams generator (without nested Tool to avoid cycles)
  val genCreateMessageParams: Gen[CreateMessageParams] = for {
    messages <- Gen.listOfN(2, genSamplingMessage)
    maxTokens <- Gen.choose(100, 4096)
    modelPreferences <- Gen.option(genModelPreferences)
    systemPrompt <- Gen.option(Gen.alphaNumStr)
    includeContext <- Gen.option(Gen.oneOf("none", "thisServer", "allServers"))
    temperature <- Gen.option(Gen.choose(0.0, 2.0))
    stopSequences <- Gen.option(Gen.listOfN(2, Gen.alphaNumStr))
    metadata <- Gen.option(genJsonObject)
    toolChoice <- Gen.option(genToolChoice)
  } yield CreateMessageParams(messages, maxTokens, modelPreferences, systemPrompt, includeContext, temperature, stopSequences, metadata, None, toolChoice)

  given Arbitrary[CreateMessageParams] = Arbitrary(genCreateMessageParams)

  val genCreateMessageResult: Gen[CreateMessageResult] = for {
    role <- genRole
    content <- genSamplingContent
    model <- Gen.oneOf("claude-3-opus-20240229", "claude-3-sonnet-20240229", "gpt-4-turbo")
    stopReason <- Gen.option(Gen.oneOf("endTurn", "stopSequence", "maxTokens", "toolUse"))
    meta <- Gen.option(genJsonObject)
  } yield CreateMessageResult(role, content, model, stopReason, meta)

  given Arbitrary[CreateMessageResult] = Arbitrary(genCreateMessageResult)

  // ============================================================================
  // Elicitation Types Generators (Phase 3)
  // ============================================================================

  val genElicitAction: Gen[ElicitAction] = Gen.oneOf(
    ElicitAction.Accept,
    ElicitAction.Decline,
    ElicitAction.Cancel
  )

  given Arbitrary[ElicitAction] = Arbitrary(genElicitAction)

  val genElicitResult: Gen[ElicitResult] = for {
    action <- genElicitAction
    content <- Gen.option(for {
      key <- Gen.alphaStr.filter(_.nonEmpty)
      value <- genJsonPrimitive
    } yield Map(key -> value))
  } yield ElicitResult(action, content)

  given Arbitrary[ElicitResult] = Arbitrary(genElicitResult)

  // ============================================================================
  // Capability Types Generators (Phase 4)
  // ============================================================================

  val genToolsCapability: Gen[ToolsCapability] = for {
    listChanged <- Gen.option(Gen.oneOf(true, false))
  } yield ToolsCapability(listChanged)

  given Arbitrary[ToolsCapability] = Arbitrary(genToolsCapability)

  val genResourcesCapability: Gen[ResourcesCapability] = for {
    subscribe <- Gen.option(Gen.oneOf(true, false))
    listChanged <- Gen.option(Gen.oneOf(true, false))
  } yield ResourcesCapability(subscribe, listChanged)

  given Arbitrary[ResourcesCapability] = Arbitrary(genResourcesCapability)

  val genPromptsCapability: Gen[PromptsCapability] = for {
    listChanged <- Gen.option(Gen.oneOf(true, false))
  } yield PromptsCapability(listChanged)

  given Arbitrary[PromptsCapability] = Arbitrary(genPromptsCapability)

  val genLoggingCapability: Gen[LoggingCapability] = Gen.const(LoggingCapability())

  given Arbitrary[LoggingCapability] = Arbitrary(genLoggingCapability)

  val genCompletionsCapability: Gen[CompletionsCapability] = Gen.const(CompletionsCapability())

  given Arbitrary[CompletionsCapability] = Arbitrary(genCompletionsCapability)

  val genServerCapabilities: Gen[ServerCapabilities] = for {
    tools <- Gen.option(genToolsCapability)
    resources <- Gen.option(genResourcesCapability)
    prompts <- Gen.option(genPromptsCapability)
    logging <- Gen.option(genLoggingCapability)
    completions <- Gen.option(genCompletionsCapability)
    experimental <- Gen.option(genJsonObject)
  } yield ServerCapabilities(tools, resources, prompts, logging, completions, None, experimental)

  given Arbitrary[ServerCapabilities] = Arbitrary(genServerCapabilities)

  val genRootsCapability: Gen[RootsCapability] = for {
    listChanged <- Gen.option(Gen.oneOf(true, false))
  } yield RootsCapability(listChanged)

  given Arbitrary[RootsCapability] = Arbitrary(genRootsCapability)

  val genSamplingCapability: Gen[SamplingCapability] = for {
    context <- Gen.option(genJsonObject)
    tools <- Gen.option(genJsonObject)
  } yield SamplingCapability(context, tools)

  given Arbitrary[SamplingCapability] = Arbitrary(genSamplingCapability)

  val genElicitationCapability: Gen[ElicitationCapability] = for {
    form <- Gen.option(genJsonObject)
    url <- Gen.option(genJsonObject)
  } yield ElicitationCapability(form, url)

  given Arbitrary[ElicitationCapability] = Arbitrary(genElicitationCapability)

  val genClientCapabilities: Gen[ClientCapabilities] = for {
    roots <- Gen.option(genRootsCapability)
    sampling <- Gen.option(genSamplingCapability)
    elicitation <- Gen.option(genElicitationCapability)
    experimental <- Gen.option(genJsonObject)
  } yield ClientCapabilities(roots, sampling, elicitation, None, experimental)

  given Arbitrary[ClientCapabilities] = Arbitrary(genClientCapabilities)

  // ============================================================================
  // JsonSchema Types Generators (Phase 5)
  // ============================================================================

  val genJsonSchemaProperty: Gen[JsonSchemaProperty] = for {
    typ <- Gen.oneOf("string", "number", "boolean", "integer", "array")
    description <- Gen.option(Gen.alphaNumStr)
    enumVals <- Gen.option(Gen.listOfN(3, Gen.alphaStr.filter(_.nonEmpty)))
    default <- Gen.option(genJsonPrimitive)
  } yield JsonSchemaProperty.make(typ, description, enumVals, default)

  given Arbitrary[JsonSchemaProperty] = Arbitrary(genJsonSchemaProperty)

  val genJsonSchema: Gen[JsonSchema] = for {
    properties <- Gen.option(for {
      keys <- Gen.listOfN(3, Gen.alphaStr.filter(_.nonEmpty))
      props <- Gen.listOfN(3, genJsonSchemaProperty)
    } yield keys.zip(props).toMap)
    required <- Gen.option(Gen.listOfN(2, Gen.alphaStr.filter(_.nonEmpty)))
  } yield JsonSchema("object", properties, required)

  given Arbitrary[JsonSchema] = Arbitrary(genJsonSchema)

  // ============================================================================
  // Resource & Prompt Types Generators (Phase 6)
  // ============================================================================

  val genResource: Gen[Resource] = for {
    uri <- Gen.alphaNumStr.map(s => s"file:///resource/$s")
    name <- Gen.alphaNumStr.filter(_.nonEmpty)
    description <- Gen.option(Gen.alphaNumStr)
    mimeType <- Gen.option(Gen.oneOf("text/plain", "application/json", "text/markdown"))
    title <- Gen.option(Gen.alphaNumStr)
    annotations <- Gen.option(genAnnotations)
    size <- Gen.option(Gen.choose(0L, 1000000L))
    icons <- Gen.option(Gen.listOfN(2, genIcon))
    meta <- Gen.option(genJsonObject)
  } yield Resource(uri, name, description, mimeType, title, annotations, size, icons, meta)

  given Arbitrary[Resource] = Arbitrary(genResource)

  val genResourceTemplate: Gen[ResourceTemplate] = for {
    uriTemplate <- Gen.alphaNumStr.map(s => s"file:///template/{$s}")
    name <- Gen.alphaNumStr.filter(_.nonEmpty)
    description <- Gen.option(Gen.alphaNumStr)
    mimeType <- Gen.option(Gen.oneOf("text/plain", "application/json"))
    title <- Gen.option(Gen.alphaNumStr)
    annotations <- Gen.option(genAnnotations)
    icons <- Gen.option(Gen.listOfN(2, genIcon))
    meta <- Gen.option(genJsonObject)
  } yield ResourceTemplate(uriTemplate, name, description, mimeType, title, annotations, icons, meta)

  given Arbitrary[ResourceTemplate] = Arbitrary(genResourceTemplate)

  val genResourceContent: Gen[ResourceContent] = for {
    uri <- Gen.alphaNumStr.map(s => s"file:///content/$s")
    mimeType <- Gen.option(Gen.oneOf("text/plain", "application/json"))
    text <- Gen.option(Gen.alphaNumStr)
    blob <- Gen.option(Gen.alphaNumStr.map(s => java.util.Base64.getEncoder.encodeToString(s.getBytes)))
  } yield ResourceContent(uri, mimeType, text, blob)

  given Arbitrary[ResourceContent] = Arbitrary(genResourceContent)

  val genPromptArgument: Gen[PromptArgument] = for {
    name <- Gen.alphaStr.filter(_.nonEmpty)
    description <- Gen.option(Gen.alphaNumStr)
    required <- Gen.oneOf(true, false)
    title <- Gen.option(Gen.alphaNumStr)
  } yield PromptArgument(name, description, required, title)

  given Arbitrary[PromptArgument] = Arbitrary(genPromptArgument)

  val genPrompt: Gen[Prompt] = for {
    name <- Gen.alphaStr.filter(_.nonEmpty)
    description <- Gen.option(Gen.alphaNumStr)
    arguments <- Gen.listOfN(2, genPromptArgument)
    title <- Gen.option(Gen.alphaNumStr)
    icons <- Gen.option(Gen.listOfN(2, genIcon))
    meta <- Gen.option(genJsonObject)
  } yield Prompt(name, description, arguments, title, icons, meta)

  given Arbitrary[Prompt] = Arbitrary(genPrompt)

  val genPromptMessage: Gen[PromptMessage] = for {
    role <- genRole
    content <- genContent
  } yield PromptMessage(role, content)

  given Arbitrary[PromptMessage] = Arbitrary(genPromptMessage)

  val genGetPromptResult: Gen[GetPromptResult] = for {
    description <- Gen.option(Gen.alphaNumStr)
    messages <- Gen.listOfN(2, genPromptMessage)
  } yield GetPromptResult(description, messages)

  given Arbitrary[GetPromptResult] = Arbitrary(genGetPromptResult)

  val genRoot: Gen[Root] = for {
    uri <- Gen.alphaNumStr.map(s => s"file:///root/$s")
    name <- Gen.option(Gen.alphaNumStr)
    meta <- Gen.option(genJsonObject)
  } yield Root(uri, name, meta)

  given Arbitrary[Root] = Arbitrary(genRoot)

  val genListRootsResult: Gen[ListRootsResult] = for {
    roots <- Gen.listOfN(2, genRoot)
  } yield ListRootsResult(roots)

  given Arbitrary[ListRootsResult] = Arbitrary(genListRootsResult)

  // ============================================================================
  // Lifecycle & Misc Types Generators (Phase 7)
  // ============================================================================

  val genServerInfo: Gen[ServerInfo] = for {
    name <- Gen.alphaStr.filter(_.nonEmpty)
    version <- Gen.oneOf("1.0.0", "2.0.0", "0.1.0")
    title <- Gen.option(Gen.alphaNumStr)
    description <- Gen.option(Gen.alphaNumStr)
    websiteUrl <- Gen.option(Gen.const("https://example.com"))
    icons <- Gen.option(Gen.listOfN(2, genIcon))
  } yield ServerInfo(name, version, title, description, websiteUrl, icons)

  given Arbitrary[ServerInfo] = Arbitrary(genServerInfo)

  val genClientInfo: Gen[ClientInfo] = for {
    name <- Gen.alphaStr.filter(_.nonEmpty)
    version <- Gen.oneOf("1.0.0", "2.0.0", "0.1.0")
    title <- Gen.option(Gen.alphaNumStr)
    description <- Gen.option(Gen.alphaNumStr)
    websiteUrl <- Gen.option(Gen.const("https://example.com"))
    icons <- Gen.option(Gen.listOfN(2, genIcon))
  } yield ClientInfo(name, version, title, description, websiteUrl, icons)

  given Arbitrary[ClientInfo] = Arbitrary(genClientInfo)

  val genImplementation: Gen[Implementation] = for {
    name <- Gen.alphaStr.filter(_.nonEmpty)
    version <- Gen.oneOf("1.0.0", "2.0.0", "0.1.0")
    title <- Gen.option(Gen.alphaNumStr)
    description <- Gen.option(Gen.alphaNumStr)
    websiteUrl <- Gen.option(Gen.const("https://example.com"))
    icons <- Gen.option(Gen.listOfN(2, genIcon))
  } yield Implementation(name, version, title, description, websiteUrl, icons)

  given Arbitrary[Implementation] = Arbitrary(genImplementation)

  val genInitializeParams: Gen[InitializeParams] = for {
    protocolVersion <- Gen.oneOf("2024-11-05", "2025-03-26")
    capabilities <- genClientCapabilities
    clientInfo <- genClientInfo
  } yield InitializeParams(protocolVersion, capabilities, clientInfo)

  given Arbitrary[InitializeParams] = Arbitrary(genInitializeParams)

  val genInitializeResult: Gen[InitializeResult] = for {
    protocolVersion <- Gen.oneOf("2024-11-05", "2025-03-26")
    capabilities <- genServerCapabilities
    serverInfo <- genServerInfo
    instructions <- Gen.option(Gen.alphaNumStr)
  } yield InitializeResult(protocolVersion, capabilities, serverInfo, instructions)

  given Arbitrary[InitializeResult] = Arbitrary(genInitializeResult)

  val genProgressParams: Gen[ProgressParams] = for {
    progressToken <- genNonNullRequestId
    progress <- Gen.choose(0.0, 100.0)
    total <- Gen.option(Gen.choose(0.0, 100.0))
  } yield ProgressParams(progressToken, progress, total)

  given Arbitrary[ProgressParams] = Arbitrary(genProgressParams)

  val genCancelledParams: Gen[CancelledParams] = for {
    requestId <- genNonNullRequestId
    reason <- Gen.option(Gen.alphaNumStr)
  } yield CancelledParams(requestId, reason)

  given Arbitrary[CancelledParams] = Arbitrary(genCancelledParams)

  val genLogMessage: Gen[LogMessage] = for {
    level <- genLogLevel
    logger <- Gen.option(Gen.alphaStr.filter(_.nonEmpty))
    data <- genJsonPrimitive
  } yield LogMessage(level, logger, data)

  given Arbitrary[LogMessage] = Arbitrary(genLogMessage)

  val genToolAnnotations: Gen[ToolAnnotations] = for {
    title <- Gen.option(Gen.alphaNumStr)
    readOnlyHint <- Gen.option(Gen.oneOf(true, false))
    destructiveHint <- Gen.option(Gen.oneOf(true, false))
    idempotentHint <- Gen.option(Gen.oneOf(true, false))
    openWorldHint <- Gen.option(Gen.oneOf(true, false))
  } yield ToolAnnotations(title, readOnlyHint, destructiveHint, idempotentHint, openWorldHint)

  given Arbitrary[ToolAnnotations] = Arbitrary(genToolAnnotations)

  val genTaskSupport: Gen[TaskSupport] = Gen.oneOf(
    TaskSupport.Forbidden,
    TaskSupport.Optional,
    TaskSupport.Required
  )

  given Arbitrary[TaskSupport] = Arbitrary(genTaskSupport)

  val genToolExecution: Gen[ToolExecution] = for {
    taskSupport <- Gen.option(genTaskSupport)
  } yield ToolExecution(taskSupport)

  given Arbitrary[ToolExecution] = Arbitrary(genToolExecution)

  val genTool: Gen[Tool] = for {
    name <- Gen.alphaStr.filter(_.nonEmpty)
    description <- Gen.option(Gen.alphaNumStr)
    inputSchema <- genJsonSchema
    title <- Gen.option(Gen.alphaNumStr)
    outputSchema <- Gen.option(genJsonSchema)
    execution <- Gen.option(genToolExecution)
    annotations <- Gen.option(genToolAnnotations)
    icons <- Gen.option(Gen.listOfN(2, genIcon))
    meta <- Gen.option(genJsonObject)
  } yield Tool(name, description, inputSchema, title, outputSchema, execution, annotations, icons, meta)

  given Arbitrary[Tool] = Arbitrary(genTool)

  val genToolResult: Gen[ToolResult] = for {
    content <- Gen.listOfN(2, genContent)
    isError <- Gen.oneOf(true, false)
    structuredContent <- Gen.option(genJsonObject)
  } yield ToolResult(content, Some(isError), structuredContent)

  given Arbitrary[ToolResult] = Arbitrary(genToolResult)

  // ============================================================================
  // McpError Generators (Phase 8)
  // ============================================================================

  val genMcpError: Gen[McpError] = Gen.oneOf(
    Gen.alphaStr.filter(_.nonEmpty).map(McpError.ToolNotFound(_)),
    Gen.alphaStr.filter(_.nonEmpty).map(McpError.ResourceNotFound(_)),
    Gen.alphaStr.filter(_.nonEmpty).map(McpError.PromptNotFound(_)),
    for {
      name <- Gen.alphaStr.filter(_.nonEmpty)
      reason <- Gen.alphaStr.filter(_.nonEmpty)
    } yield McpError.InvalidToolArguments(name, reason),
    for {
      name <- Gen.alphaStr.filter(_.nonEmpty)
      reason <- Gen.alphaStr.filter(_.nonEmpty)
    } yield McpError.InvalidPromptArguments(name, reason),
    for {
      requested <- Gen.oneOf("2024-11-05", "2025-03-26")
      supported <- Gen.oneOf("2024-11-05", "2025-03-26")
    } yield McpError.ProtocolVersionMismatch(requested, supported),
    Gen.const(McpError.NotInitialized()),
    Gen.const(McpError.AlreadyInitialized()),
    Gen.alphaStr.filter(_.nonEmpty).map(McpError.InternalError(_)),
    Gen.alphaStr.filter(_.nonEmpty).map(McpError.MethodNotFound(_)),
    Gen.alphaStr.filter(_.nonEmpty).map(McpError.MethodNotSupported(_)),
    genNonNullRequestId.map(McpError.RequestCancelled(_)),
    Gen.alphaStr.filter(_.nonEmpty).map(McpError.CapabilityNotSupported(_)),
    Gen.const(McpError.SamplingNotSupported()),
    Gen.const(McpError.ElicitationNotSupported())
  )

  given Arbitrary[McpError] = Arbitrary(genMcpError)

  // ============================================================================
  // JSON-RPC Property Tests
  // ============================================================================

  property("RequestId serialization roundtrip") {
    forAll { (id: RequestId) =>
      val json = id.asJson
      val decoded = json.as[RequestId]
      decoded == Right(id)
    }
  }

  property("RequestId.StringId encodes to JSON string") {
    forAll(Gen.alphaNumStr) { (s: String) =>
      val id = RequestId.StringId(s)
      id.asJson == Json.fromString(s)
    }
  }

  property("RequestId.NumberId encodes to JSON number") {
    forAll { (n: Long) =>
      val id = RequestId.NumberId(n)
      id.asJson == Json.fromLong(n)
    }
  }

  property("JsonRpcRequest serialization roundtrip") {
    forAll { (req: JsonRpcRequest) =>
      val json = req.asJson
      val decoded = json.as[JsonRpcRequest]
      decoded == Right(req)
    }
  }

  property("JsonRpcRequest always has jsonrpc 2.0") {
    forAll { (req: JsonRpcRequest) =>
      req.asJson.hcursor.get[String]("jsonrpc") == Right("2.0")
    }
  }

  property("JsonRpcNotification serialization roundtrip") {
    forAll { (notif: JsonRpcNotification) =>
      val json = notif.asJson
      val decoded = json.as[JsonRpcNotification]
      decoded == Right(notif)
    }
  }

  property("JsonRpcNotification has no id field") {
    forAll { (notif: JsonRpcNotification) =>
      notif.asJson.hcursor.downField("id").failed
    }
  }

  property("JsonRpcResponse serialization roundtrip") {
    forAll { (resp: JsonRpcResponse) =>
      val json = resp.asJson
      val decoded = json.as[JsonRpcResponse]
      decoded == Right(resp)
    }
  }

  property("JsonRpcErrorResponse serialization roundtrip") {
    forAll { (resp: JsonRpcErrorResponse) =>
      val json = resp.asJson
      val decoded = json.as[JsonRpcErrorResponse]
      decoded == Right(resp)
    }
  }

  property("JsonRpcError serialization roundtrip") {
    forAll { (err: JsonRpcError) =>
      val json = err.asJson
      val decoded = json.as[JsonRpcError]
      decoded == Right(err)
    }
  }

  // ============================================================================
  // Common Types Property Tests
  // ============================================================================

  property("Role serialization roundtrip") {
    forAll { (role: Role) =>
      val json = role.asJson
      val decoded = json.as[Role]
      decoded == Right(role)
    }
  }

  property("LogLevel serialization roundtrip") {
    forAll { (level: LogLevel) =>
      val json = level.asJson
      val decoded = json.as[LogLevel]
      decoded == Right(level)
    }
  }

  property("Cursor serialization roundtrip") {
    forAll { (cursor: Cursor) =>
      val json = cursor.asJson
      val decoded = json.as[Cursor]
      decoded == Right(cursor)
    }
  }

  // ============================================================================
  // Icon and Annotations Property Tests
  // ============================================================================

  property("Icon serialization roundtrip") {
    forAll { (icon: Icon) =>
      val json = icon.asJson
      val decoded = json.as[Icon]
      decoded == Right(icon)
    }
  }

  property("Annotations serialization roundtrip") {
    forAll { (ann: Annotations) =>
      val json = ann.asJson
      val decoded = json.as[Annotations]
      decoded == Right(ann)
    }
  }

  // ============================================================================
  // Content Types Property Tests (Phase 1)
  // ============================================================================

  property("TextContent serialization roundtrip") {
    forAll { (tc: TextContent) =>
      val json = (tc: Content).asJson
      val decoded = json.as[Content]
      decoded == Right(tc)
    }
  }

  property("TextContent has type text") {
    forAll { (tc: TextContent) =>
      val json = (tc: Content).asJson
      json.hcursor.get[String]("type") == Right("text")
    }
  }

  property("ImageContent serialization roundtrip") {
    forAll { (ic: ImageContent) =>
      val json = (ic: Content).asJson
      val decoded = json.as[Content]
      decoded == Right(ic)
    }
  }

  property("ImageContent has type image") {
    forAll { (ic: ImageContent) =>
      val json = (ic: Content).asJson
      json.hcursor.get[String]("type") == Right("image")
    }
  }

  property("AudioContent serialization roundtrip") {
    forAll { (ac: AudioContent) =>
      val json = (ac: Content).asJson
      val decoded = json.as[Content]
      decoded == Right(ac)
    }
  }

  property("AudioContent has type audio") {
    forAll { (ac: AudioContent) =>
      val json = (ac: Content).asJson
      json.hcursor.get[String]("type") == Right("audio")
    }
  }

  property("ResourceContentRef serialization roundtrip") {
    forAll { (rc: ResourceContentRef) =>
      val json = (rc: Content).asJson
      val decoded = json.as[Content]
      decoded == Right(rc)
    }
  }

  property("ResourceContentRef has type resource") {
    forAll { (rc: ResourceContentRef) =>
      val json = (rc: Content).asJson
      json.hcursor.get[String]("type") == Right("resource")
    }
  }

  property("ResourceLinkContent serialization roundtrip") {
    forAll { (rl: ResourceLinkContent) =>
      val json = (rl: Content).asJson
      val decoded = json.as[Content]
      decoded == Right(rl)
    }
  }

  property("ResourceLinkContent has type resource_link") {
    forAll { (rl: ResourceLinkContent) =>
      val json = (rl: Content).asJson
      json.hcursor.get[String]("type") == Right("resource_link")
    }
  }

  property("Content serialization roundtrip") {
    forAll { (content: Content) =>
      val json = content.asJson
      val decoded = json.as[Content]
      decoded == Right(content)
    }
  }

  // ============================================================================
  // Sampling Types Property Tests (Phase 2)
  // ============================================================================

  property("SamplingTextContent serialization roundtrip") {
    forAll { (stc: SamplingTextContent) =>
      val json = (stc: SamplingContent).asJson
      val decoded = json.as[SamplingContent]
      decoded == Right(stc)
    }
  }

  property("SamplingTextContent has type text") {
    forAll { (stc: SamplingTextContent) =>
      val json = (stc: SamplingContent).asJson
      json.hcursor.get[String]("type") == Right("text")
    }
  }

  property("SamplingImageContent serialization roundtrip") {
    forAll { (sic: SamplingImageContent) =>
      val json = (sic: SamplingContent).asJson
      val decoded = json.as[SamplingContent]
      decoded == Right(sic)
    }
  }

  property("SamplingImageContent has type image") {
    forAll { (sic: SamplingImageContent) =>
      val json = (sic: SamplingContent).asJson
      json.hcursor.get[String]("type") == Right("image")
    }
  }

  property("SamplingAudioContent serialization roundtrip") {
    forAll { (sac: SamplingAudioContent) =>
      val json = (sac: SamplingContent).asJson
      val decoded = json.as[SamplingContent]
      decoded == Right(sac)
    }
  }

  property("SamplingAudioContent has type audio") {
    forAll { (sac: SamplingAudioContent) =>
      val json = (sac: SamplingContent).asJson
      json.hcursor.get[String]("type") == Right("audio")
    }
  }

  property("ToolUseContent serialization roundtrip") {
    forAll { (tuc: ToolUseContent) =>
      val json = (tuc: SamplingContent).asJson
      val decoded = json.as[SamplingContent]
      decoded == Right(tuc)
    }
  }

  property("ToolUseContent has type tool_use") {
    forAll { (tuc: ToolUseContent) =>
      val json = (tuc: SamplingContent).asJson
      json.hcursor.get[String]("type") == Right("tool_use")
    }
  }

  property("ToolResultContent serialization roundtrip") {
    forAll { (trc: ToolResultContent) =>
      val json = (trc: SamplingContent).asJson
      val decoded = json.as[SamplingContent]
      decoded == Right(trc)
    }
  }

  property("ToolResultContent has type tool_result") {
    forAll { (trc: ToolResultContent) =>
      val json = (trc: SamplingContent).asJson
      json.hcursor.get[String]("type") == Right("tool_result")
    }
  }

  property("SamplingContent serialization roundtrip") {
    forAll { (sc: SamplingContent) =>
      val json = sc.asJson
      val decoded = json.as[SamplingContent]
      decoded == Right(sc)
    }
  }

  property("SamplingMessage serialization roundtrip") {
    forAll { (sm: SamplingMessage) =>
      val json = sm.asJson
      val decoded = json.as[SamplingMessage]
      decoded == Right(sm)
    }
  }

  property("ModelHint serialization roundtrip") {
    forAll { (mh: ModelHint) =>
      val json = mh.asJson
      val decoded = json.as[ModelHint]
      decoded == Right(mh)
    }
  }

  property("ModelPreferences serialization roundtrip") {
    forAll { (mp: ModelPreferences) =>
      val json = mp.asJson
      val decoded = json.as[ModelPreferences]
      decoded == Right(mp)
    }
  }

  property("ModelPreferences priority values in valid range") {
    forAll { (mp: ModelPreferences) =>
      mp.costPriority.forall(v => v >= 0.0 && v <= 1.0) &&
      mp.speedPriority.forall(v => v >= 0.0 && v <= 1.0) &&
      mp.intelligencePriority.forall(v => v >= 0.0 && v <= 1.0)
    }
  }

  property("ToolChoice serialization roundtrip") {
    forAll { (tc: ToolChoice) =>
      val json = tc.asJson
      val decoded = json.as[ToolChoice]
      decoded == Right(tc)
    }
  }

  property("ToolChoice.Auto has type auto") {
    val json = (ToolChoice.Auto: ToolChoice).asJson
    json.hcursor.get[String]("type") == Right("auto")
  }

  property("ToolChoice.None has type none") {
    val json = (ToolChoice.None: ToolChoice).asJson
    json.hcursor.get[String]("type") == Right("none")
  }

  property("ToolChoice.Specific has type tool") {
    forAll(Gen.alphaStr.filter(_.nonEmpty)) { (name: String) =>
      val json = (ToolChoice.Specific(name): ToolChoice).asJson
      json.hcursor.get[String]("type") == Right("tool") &&
      json.hcursor.get[String]("name") == Right(name)
    }
  }

  property("CreateMessageParams serialization roundtrip") {
    forAll { (cmp: CreateMessageParams) =>
      val json = cmp.asJson
      val decoded = json.as[CreateMessageParams]
      decoded == Right(cmp)
    }
  }

  property("CreateMessageResult serialization roundtrip") {
    forAll { (cmr: CreateMessageResult) =>
      val json = cmr.asJson
      val decoded = json.as[CreateMessageResult]
      decoded == Right(cmr)
    }
  }

  // ============================================================================
  // Elicitation Types Property Tests (Phase 3)
  // ============================================================================

  property("ElicitAction serialization roundtrip") {
    forAll { (ea: ElicitAction) =>
      val json = ea.asJson
      val decoded = json.as[ElicitAction]
      decoded == Right(ea)
    }
  }

  property("ElicitResult serialization roundtrip") {
    forAll { (er: ElicitResult) =>
      val json = er.asJson
      val decoded = json.as[ElicitResult]
      decoded == Right(er)
    }
  }

  // ============================================================================
  // Capability Types Property Tests (Phase 4)
  // ============================================================================

  property("ToolsCapability serialization roundtrip") {
    forAll { (tc: ToolsCapability) =>
      val json = tc.asJson
      val decoded = json.as[ToolsCapability]
      decoded == Right(tc)
    }
  }

  property("ResourcesCapability serialization roundtrip") {
    forAll { (rc: ResourcesCapability) =>
      val json = rc.asJson
      val decoded = json.as[ResourcesCapability]
      decoded == Right(rc)
    }
  }

  property("PromptsCapability serialization roundtrip") {
    forAll { (pc: PromptsCapability) =>
      val json = pc.asJson
      val decoded = json.as[PromptsCapability]
      decoded == Right(pc)
    }
  }

  property("LoggingCapability serialization roundtrip") {
    forAll { (lc: LoggingCapability) =>
      val json = lc.asJson
      val decoded = json.as[LoggingCapability]
      decoded == Right(lc)
    }
  }

  property("CompletionsCapability serialization roundtrip") {
    forAll { (cc: CompletionsCapability) =>
      val json = cc.asJson
      val decoded = json.as[CompletionsCapability]
      decoded == Right(cc)
    }
  }

  property("ServerCapabilities serialization roundtrip") {
    forAll { (sc: ServerCapabilities) =>
      val json = sc.asJson
      val decoded = json.as[ServerCapabilities]
      decoded == Right(sc)
    }
  }

  property("RootsCapability serialization roundtrip") {
    forAll { (rc: RootsCapability) =>
      val json = rc.asJson
      val decoded = json.as[RootsCapability]
      decoded == Right(rc)
    }
  }

  property("SamplingCapability serialization roundtrip") {
    forAll { (sc: SamplingCapability) =>
      val json = sc.asJson
      val decoded = json.as[SamplingCapability]
      decoded == Right(sc)
    }
  }

  property("ElicitationCapability serialization roundtrip") {
    forAll { (ec: ElicitationCapability) =>
      val json = ec.asJson
      val decoded = json.as[ElicitationCapability]
      decoded == Right(ec)
    }
  }

  property("ClientCapabilities serialization roundtrip") {
    forAll { (cc: ClientCapabilities) =>
      val json = cc.asJson
      val decoded = json.as[ClientCapabilities]
      decoded == Right(cc)
    }
  }

  // ============================================================================
  // JsonSchema Types Property Tests (Phase 5)
  // ============================================================================

  property("JsonSchemaProperty serialization roundtrip") {
    forAll { (jsp: JsonSchemaProperty) =>
      val json = jsp.asJson
      val decoded = json.as[JsonSchemaProperty]
      decoded == Right(jsp)
    }
  }

  property("JsonSchema serialization roundtrip") {
    forAll { (js: JsonSchema) =>
      val json = js.asJson
      val decoded = json.as[JsonSchema]
      decoded == Right(js)
    }
  }

  property("JsonSchema.string has type string") {
    forAll(Gen.option(Gen.alphaNumStr)) { (desc: Option[String]) =>
      val schema = JsonSchema.string(desc)
      schema.asJson.hcursor.get[String]("type") == Right("string")
    }
  }

  property("JsonSchema.number has type number") {
    forAll(Gen.option(Gen.alphaNumStr)) { (desc: Option[String]) =>
      val schema = JsonSchema.number(desc)
      schema.asJson.hcursor.get[String]("type") == Right("number")
    }
  }

  property("JsonSchema.boolean has type boolean") {
    forAll(Gen.option(Gen.alphaNumStr)) { (desc: Option[String]) =>
      val schema = JsonSchema.boolean(desc)
      schema.asJson.hcursor.get[String]("type") == Right("boolean")
    }
  }

  property("JsonSchema.integer has type integer") {
    forAll(Gen.option(Gen.alphaNumStr)) { (desc: Option[String]) =>
      val schema = JsonSchema.integer(desc)
      schema.asJson.hcursor.get[String]("type") == Right("integer")
    }
  }

  property("JsonSchema.obj has type object") {
    val schema = JsonSchema.obj(Map.empty, List.empty)
    schema.asJson.hcursor.get[String]("type") == Right("object")
  }

  property("JsonSchema preserves required fields") {
    forAll(Gen.listOfN(2, Gen.alphaStr.filter(_.nonEmpty))) { (fields: List[String]) =>
      val schema = JsonSchema.obj(Map.empty, fields)
      val decoded = schema.asJson.as[JsonSchema]
      decoded.toOption.flatMap(_.required) == Some(fields)
    }
  }

  property("JsonSchemaProperty preserves enum values") {
    forAll(Gen.listOfN(3, Gen.alphaStr.filter(_.nonEmpty))) { (values: List[String]) =>
      val prop = JsonSchema.stringEnum(values)
      val decoded = prop.asJson.as[JsonSchemaProperty]
      decoded.toOption.flatMap(_.`enum`) == Some(values)
    }
  }

  property("JsonSchemaProperty preserves default value") {
    forAll(Gen.alphaStr.filter(_.nonEmpty)) { (default: String) =>
      val prop = JsonSchema.stringWithDefault("desc", default)
      val decoded = prop.asJson.as[JsonSchemaProperty]
      decoded.toOption.flatMap(_.default) == Some(Json.fromString(default))
    }
  }

  // ============================================================================
  // Resource & Prompt Types Property Tests (Phase 6)
  // ============================================================================

  property("Resource serialization roundtrip") {
    forAll { (r: Resource) =>
      val json = r.asJson
      val decoded = json.as[Resource]
      decoded == Right(r)
    }
  }

  property("ResourceTemplate serialization roundtrip") {
    forAll { (rt: ResourceTemplate) =>
      val json = rt.asJson
      val decoded = json.as[ResourceTemplate]
      decoded == Right(rt)
    }
  }

  property("ResourceContent serialization roundtrip") {
    forAll { (rc: ResourceContent) =>
      val json = rc.asJson
      val decoded = json.as[ResourceContent]
      decoded == Right(rc)
    }
  }

  property("PromptArgument serialization roundtrip") {
    forAll { (pa: PromptArgument) =>
      val json = pa.asJson
      val decoded = json.as[PromptArgument]
      decoded == Right(pa)
    }
  }

  property("Prompt serialization roundtrip") {
    forAll { (p: Prompt) =>
      val json = p.asJson
      val decoded = json.as[Prompt]
      decoded == Right(p)
    }
  }

  property("PromptMessage serialization roundtrip") {
    forAll { (pm: PromptMessage) =>
      val json = pm.asJson
      val decoded = json.as[PromptMessage]
      decoded == Right(pm)
    }
  }

  property("GetPromptResult serialization roundtrip") {
    forAll { (gpr: GetPromptResult) =>
      val json = gpr.asJson
      val decoded = json.as[GetPromptResult]
      decoded == Right(gpr)
    }
  }

  property("Root serialization roundtrip") {
    forAll { (r: Root) =>
      val json = r.asJson
      val decoded = json.as[Root]
      decoded == Right(r)
    }
  }

  property("ListRootsResult serialization roundtrip") {
    forAll { (lrr: ListRootsResult) =>
      val json = lrr.asJson
      val decoded = json.as[ListRootsResult]
      decoded == Right(lrr)
    }
  }

  // ============================================================================
  // Lifecycle & Misc Types Property Tests (Phase 7)
  // ============================================================================

  property("ServerInfo serialization roundtrip") {
    forAll { (si: ServerInfo) =>
      val json = si.asJson
      val decoded = json.as[ServerInfo]
      decoded == Right(si)
    }
  }

  property("ClientInfo serialization roundtrip") {
    forAll { (ci: ClientInfo) =>
      val json = ci.asJson
      val decoded = json.as[ClientInfo]
      decoded == Right(ci)
    }
  }

  property("Implementation serialization roundtrip") {
    forAll { (impl: Implementation) =>
      val json = impl.asJson
      val decoded = json.as[Implementation]
      decoded == Right(impl)
    }
  }

  property("InitializeParams serialization roundtrip") {
    forAll { (ip: InitializeParams) =>
      val json = ip.asJson
      val decoded = json.as[InitializeParams]
      decoded == Right(ip)
    }
  }

  property("InitializeResult serialization roundtrip") {
    forAll { (ir: InitializeResult) =>
      val json = ir.asJson
      val decoded = json.as[InitializeResult]
      decoded == Right(ir)
    }
  }

  property("ProgressParams serialization roundtrip") {
    forAll { (pp: ProgressParams) =>
      val json = pp.asJson
      val decoded = json.as[ProgressParams]
      decoded == Right(pp)
    }
  }

  property("CancelledParams serialization roundtrip") {
    forAll { (cp: CancelledParams) =>
      val json = cp.asJson
      val decoded = json.as[CancelledParams]
      decoded == Right(cp)
    }
  }

  property("LogMessage serialization roundtrip") {
    forAll { (lm: LogMessage) =>
      val json = lm.asJson
      val decoded = json.as[LogMessage]
      decoded == Right(lm)
    }
  }

  property("ToolAnnotations serialization roundtrip") {
    forAll { (ta: ToolAnnotations) =>
      val json = ta.asJson
      val decoded = json.as[ToolAnnotations]
      decoded == Right(ta)
    }
  }

  property("TaskSupport serialization roundtrip") {
    forAll { (ts: TaskSupport) =>
      val json = ts.asJson
      val decoded = json.as[TaskSupport]
      decoded == Right(ts)
    }
  }

  property("ToolExecution serialization roundtrip") {
    forAll { (te: ToolExecution) =>
      val json = te.asJson
      val decoded = json.as[ToolExecution]
      decoded == Right(te)
    }
  }

  property("Tool serialization roundtrip") {
    forAll { (t: Tool) =>
      val json = t.asJson
      val decoded = json.as[Tool]
      decoded == Right(t)
    }
  }

  property("ToolResult serialization roundtrip") {
    forAll { (tr: ToolResult) =>
      val json = tr.asJson
      val decoded = json.as[ToolResult]
      decoded == Right(tr)
    }
  }

  // ============================================================================
  // McpError Property Tests (Phase 8)
  // ============================================================================

  property("McpError to JsonRpcError preserves error code category") {
    forAll { (err: McpError) =>
      val jsonErr = McpError.toJsonRpcError(err)
      // Verify error codes are in valid ranges
      jsonErr.code match
        case -32700 => true  // Parse error
        case -32600 => true  // Invalid request
        case -32601 => true  // Method not found
        case -32602 => true  // Invalid params
        case -32603 => true  // Internal error
        case -32800 => true  // Cancelled
        case _ => false
    }
  }

  property("McpError message is included in JsonRpcError") {
    forAll { (err: McpError) =>
      val jsonErr = McpError.toJsonRpcError(err)
      // Message is either preserved or enhanced (e.g., methodNotFound prepends "Method not found: ")
      jsonErr.message.contains(err.message) || err.message.contains(jsonErr.message) ||
      // Special case: methodNotFound helper reformats the message
      (err match {
        case _: McpError.ToolNotFound | _: McpError.MethodNotFound | _: McpError.MethodNotSupported =>
          jsonErr.message.startsWith("Method not found:")
        case _ => false
      })
    }
  }

  property("ToolNotFound produces method not found error code") {
    forAll(Gen.alphaStr.filter(_.nonEmpty)) { (name: String) =>
      val err = McpError.ToolNotFound(name)
      val jsonErr = McpError.toJsonRpcError(err)
      jsonErr.code == JsonRpcErrorCode.MethodNotFound
    }
  }

  property("ResourceNotFound produces invalid params error code") {
    forAll(Gen.alphaStr.filter(_.nonEmpty)) { (uri: String) =>
      val err = McpError.ResourceNotFound(uri)
      val jsonErr = McpError.toJsonRpcError(err)
      jsonErr.code == JsonRpcErrorCode.InvalidParams
    }
  }

  property("PromptNotFound produces invalid params error code") {
    forAll(Gen.alphaStr.filter(_.nonEmpty)) { (name: String) =>
      val err = McpError.PromptNotFound(name)
      val jsonErr = McpError.toJsonRpcError(err)
      jsonErr.code == JsonRpcErrorCode.InvalidParams
    }
  }

  property("InvalidToolArguments produces invalid params error code") {
    forAll(Gen.alphaStr.filter(_.nonEmpty), Gen.alphaStr.filter(_.nonEmpty)) { (name: String, reason: String) =>
      val err = McpError.InvalidToolArguments(name, reason)
      val jsonErr = McpError.toJsonRpcError(err)
      jsonErr.code == JsonRpcErrorCode.InvalidParams
    }
  }

  property("MethodNotFound produces method not found error code") {
    forAll(Gen.alphaStr.filter(_.nonEmpty)) { (method: String) =>
      val err = McpError.MethodNotFound(method)
      val jsonErr = McpError.toJsonRpcError(err)
      jsonErr.code == JsonRpcErrorCode.MethodNotFound
    }
  }

  property("InternalError produces internal error code") {
    forAll(Gen.alphaStr.filter(_.nonEmpty)) { (msg: String) =>
      val err = McpError.InternalError(msg)
      val jsonErr = McpError.toJsonRpcError(err)
      jsonErr.code == JsonRpcErrorCode.InternalError
    }
  }

  property("RequestCancelled produces -32800 error code") {
    forAll(genNonNullRequestId) { (requestId: RequestId) =>
      val err = McpError.RequestCancelled(requestId)
      val jsonErr = McpError.toJsonRpcError(err)
      jsonErr.code == -32800
    }
  }
