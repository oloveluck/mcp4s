package mcp4s.protocol

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*

/** Circe codecs for JSON-RPC and MCP protocol types
  * Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/schema
  */
object Codecs:

  // === RequestId ===

  given Encoder[RequestId] = Encoder.instance {
    case RequestId.StringId(s) => Json.fromString(s)
    case RequestId.NumberId(n) => Json.fromLong(n)
    case RequestId.NullId      => Json.Null
  }

  given Decoder[RequestId] = Decoder.instance { cursor =>
    cursor.as[String].map(RequestId.StringId(_))
      .orElse(cursor.as[Long].map(RequestId.NumberId(_)))
      .orElse(cursor.as[Option[Unit]].map(_ => RequestId.NullId))
  }

  // === Opaque Types ===

  given Encoder[ToolName] = Encoder.encodeString.contramap(_.value)
  given Decoder[ToolName] = Decoder.decodeString.map(ToolName(_))

  given Encoder[ResourceUri] = Encoder.encodeString.contramap(_.value)
  given Decoder[ResourceUri] = Decoder.decodeString.map(ResourceUri(_))

  given Encoder[PromptName] = Encoder.encodeString.contramap(_.value)
  given Decoder[PromptName] = Decoder.decodeString.map(PromptName(_))

  // === JsonRpcError ===

  given Encoder[JsonRpcError] = deriveEncoder
  given Decoder[JsonRpcError] = deriveDecoder

  // === JSON-RPC Messages ===

  given Encoder[JsonRpcRequest] = Encoder.instance { req =>
    Json.obj(
      "jsonrpc" -> Json.fromString(req.jsonrpc),
      "id" -> req.id.asJson,
      "method" -> Json.fromString(req.method),
      "params" -> req.params.getOrElse(Json.Null)
    ).dropNullValues
  }

  given Decoder[JsonRpcRequest] = Decoder.instance { cursor =>
    for
      id <- cursor.get[RequestId]("id")
      method <- cursor.get[String]("method")
      params <- cursor.get[Option[Json]]("params")
    yield JsonRpcRequest(id, method, params)
  }

  given Encoder[JsonRpcNotification] = Encoder.instance { notif =>
    Json.obj(
      "jsonrpc" -> Json.fromString(notif.jsonrpc),
      "method" -> Json.fromString(notif.method),
      "params" -> notif.params.getOrElse(Json.Null)
    ).dropNullValues
  }

  given Decoder[JsonRpcNotification] = Decoder.instance { cursor =>
    for
      method <- cursor.get[String]("method")
      params <- cursor.get[Option[Json]]("params")
    yield JsonRpcNotification(method, params)
  }

  given Encoder[JsonRpcResponse] = Encoder.instance { resp =>
    Json.obj(
      "jsonrpc" -> Json.fromString(resp.jsonrpc),
      "id" -> resp.id.asJson,
      "result" -> resp.result
    )
  }

  given Decoder[JsonRpcResponse] = Decoder.instance { cursor =>
    for
      id <- cursor.get[RequestId]("id")
      result <- cursor.get[Json]("result")
    yield JsonRpcResponse(id, result)
  }

  given Encoder[JsonRpcErrorResponse] = Encoder.instance { resp =>
    Json.obj(
      "jsonrpc" -> Json.fromString(resp.jsonrpc),
      "id" -> resp.id.asJson,
      "error" -> resp.error.asJson
    )
  }

  given Decoder[JsonRpcErrorResponse] = Decoder.instance { cursor =>
    for
      id <- cursor.get[RequestId]("id")
      error <- cursor.get[JsonRpcError]("error")
    yield JsonRpcErrorResponse(id, error)
  }

  given Encoder[JsonRpcMessage] = Encoder.instance {
    case r: JsonRpcRequest       => r.asJson
    case n: JsonRpcNotification  => n.asJson
    case r: JsonRpcResponse      => r.asJson
    case e: JsonRpcErrorResponse => e.asJson
  }

  given Decoder[JsonRpcMessage] = Decoder.instance { cursor =>
    val hasId = cursor.downField("id").succeeded
    val hasMethod = cursor.downField("method").succeeded
    val hasResult = cursor.downField("result").succeeded
    val hasError = cursor.downField("error").succeeded

    (hasId, hasMethod, hasResult, hasError) match
      case (true, true, false, false)  => cursor.as[JsonRpcRequest]
      case (false, true, false, false) => cursor.as[JsonRpcNotification]
      case (true, false, true, false)  => cursor.as[JsonRpcResponse]
      case (true, false, false, true)  => cursor.as[JsonRpcErrorResponse]
      case _ =>
        Left(DecodingFailure("Invalid JSON-RPC message structure", cursor.history))
  }

  // === Common Types ===

  given Encoder[Icon] = deriveEncoder
  given Decoder[Icon] = deriveDecoder

  given Encoder[Annotations] = deriveEncoder
  given Decoder[Annotations] = deriveDecoder

  // === MCP Types ===

  given Encoder[ServerInfo] = deriveEncoder[ServerInfo].mapJson(_.dropNullValues)
  given Decoder[ServerInfo] = deriveDecoder

  given Encoder[ClientInfo] = deriveEncoder
  given Decoder[ClientInfo] = deriveDecoder

  given Encoder[Implementation] = deriveEncoder
  given Decoder[Implementation] = deriveDecoder

  // === Capabilities ===

  given Encoder[ToolsCapability] = deriveEncoder[ToolsCapability].mapJson(_.dropNullValues)
  given Decoder[ToolsCapability] = deriveDecoder

  given Encoder[ResourcesCapability] = deriveEncoder[ResourcesCapability].mapJson(_.dropNullValues)
  given Decoder[ResourcesCapability] = deriveDecoder

  given Encoder[PromptsCapability] = deriveEncoder[PromptsCapability].mapJson(_.dropNullValues)
  given Decoder[PromptsCapability] = deriveDecoder

  given Encoder[LoggingCapability] = Encoder.instance(_ => Json.obj())
  given Decoder[LoggingCapability] = Decoder.instance(_ => Right(LoggingCapability()))

  given Encoder[CompletionsCapability] = Encoder.instance(_ => Json.obj())
  given Decoder[CompletionsCapability] = Decoder.instance(_ => Right(CompletionsCapability()))

  // Server tasks
  given Encoder[ToolTaskRequests] = deriveEncoder
  given Decoder[ToolTaskRequests] = deriveDecoder

  given Encoder[ServerTaskRequests] = deriveEncoder
  given Decoder[ServerTaskRequests] = deriveDecoder

  given Encoder[ServerTasksCapability] = deriveEncoder
  given Decoder[ServerTasksCapability] = deriveDecoder

  given Encoder[ServerCapabilities] = deriveEncoder[ServerCapabilities].mapJson(_.dropNullValues)
  given Decoder[ServerCapabilities] = deriveDecoder

  // Client tasks
  given Encoder[SamplingTaskRequests] = deriveEncoder
  given Decoder[SamplingTaskRequests] = deriveDecoder

  given Encoder[ElicitationTaskRequests] = deriveEncoder
  given Decoder[ElicitationTaskRequests] = deriveDecoder

  given Encoder[ClientTaskRequests] = deriveEncoder
  given Decoder[ClientTaskRequests] = deriveDecoder

  given Encoder[ClientTasksCapability] = deriveEncoder
  given Decoder[ClientTasksCapability] = deriveDecoder

  given Encoder[ElicitationCapability] = deriveEncoder
  given Decoder[ElicitationCapability] = deriveDecoder

  given Encoder[RootsCapability] = deriveEncoder
  given Decoder[RootsCapability] = deriveDecoder

  given Encoder[SamplingCapability] = deriveEncoder
  given Decoder[SamplingCapability] = deriveDecoder

  given Encoder[ClientCapabilities] = deriveEncoder
  given Decoder[ClientCapabilities] = deriveDecoder

  // === JSON Schema ===

  given Encoder[JsonSchemaProperty] = deriveEncoder[JsonSchemaProperty].mapJson(_.dropNullValues)
  given Decoder[JsonSchemaProperty] = deriveDecoder

  given Encoder[JsonSchema] = deriveEncoder[JsonSchema].mapJson(_.dropNullValues)
  given Decoder[JsonSchema] = deriveDecoder

  // === Tools ===

  given Encoder[ToolAnnotations] = deriveEncoder
  given Decoder[ToolAnnotations] = deriveDecoder

  given Encoder[TaskSupport] = Encoder.encodeString.contramap {
    case TaskSupport.Forbidden => "forbidden"
    case TaskSupport.Optional  => "optional"
    case TaskSupport.Required  => "required"
  }

  given Decoder[TaskSupport] = Decoder.decodeString.emap {
    case "forbidden" => Right(TaskSupport.Forbidden)
    case "optional"  => Right(TaskSupport.Optional)
    case "required"  => Right(TaskSupport.Required)
    case other       => Left(s"Unknown task support: $other")
  }

  given Encoder[ToolExecution] = deriveEncoder
  given Decoder[ToolExecution] = deriveDecoder

  given Encoder[Tool] = deriveEncoder[Tool].mapJson(_.dropNullValues)
  given Decoder[Tool] = deriveDecoder

  // === Content Types ===

  given Encoder[ContentType] = Encoder.encodeString.contramap {
    case ContentType.Text         => "text"
    case ContentType.Image        => "image"
    case ContentType.Audio        => "audio"
    case ContentType.Resource     => "resource"
    case ContentType.ResourceLink => "resource_link"
  }

  given Decoder[ContentType] = Decoder.decodeString.emap {
    case "text"          => Right(ContentType.Text)
    case "image"         => Right(ContentType.Image)
    case "audio"         => Right(ContentType.Audio)
    case "resource"      => Right(ContentType.Resource)
    case "resource_link" => Right(ContentType.ResourceLink)
    case other           => Left(s"Unknown content type: $other")
  }

  given Encoder[TextContent] = Encoder.instance { tc =>
    Json.obj(
      "type" -> Json.fromString("text"),
      "text" -> Json.fromString(tc.text),
      "annotations" -> tc.annotations.asJson,
      "_meta" -> tc._meta.asJson
    ).dropNullValues
  }

  given Encoder[ImageContent] = Encoder.instance { ic =>
    Json.obj(
      "type" -> Json.fromString("image"),
      "data" -> Json.fromString(ic.data),
      "mimeType" -> Json.fromString(ic.mimeType),
      "annotations" -> ic.annotations.asJson,
      "_meta" -> ic._meta.asJson
    ).dropNullValues
  }

  given Encoder[AudioContent] = Encoder.instance { ac =>
    Json.obj(
      "type" -> Json.fromString("audio"),
      "data" -> Json.fromString(ac.data),
      "mimeType" -> Json.fromString(ac.mimeType),
      "annotations" -> ac.annotations.asJson,
      "_meta" -> ac._meta.asJson
    ).dropNullValues
  }

  given Encoder[ResourceContentRef] = Encoder.instance { rc =>
    Json.obj(
      "type" -> Json.fromString("resource"),
      "resource" -> Json.obj(
        "uri" -> Json.fromString(rc.uri),
        "mimeType" -> rc.mimeType.fold(Json.Null)(Json.fromString),
        "text" -> rc.text.fold(Json.Null)(Json.fromString)
      ).dropNullValues,
      "annotations" -> rc.annotations.asJson,
      "_meta" -> rc._meta.asJson
    ).dropNullValues
  }

  given Encoder[ResourceLinkContent] = Encoder.instance { rl =>
    Json.obj(
      "type" -> Json.fromString("resource_link"),
      "uri" -> Json.fromString(rl.uri),
      "name" -> Json.fromString(rl.name),
      "title" -> rl.title.asJson,
      "description" -> rl.description.asJson,
      "mimeType" -> rl.mimeType.asJson,
      "annotations" -> rl.annotations.asJson,
      "size" -> rl.size.asJson,
      "icons" -> rl.icons.asJson
    ).dropNullValues
  }

  given Encoder[Content] = Encoder.instance {
    case tc: TextContent        => tc.asJson
    case ic: ImageContent       => ic.asJson
    case ac: AudioContent       => ac.asJson
    case rc: ResourceContentRef => rc.asJson
    case rl: ResourceLinkContent => rl.asJson
  }

  given Decoder[Content] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "text" =>
        for
          text <- cursor.get[String]("text")
          annotations <- cursor.get[Option[Annotations]]("annotations")
          meta <- cursor.get[Option[Json]]("_meta")
        yield TextContent(text, annotations, meta)

      case "image" =>
        for
          data <- cursor.get[String]("data")
          mime <- cursor.get[String]("mimeType")
          annotations <- cursor.get[Option[Annotations]]("annotations")
          meta <- cursor.get[Option[Json]]("_meta")
        yield ImageContent(data, mime, annotations, meta)

      case "audio" =>
        for
          data <- cursor.get[String]("data")
          mime <- cursor.get[String]("mimeType")
          annotations <- cursor.get[Option[Annotations]]("annotations")
          meta <- cursor.get[Option[Json]]("_meta")
        yield AudioContent(data, mime, annotations, meta)

      case "resource" =>
        val res = cursor.downField("resource")
        for
          uri <- res.get[String]("uri")
          mime <- res.get[Option[String]]("mimeType")
          text <- res.get[Option[String]]("text")
          annotations <- cursor.get[Option[Annotations]]("annotations")
          meta <- cursor.get[Option[Json]]("_meta")
        yield ResourceContentRef(uri, mime, text, annotations, meta)

      case "resource_link" =>
        for
          uri <- cursor.get[String]("uri")
          name <- cursor.get[String]("name")
          title <- cursor.get[Option[String]]("title")
          description <- cursor.get[Option[String]]("description")
          mimeType <- cursor.get[Option[String]]("mimeType")
          annotations <- cursor.get[Option[Annotations]]("annotations")
          size <- cursor.get[Option[Long]]("size")
          icons <- cursor.get[Option[List[Icon]]]("icons")
        yield ResourceLinkContent(uri, name, title, description, mimeType, annotations, size, icons)

      case other =>
        Left(DecodingFailure(s"Unknown content type: $other", cursor.history))
    }
  }

  given Encoder[ToolResult] = deriveEncoder[ToolResult].mapJson(_.dropNullValues)
  given Decoder[ToolResult] = deriveDecoder

  // === Resources ===

  given Encoder[Resource] = deriveEncoder
  given Decoder[Resource] = deriveDecoder

  given Encoder[ResourceTemplate] = deriveEncoder
  given Decoder[ResourceTemplate] = deriveDecoder

  given Encoder[ResourceContent] = deriveEncoder
  given Decoder[ResourceContent] = deriveDecoder

  // === Prompts ===

  given Encoder[PromptArgument] = deriveEncoder
  given Decoder[PromptArgument] = deriveDecoder

  given Encoder[Prompt] = deriveEncoder
  given Decoder[Prompt] = deriveDecoder

  given Encoder[Role] = Encoder.encodeString.contramap {
    case Role.User      => "user"
    case Role.Assistant => "assistant"
  }

  given Decoder[Role] = Decoder.decodeString.emap {
    case "user"      => Right(Role.User)
    case "assistant" => Right(Role.Assistant)
    case other       => Left(s"Unknown role: $other")
  }

  given Encoder[PromptMessage] = deriveEncoder
  given Decoder[PromptMessage] = deriveDecoder

  given Encoder[GetPromptResult] = deriveEncoder
  given Decoder[GetPromptResult] = deriveDecoder

  // === Lifecycle ===

  given Encoder[InitializeParams] = deriveEncoder
  given Decoder[InitializeParams] = deriveDecoder

  given Encoder[InitializeResult] = deriveEncoder[InitializeResult].mapJson(_.dropNullValues)
  given Decoder[InitializeResult] = deriveDecoder

  // === Logging ===

  given Encoder[LogLevel] = Encoder.encodeString.contramap {
    case LogLevel.Debug     => "debug"
    case LogLevel.Info      => "info"
    case LogLevel.Notice    => "notice"
    case LogLevel.Warning   => "warning"
    case LogLevel.Error     => "error"
    case LogLevel.Critical  => "critical"
    case LogLevel.Alert     => "alert"
    case LogLevel.Emergency => "emergency"
  }

  given Decoder[LogLevel] = Decoder.decodeString.emap {
    case "debug"     => Right(LogLevel.Debug)
    case "info"      => Right(LogLevel.Info)
    case "notice"    => Right(LogLevel.Notice)
    case "warning"   => Right(LogLevel.Warning)
    case "error"     => Right(LogLevel.Error)
    case "critical"  => Right(LogLevel.Critical)
    case "alert"     => Right(LogLevel.Alert)
    case "emergency" => Right(LogLevel.Emergency)
    case other       => Left(s"Unknown log level: $other")
  }

  given Encoder[LogMessage] = deriveEncoder
  given Decoder[LogMessage] = deriveDecoder

  // === Pagination ===

  given Encoder[Cursor] = Encoder.encodeString.contramap(_.value)
  given Decoder[Cursor] = Decoder.decodeString.map(Cursor(_))

  // === Notifications ===

  given Encoder[CancelledParams] = deriveEncoder
  given Decoder[CancelledParams] = deriveDecoder

  given Encoder[ProgressParams] = deriveEncoder
  given Decoder[ProgressParams] = deriveDecoder

  // === Roots (Client Feature) ===

  given Encoder[Root] = deriveEncoder
  given Decoder[Root] = deriveDecoder

  given Encoder[ListRootsResult] = deriveEncoder
  given Decoder[ListRootsResult] = deriveDecoder

  // === Sampling (Client Feature) ===

  given Encoder[SamplingTextContent] = Encoder.instance { stc =>
    Json.obj(
      "type" -> Json.fromString("text"),
      "text" -> Json.fromString(stc.text)
    )
  }

  given Encoder[SamplingImageContent] = Encoder.instance { sic =>
    Json.obj(
      "type" -> Json.fromString("image"),
      "data" -> Json.fromString(sic.data),
      "mimeType" -> Json.fromString(sic.mimeType)
    )
  }

  given Encoder[SamplingAudioContent] = Encoder.instance { sac =>
    Json.obj(
      "type" -> Json.fromString("audio"),
      "data" -> Json.fromString(sac.data),
      "mimeType" -> Json.fromString(sac.mimeType)
    )
  }

  given Encoder[ToolUseContent] = Encoder.instance { tuc =>
    Json.obj(
      "type" -> Json.fromString("tool_use"),
      "id" -> Json.fromString(tuc.id),
      "name" -> Json.fromString(tuc.name),
      "input" -> tuc.input
    )
  }

  given Encoder[ToolResultContent] = Encoder.instance { trc =>
    Json.obj(
      "type" -> Json.fromString("tool_result"),
      "toolUseId" -> Json.fromString(trc.toolUseId),
      "content" -> trc.content.asJson,
      "isError" -> Json.fromBoolean(trc.isError)
    ).dropNullValues
  }

  given Encoder[SamplingContent] = Encoder.instance {
    case stc: SamplingTextContent  => stc.asJson
    case sic: SamplingImageContent => sic.asJson
    case sac: SamplingAudioContent => sac.asJson
    case tuc: ToolUseContent       => tuc.asJson
    case trc: ToolResultContent    => trc.asJson
  }

  given Decoder[SamplingContent] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "text" =>
        cursor.get[String]("text").map(SamplingTextContent(_))
      case "image" =>
        for
          data <- cursor.get[String]("data")
          mime <- cursor.get[String]("mimeType")
        yield SamplingImageContent(data, mime)
      case "audio" =>
        for
          data <- cursor.get[String]("data")
          mime <- cursor.get[String]("mimeType")
        yield SamplingAudioContent(data, mime)
      case "tool_use" =>
        for
          id <- cursor.get[String]("id")
          name <- cursor.get[String]("name")
          input <- cursor.get[Json]("input")
        yield ToolUseContent(id, name, input)
      case "tool_result" =>
        for
          toolUseId <- cursor.get[String]("toolUseId")
          content <- cursor.get[List[Content]]("content")
          isError <- cursor.get[Option[Boolean]]("isError")
        yield ToolResultContent(toolUseId, content, isError.getOrElse(false))
      case other =>
        Left(DecodingFailure(s"Unknown sampling content type: $other", cursor.history))
    }
  }

  given Encoder[SamplingMessage] = Encoder.instance { sm =>
    Json.obj(
      "role" -> sm.role.asJson,
      "content" -> sm.content.asJson,
      "_meta" -> sm._meta.asJson
    ).dropNullValues
  }

  given Decoder[SamplingMessage] = Decoder.instance { cursor =>
    for
      role <- cursor.get[Role]("role")
      content <- cursor.get[SamplingContent]("content")
      meta <- cursor.get[Option[Json]]("_meta")
    yield SamplingMessage(role, content, meta)
  }

  given Encoder[ModelHint] = deriveEncoder
  given Decoder[ModelHint] = deriveDecoder

  given Encoder[ModelPreferences] = deriveEncoder
  given Decoder[ModelPreferences] = deriveDecoder

  given Encoder[ToolChoice] = Encoder.instance {
    case ToolChoice.Auto            => Json.obj("type" -> Json.fromString("auto"))
    case ToolChoice.None            => Json.obj("type" -> Json.fromString("none"))
    case ToolChoice.Specific(name)  => Json.obj("type" -> Json.fromString("tool"), "name" -> Json.fromString(name))
  }

  given Decoder[ToolChoice] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "auto" => Right(ToolChoice.Auto)
      case "none" => Right(ToolChoice.None)
      case "tool" => cursor.get[String]("name").map(ToolChoice.Specific(_))
      case other  => Left(DecodingFailure(s"Unknown tool choice type: $other", cursor.history))
    }
  }

  given Encoder[CreateMessageParams] = deriveEncoder
  given Decoder[CreateMessageParams] = deriveDecoder

  given Encoder[CreateMessageResult] = Encoder.instance { cmr =>
    Json.obj(
      "role" -> cmr.role.asJson,
      "content" -> cmr.content.asJson,
      "model" -> Json.fromString(cmr.model),
      "stopReason" -> cmr.stopReason.asJson,
      "_meta" -> cmr._meta.asJson
    ).dropNullValues
  }

  given Decoder[CreateMessageResult] = Decoder.instance { cursor =>
    for
      role <- cursor.get[Role]("role")
      content <- cursor.get[SamplingContent]("content")
      model <- cursor.get[String]("model")
      stopReason <- cursor.get[Option[String]]("stopReason")
      meta <- cursor.get[Option[Json]]("_meta")
    yield CreateMessageResult(role, content, model, stopReason, meta)
  }

  // === Elicitation (Client Feature) ===

  given Encoder[ElicitFormParams] = Encoder.instance { efp =>
    Json.obj(
      "mode" -> Json.fromString("form"),
      "message" -> Json.fromString(efp.message),
      "requestedSchema" -> efp.requestedSchema.asJson
    )
  }

  given Encoder[ElicitUrlParams] = Encoder.instance { eup =>
    Json.obj(
      "mode" -> Json.fromString("url"),
      "message" -> Json.fromString(eup.message),
      "elicitationId" -> Json.fromString(eup.elicitationId),
      "url" -> Json.fromString(eup.url)
    )
  }

  given Encoder[ElicitParams] = Encoder.instance {
    case efp: ElicitFormParams => efp.asJson
    case eup: ElicitUrlParams  => eup.asJson
  }

  given Decoder[ElicitParams] = Decoder.instance { cursor =>
    cursor.get[Option[String]]("mode").flatMap {
      case Some("url") =>
        for
          message <- cursor.get[String]("message")
          elicitationId <- cursor.get[String]("elicitationId")
          url <- cursor.get[String]("url")
        yield ElicitUrlParams(message, elicitationId, url)
      case _ => // Default to form mode
        for
          message <- cursor.get[String]("message")
          schema <- cursor.get[JsonSchema]("requestedSchema")
          mode <- cursor.get[Option[String]]("mode")
        yield ElicitFormParams(message, schema, mode)
    }
  }

  given Encoder[ElicitAction] = Encoder.encodeString.contramap {
    case ElicitAction.Accept  => "accept"
    case ElicitAction.Decline => "decline"
    case ElicitAction.Cancel  => "cancel"
  }

  given Decoder[ElicitAction] = Decoder.decodeString.emap {
    case "accept"  => Right(ElicitAction.Accept)
    case "decline" => Right(ElicitAction.Decline)
    case "cancel"  => Right(ElicitAction.Cancel)
    case other     => Left(s"Unknown elicit action: $other")
  }

  given Encoder[ElicitResult] = Encoder.instance { er =>
    Json.obj(
      "action" -> er.action.asJson,
      "content" -> er.content.map(_.asJson).getOrElse(Json.Null)
    ).dropNullValues
  }

  given Decoder[ElicitResult] = Decoder.instance { cursor =>
    for
      action <- cursor.get[ElicitAction]("action")
      content <- cursor.get[Option[Map[String, Json]]]("content")
    yield ElicitResult(action, content)
  }
