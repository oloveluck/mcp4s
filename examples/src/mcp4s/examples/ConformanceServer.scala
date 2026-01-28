package mcp4s.examples

import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.transport.*
import org.typelevel.otel4s.trace.Tracer

/** MCP Conformance Test Server
  *
  * Implements all tools, resources, and prompts required by the official
  * MCP Conformance Test Framework.
  *
  * Run with: mill examples.runMain mcp4s.examples.ConformanceServer
  *
  * Then run conformance tests:
  *   mill conformance --url http://localhost:3000/mcp
  */
object ConformanceServer extends IOApp.Simple:

  // === Test Data ===

  // 1x1 red PNG pixel (base64)
  val TestImageBase64: String =
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8DwHwAFBQIAX8jx0gAAAABJRU5ErkJggg=="

  // Minimal WAV file (base64)
  val TestAudioBase64: String =
    "UklGRiYAAABXQVZFZm10IBAAAAABAAEAQB8AAAB9AAACABAAZGF0YQIAAAA="

  // === Tool Input Types ===

  case class SamplingArgs(prompt: String)
  object SamplingArgs:
    given Decoder[SamplingArgs] = deriveDecoder
    given ToolInput[SamplingArgs] = ToolInput.derived

  case class ElicitationArgs(message: String)
  object ElicitationArgs:
    given Decoder[ElicitationArgs] = deriveDecoder
    given ToolInput[ElicitationArgs] = ToolInput.derived

  // === Prompt Input Types ===

  case class PromptWithArgsInput(
      @description("First test argument") arg1: String,
      @description("Second test argument") arg2: String
  )
  object PromptWithArgsInput:
    given PromptInput[PromptWithArgsInput] = PromptInput.derived

  case class EmbeddedResourcePromptInput(
      @description("URI of the resource to embed") resourceUri: String
  )
  object EmbeddedResourcePromptInput:
    given PromptInput[EmbeddedResourcePromptInput] = PromptInput.derived

  // === Server Definition ===

  val server: McpServer[IO] = McpServer
    .builder[IO]
    .withInfo(ServerInfo("mcp-conformance-test-server", "1.0.0"))
    // ========================================
    // TOOLS
    // ========================================
    // Simple text tool
    .tool("test_simple_text", "Tests simple text content response") { _ =>
      IO.pure(ToolResult(List(TextContent("This is a simple text response for testing."))))
    }
    // Image content tool
    .tool("test_image_content", "Tests image content response") { _ =>
      IO.pure(ToolResult(List(ImageContent(TestImageBase64, "image/png"))))
    }
    // Audio content tool
    .tool("test_audio_content", "Tests audio content response") { _ =>
      IO.pure(ToolResult(List(AudioContent(TestAudioBase64, "audio/wav"))))
    }
    // Embedded resource tool
    .tool("test_embedded_resource", "Tests embedded resource content response") { _ =>
      IO.pure(
        ToolResult(
          List(
            ResourceContentRef(
              uri = "test://embedded-resource",
              mimeType = Some("text/plain"),
              text = Some("This is an embedded resource content.")
            )
          )
        )
      )
    }
    // Multiple content types tool
    .tool("test_multiple_content_types", "Tests response with multiple content types") { _ =>
      IO.pure(
        ToolResult(
          List(
            TextContent("Multiple content types test:"),
            ImageContent(TestImageBase64, "image/png"),
            ResourceContentRef(
              uri = "test://mixed-content-resource",
              mimeType = Some("application/json"),
              text = Some("""{"test":"data","value":123}""")
            )
          )
        )
      )
    }
    // Tool with logging
    .toolWithContext("test_tool_with_logging", "Tests tool that emits log messages during execution") {
      (_, ctx) =>
        for
          _ <- ctx.log(LogLevel.Info, "Starting tool execution")
          _ <- ctx.log(LogLevel.Debug, "Processing...")
          _ <- ctx.log(LogLevel.Info, "Completed")
        yield ToolResult(List(TextContent("Tool with logging executed successfully")))
    }
    // Error handling tool - returns isError: true, not an exception
    .tool("test_error_handling", "Tests error response handling") { _ =>
      IO.pure(ToolResult.error("This tool intentionally returns an error for testing"))
    }
    // Progress tool (requires context for progress notifications)
    .toolWithContext("test_tool_with_progress", "Tests tool that reports progress notifications") {
      (_, ctx) =>
        for
          _ <- ctx.progress(0, Some(100))
          _ <- IO.sleep(scala.concurrent.duration.Duration(50, "ms"))
          _ <- ctx.progress(50, Some(100))
          _ <- IO.sleep(scala.concurrent.duration.Duration(50, "ms"))
          _ <- ctx.progress(100, Some(100))
        yield ToolResult(List(TextContent(ctx.requestId.toString)))
    }
    // Sampling tool (requires context for client requests)
    .toolWithContext[SamplingArgs]("test_sampling", "Tests server-initiated sampling") {
      (args, ctx) =>
        if !ctx.sampling.supportsSampling then
          IO.pure(ToolResult(List(TextContent("Sampling not supported by client"))))
        else
          ctx.sampling
            .createMessage(
              CreateMessageParams(
                messages = List(SamplingMessage(Role.User, SamplingTextContent(args.prompt))),
                maxTokens = 100
              )
            )
            .map { result =>
              val responseText = result.content match
                case SamplingTextContent(text) => text
                case _                         => "Unexpected response type"
              ToolResult(List(TextContent(s"LLM response: $responseText")))
            }
            .handleError { err =>
              ToolResult(List(TextContent(s"Sampling error: ${err.getMessage}")))
            }
    }
    // Elicitation tool
    .toolWithContext[ElicitationArgs]("test_elicitation", "Tests server-initiated elicitation") {
      (args, ctx) =>
        ctx.elicitation
          .elicit(
            ElicitFormParams(
              message = args.message,
              requestedSchema = JsonSchema.obj(
                "confirmation" -> JsonSchema.boolean("Confirm action")
              )
            )
          )
          .map { result =>
            result.action match
              case ElicitAction.Accept =>
                ToolResult.text(s"User accepted: ${result.content.getOrElse(Map.empty)}")
              case ElicitAction.Decline =>
                ToolResult.text("User declined")
              case ElicitAction.Cancel =>
                ToolResult.text("User cancelled")
          }
          .handleError { err =>
            ToolResult(List(TextContent(s"Elicitation error: ${err.getMessage}")))
          }
    }
    // SEP-1034: Elicitation with defaults for all primitive types
    .toolWithContext("test_elicitation_sep1034_defaults", "Tests elicitation with default values per SEP-1034") {
      (_, ctx) =>
        ctx.elicitation
          .elicit(
            ElicitFormParams(
              message = "Please confirm your details",
              requestedSchema = JsonSchema.obj(
                Map(
                  "name" -> JsonSchema.stringWithDefault("Your name", "John Doe"),
                  "age" -> JsonSchema.integerWithDefault("Your age", 30),
                  "score" -> JsonSchema.numberWithDefault("Your score", 95.5),
                  "status" -> JsonSchema.stringEnumWithDefault(
                    List("active", "inactive", "pending"),
                    "active",
                    Some("Your status")
                  ),
                  "verified" -> JsonSchema.booleanWithDefault("Verified user", true)
                )
              )
            )
          )
          .map(result => ToolResult.text(s"Elicitation completed: action=${result.action}, content=${result.content.getOrElse(Map.empty)}"))
          .handleError { err =>
            ToolResult(List(TextContent(s"Elicitation error: ${err.getMessage}")))
          }
    }
    // SEP-1330: Elicitation with all 5 enum variants
    .toolWithContext("test_elicitation_sep1330_enums", "Tests elicitation with enum schema per SEP-1330") {
      (_, ctx) =>
        ctx.elicitation
          .elicit(
            ElicitFormParams(
              message = "Select your preferences",
              requestedSchema = JsonSchema.obj(
                Map(
                  // 1. Untitled single-select: { type: "string", enum: [...] }
                  "untitledSingle" -> JsonSchema.stringEnum(
                    List("option1", "option2", "option3"),
                    Some("Untitled single-select")
                  ),
                  // 2. Titled single-select: { type: "string", oneOf: [{ const: "...", title: "..." }, ...] }
                  "titledSingle" -> JsonSchema.titledEnum(
                    List(
                      "value1" -> "First Option",
                      "value2" -> "Second Option",
                      "value3" -> "Third Option"
                    ),
                    Some("Titled single-select")
                  ),
                  // 3. Legacy titled (deprecated): { type: "string", enum: [...], enumNames: [...] }
                  "legacyEnum" -> JsonSchema.legacyTitledEnum(
                    List("opt1", "opt2", "opt3"),
                    List("Option One", "Option Two", "Option Three"),
                    Some("Legacy titled enum")
                  ),
                  // 4. Untitled multi-select: { type: "array", items: { type: "string", enum: [...] } }
                  "untitledMulti" -> JsonSchema.array(
                    JsonSchema.stringEnum(List("option1", "option2", "option3")),
                    Some("Untitled multi-select")
                  ),
                  // 5. Titled multi-select: { type: "array", items: { anyOf: [...] } }
                  "titledMulti" -> JsonSchema.titledMultiSelect(
                    List(
                      "value1" -> "First Choice",
                      "value2" -> "Second Choice",
                      "value3" -> "Third Choice"
                    ),
                    Some("Titled multi-select")
                  )
                )
              )
            )
          )
          .map(result => ToolResult.text(s"Elicitation completed: action=${result.action}, content=${result.content.getOrElse(Map.empty)}"))
          .handleError { err =>
            ToolResult(List(TextContent(s"Elicitation error: ${err.getMessage}")))
          }
    }
    // JSON Schema 2020-12 tool
    .tool(
      "json_schema_2020_12_tool",
      "Tool with JSON Schema 2020-12 features for conformance testing",
      JsonSchema.obj(
        "name"   -> JsonSchema.string("Name field"),
        "street" -> JsonSchema.string("Street address"),
        "city"   -> JsonSchema.string("City name")
      )
    ) { json =>
      IO.pure(ToolResult(List(TextContent(s"JSON Schema 2020-12 tool called with: ${json.noSpaces}"))))
    }
    // SSE reconnection test tool
    .tool("test_reconnection", "Tests SSE stream disconnection and client reconnection") { _ =>
      IO.pure(
        ToolResult(
          List(
            TextContent(
              "Reconnection test completed successfully. If you received this, the client properly reconnected after stream closure."
            )
          )
        )
      )
    }
    // ========================================
    // RESOURCES
    // ========================================
    // Static text resource
    .withResource(
      Resource(
        uri = "test://static-text",
        name = "Static Text Resource",
        description = Some("A static text resource for testing"),
        mimeType = Some("text/plain")
      ),
      _ => IO.pure(ResourceContent.text("test://static-text", "This is the content of the static text resource."))
    )
    // Static binary resource
    .withResource(
      Resource(
        uri = "test://static-binary",
        name = "Static Binary Resource",
        description = Some("A static binary resource (image) for testing"),
        mimeType = Some("image/png")
      ),
      _ => IO.pure(ResourceContent.blob("test://static-binary", TestImageBase64, Some("image/png")))
    )
    // Watched resource
    .withResource(
      Resource(
        uri = "test://watched-resource",
        name = "Watched Resource",
        description = Some("A resource that can be subscribed to"),
        mimeType = Some("text/plain")
      ),
      _ => IO.pure(ResourceContent.text("test://watched-resource", "Watched resource content"))
    )
    // Resource template with handler
    .withResourceTemplate(
      ResourceTemplate(
        uriTemplate = "test://template/{id}/data",
        name = "Resource Template",
        description = Some("A resource template with parameter substitution"),
        mimeType = Some("application/json")
      ),
      uri => {
        // Extract {id} from test://template/{id}/data
        val parts = uri.split("/")
        val id = if parts.length >= 4 then parts(3) else "unknown"
        IO.pure(ResourceContent.text(uri, s"""{"id":"$id","templateTest":true,"data":"Data for ID: $id"}""", Some("application/json")))
      }
    )
    // ========================================
    // PROMPTS
    // ========================================
    // Simple prompt
    .prompt("test_simple_prompt", "A simple prompt without arguments") { _ =>
      IO.pure(
        GetPromptResult(
          description = Some("Simple test prompt"),
          messages = List(
            PromptMessage(Role.User, TextContent("This is a simple prompt for testing."))
          )
        )
      )
    }
    // Prompt with arguments
    .prompt[PromptWithArgsInput]("test_prompt_with_arguments", "A prompt with required arguments") { args =>
      IO.pure(
        GetPromptResult(
          description = Some("Prompt with arguments"),
          messages = List(
            PromptMessage(Role.User, TextContent(s"Prompt with arguments: arg1='${args.arg1}', arg2='${args.arg2}'"))
          )
        )
      )
    }
    // Prompt with embedded resource
    .prompt[EmbeddedResourcePromptInput](
      "test_prompt_with_embedded_resource",
      "A prompt that includes an embedded resource"
    ) { args =>
      IO.pure(
        GetPromptResult(
          description = Some("Prompt with embedded resource"),
          messages = List(
            PromptMessage(
              Role.User,
              ResourceContentRef(
                uri = args.resourceUri,
                mimeType = Some("text/plain"),
                text = Some("Embedded resource content for testing.")
              )
            ),
            PromptMessage(Role.User, TextContent("Please process the embedded resource above."))
          )
        )
      )
    }
    // Prompt with image
    .prompt("test_prompt_with_image", "A prompt that includes image content") { _ =>
      IO.pure(
        GetPromptResult(
          description = Some("Prompt with image"),
          messages = List(
            PromptMessage(Role.User, ImageContent(TestImageBase64, "image/png")),
            PromptMessage(Role.User, TextContent("Please analyze the image above."))
          )
        )
      )
    }
    .build

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    // No auth - simple HTTP server for conformance testing
    val httpConfig = HttpConfig[IO]()
    IO.println("Starting MCP Conformance Test Server on http://localhost:3000") *>
      IO.println("  - MCP endpoint: http://localhost:3000/mcp") *>
      IO.println("  - Health check: http://localhost:3000/health") *>
      HttpTransport.serve[IO](server, httpConfig).useForever
