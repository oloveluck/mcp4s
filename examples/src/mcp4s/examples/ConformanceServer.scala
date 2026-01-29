package mcp4s.examples

import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.mcp
import mcp4s.server.mcp.{ok, error, content, text, messages, textContent, imageContent, audioContent, user, pure}
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

  val TestImageBase64: String =
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8DwHwAFBQIAX8jx0gAAAABJRU5ErkJggg=="

  val TestAudioBase64: String =
    "UklGRiYAAABXQVZFZm10IBAAAAABAAEAQB8AAAB9AAACABAAZGF0YQIAAAA="

  case class SamplingArgs(prompt: String) derives ToolInput

  case class ElicitationArgs(message: String) derives ToolInput

  case class PromptWithArgsInput(
      @description("First test argument") arg1: String,
      @description("Second test argument") arg2: String
  ) derives PromptInput

  case class EmbeddedResourcePromptInput(
      @description("URI of the resource to embed") resourceUri: String
  ) derives PromptInput

  val simpleTools: McpTools[IO] =
    mcp.Tool[IO]("test_simple_text", "Tests simple text content response") {
      ok("This is a simple text response for testing.").pure[IO]
    } |+|
    mcp.Tool[IO]("test_image_content", "Tests image content response") {
      content(imageContent(TestImageBase64, "image/png")).pure[IO]
    } |+|
    mcp.Tool[IO]("test_audio_content", "Tests audio content response") {
      content(audioContent(TestAudioBase64, "audio/wav")).pure[IO]
    } |+|
    mcp.Tool[IO]("test_embedded_resource", "Tests embedded resource content response") {
      ToolResult(List(
        ResourceContentRef(
          uri = "test://embedded-resource",
          mimeType = Some("text/plain"),
          text = Some("This is an embedded resource content.")
        )
      )).pure[IO]
    } |+|
    mcp.Tool[IO]("test_multiple_content_types", "Tests response with multiple content types") {
      ToolResult(List(
        textContent("Multiple content types test:"),
        imageContent(TestImageBase64, "image/png"),
        ResourceContentRef(
          uri = "test://mixed-content-resource",
          mimeType = Some("application/json"),
          text = Some("""{"test":"data","value":123}""")
        )
      )).pure[IO]
    } |+|
    mcp.Tool[IO]("test_error_handling", "Tests error response handling") {
      error("This tool intentionally returns an error for testing").pure[IO]
    } |+|
    mcp.Tool[IO]("test_reconnection", "Tests SSE stream disconnection and client reconnection") {
      ok("Reconnection test completed successfully. If you received this, the client properly reconnected after stream closure.").pure[IO]
    }

  val contextTools: McpTools[IO] =
    mcp.Tool.withContext[IO]("test_tool_with_logging", "Tests tool that emits log messages during execution") { ctx =>
      for
        _ <- ctx.log(LogLevel.Info, "Starting tool execution")
        _ <- ctx.log(LogLevel.Debug, "Processing...")
        _ <- ctx.log(LogLevel.Info, "Completed")
      yield ok("Tool with logging executed successfully")
    } |+|
    mcp.Tool.withContext[IO]("test_tool_with_progress", "Tests tool that reports progress notifications") { ctx =>
      for
        _ <- ctx.progress(0, Some(100))
        _ <- IO.sleep(scala.concurrent.duration.Duration(50, "ms"))
        _ <- ctx.progress(50, Some(100))
        _ <- IO.sleep(scala.concurrent.duration.Duration(50, "ms"))
        _ <- ctx.progress(100, Some(100))
      yield ok(ctx.requestId.toString)
    } |+|
    mcp.Tool.withContext[IO, SamplingArgs]("test_sampling", "Tests server-initiated sampling") { (args, ctx) =>
      if !ctx.sampling.supportsSampling then
        ok("Sampling not supported by client").pure[IO]
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
            ok(s"LLM response: $responseText")
          }
          .handleError { err =>
            ok(s"Sampling error: ${err.getMessage}")
          }
    } |+|
    mcp.Tool.withContext[IO, ElicitationArgs]("test_elicitation", "Tests server-initiated elicitation") { (args, ctx) =>
      ctx.elicitation
        .elicit(
          ElicitFormParams(
            message = args.message,
            requestedSchema = JsonSchema.obj("confirmation" -> JsonSchema.boolean("Confirm action"))
          )
        )
        .map { result =>
          result.action match
            case ElicitAction.Accept  => ok(s"User accepted: ${result.content.getOrElse(Map.empty)}")
            case ElicitAction.Decline => ok("User declined")
            case ElicitAction.Cancel  => ok("User cancelled")
        }
        .handleError { err =>
          ok(s"Elicitation error: ${err.getMessage}")
        }
    } |+|
    mcp.Tool.withContext[IO]("test_elicitation_sep1034_defaults", "Tests elicitation with default values per SEP-1034") { ctx =>
      ctx.elicitation
        .elicit(
          ElicitFormParams(
            message = "Please confirm your details",
            requestedSchema = JsonSchema.obj(
              Map(
                "name"     -> JsonSchema.stringWithDefault("Your name", "John Doe"),
                "age"      -> JsonSchema.integerWithDefault("Your age", 30),
                "score"    -> JsonSchema.numberWithDefault("Your score", 95.5),
                "status"   -> JsonSchema.stringEnumWithDefault(List("active", "inactive", "pending"), "active", Some("Your status")),
                "verified" -> JsonSchema.booleanWithDefault("Verified user", true)
              )
            )
          )
        )
        .map(result => ok(s"Elicitation completed: action=${result.action}, content=${result.content.getOrElse(Map.empty)}"))
        .handleError(err => ok(s"Elicitation error: ${err.getMessage}"))
    } |+|
    mcp.Tool.withContext[IO]("test_elicitation_sep1330_enums", "Tests elicitation with enum schema per SEP-1330") { ctx =>
      ctx.elicitation
        .elicit(
          ElicitFormParams(
            message = "Select your preferences",
            requestedSchema = JsonSchema.obj(
              Map(
                "untitledSingle" -> JsonSchema.stringEnum(List("option1", "option2", "option3"), Some("Untitled single-select")),
                "titledSingle" -> JsonSchema.titledEnum(
                  List("value1" -> "First Option", "value2" -> "Second Option", "value3" -> "Third Option"),
                  Some("Titled single-select")
                ),
                "legacyEnum" -> JsonSchema.legacyTitledEnum(
                  List("opt1", "opt2", "opt3"),
                  List("Option One", "Option Two", "Option Three"),
                  Some("Legacy titled enum")
                ),
                "untitledMulti" -> JsonSchema.array(
                  JsonSchema.stringEnum(List("option1", "option2", "option3")),
                  Some("Untitled multi-select")
                ),
                "titledMulti" -> JsonSchema.titledMultiSelect(
                  List("value1" -> "First Choice", "value2" -> "Second Choice", "value3" -> "Third Choice"),
                  Some("Titled multi-select")
                )
              )
            )
          )
        )
        .map(result => ok(s"Elicitation completed: action=${result.action}, content=${result.content.getOrElse(Map.empty)}"))
        .handleError(err => ok(s"Elicitation error: ${err.getMessage}"))
    }

  val allTools: McpTools[IO] = simpleTools |+| contextTools

  val staticResources: McpResources[IO] =
    mcp.Resource.text[IO]("test://static-text", "Static Text Resource") {
      "This is the content of the static text resource."
    } |+|
    mcp.Resource.text[IO]("test://watched-resource", "Watched Resource") {
      "Watched resource content"
    }

  val binaryResource: McpResources[IO] =
    mcp.Resource[IO]("test://static-binary", "Static Binary Resource") {
      IO.pure(ResourceContent.blob("test://static-binary", TestImageBase64, Some("image/png")))
    }

  val templateResource: McpResources[IO] =
    mcp.Resource.template[IO]("test://template/{id}/data", "Resource Template", "A resource template with parameter substitution") { uri =>
      val parts = uri.split("/")
      val id = if parts.length >= 4 then parts(3) else "unknown"
      IO.pure(text(uri, s"""{"id":"$id","templateTest":true,"data":"Data for ID: $id"}"""))
    }

  val allResources: McpResources[IO] = staticResources |+| binaryResource |+| templateResource

  val simplePrompts: McpPrompts[IO] =
    mcp.Prompt.withDesc[IO]("test_simple_prompt", "A simple prompt without arguments", "Simple test prompt")(
      user("This is a simple prompt for testing.")
    ) |+|
    mcp.Prompt.withDesc[IO]("test_prompt_with_image", "A prompt that includes image content", "Prompt with image")(
      user(imageContent(TestImageBase64, "image/png")),
      user("Please analyze the image above.")
    )

  val argPrompts: McpPrompts[IO] =
    mcp.Prompt[IO, PromptWithArgsInput]("test_prompt_with_arguments", "A prompt with required arguments") { args =>
      messages("Prompt with arguments")(
        user(s"Prompt with arguments: arg1='${args.arg1}', arg2='${args.arg2}'")
      ).pure[IO]
    } |+|
    mcp.Prompt[IO, EmbeddedResourcePromptInput]("test_prompt_with_embedded_resource", "A prompt that includes an embedded resource") { args =>
      GetPromptResult(
        description = Some("Prompt with embedded resource"),
        messages = List(
          PromptMessage(
            Role.User,
            ResourceContentRef(uri = args.resourceUri, mimeType = Some("text/plain"), text = Some("Embedded resource content for testing."))
          ),
          PromptMessage(Role.User, textContent("Please process the embedded resource above."))
        )
      ).pure[IO]
    }

  val allPrompts: McpPrompts[IO] = simplePrompts |+| argPrompts

  val server: McpServer[IO] = McpServer
    .builder[IO]
    .withInfo(ServerInfo("mcp-conformance-test-server", "1.0.0"))
    .withTools(allTools)
    .withResources(allResources)
    .withPrompts(allPrompts)
    .tool(
      "json_schema_2020_12_tool",
      "Tool with JSON Schema 2020-12 features for conformance testing",
      JsonSchema.obj(
        "name"   -> JsonSchema.string("Name field"),
        "street" -> JsonSchema.string("Street address"),
        "city"   -> JsonSchema.string("City name")
      )
    ) { json =>
      ok(s"JSON Schema 2020-12 tool called with: ${json.noSpaces}").pure[IO]
    }
    .build

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    val httpConfig = HttpConfig[IO]()
    IO.println("Starting MCP Conformance Test Server on http://localhost:3000") *>
      IO.println("  - MCP endpoint: http://localhost:3000/mcp") *>
      IO.println("  - Health check: http://localhost:3000/health") *>
      HttpTransport.serve[IO](server, httpConfig).useForever
