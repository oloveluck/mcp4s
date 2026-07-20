/*
 * Copyright 2025 MCP4S Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mcp4s.examples

import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import mcp4s.protocol.*
import mcp4s.schema.Schema
import mcp4s.server.*
import mcp4s.server.transport.*
import org.typelevel.otel4s.trace.Tracer

/** MCP Conformance Test Server
  *
  * Implements all tools, resources, and prompts required by the official MCP Conformance Test
  * Framework.
  *
  * Run with: mill examples.runMain mcp4s.examples.ConformanceServer
  *
  * Then run conformance tests: mill conformance --url http://localhost:3000/mcp
  */
object ConformanceServer extends IOApp.Simple:

  import mcp4s.server.dsl.*

  val TestImageBase64: String =
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8DwHwAFBQIAX8jx0gAAAABJRU5ErkJggg=="

  val TestAudioBase64: String =
    "UklGRiYAAABXQVZFZm10IBAAAAABAAEAQB8AAAB9AAACABAAZGF0YQIAAAA="

  case class SamplingArgs(prompt: String) derives Schema

  case class ElicitationArgs(message: String) derives Schema

  case class PromptWithArgsInput(
      @description("First test argument") arg1: String,
      @description("Second test argument") arg2: String
  ) derives Schema

  case class EmbeddedResourcePromptInput(
      @description("URI of the resource to embed") resourceUri: String
  ) derives Schema

  val simpleTools: Tools[IO] =
    Tool("test_simple_text").withDescription("Tests simple text content response").handle[IO] { _ =>
      IO.pure(ok("This is a simple text response for testing."))
    } |+|
      Tool("test_image_content")
        .withDescription("Tests image content response")
        .handleWith[IO] { (_, _) =>
          content(imageContent(TestImageBase64, "image/png")).pure[IO]
        } |+|
      Tool("test_audio_content")
        .withDescription("Tests audio content response")
        .handleWith[IO] { (_, _) =>
          content(audioContent(TestAudioBase64, "audio/wav")).pure[IO]
        } |+|
      Tool("test_embedded_resource")
        .withDescription("Tests embedded resource content response")
        .handleWith[IO] { (_, _) =>
          ToolResult(
            List(
              ResourceContentRef(
                uri = "test://embedded-resource",
                mimeType = Some("text/plain"),
                text = Some("This is an embedded resource content.")
              )
            )
          ).pure[IO]
        } |+|
      Tool("test_multiple_content_types")
        .withDescription("Tests response with multiple content types")
        .handleWith[IO] { (_, _) =>
          ToolResult(
            List(
              textContent("Multiple content types test:"),
              imageContent(TestImageBase64, "image/png"),
              ResourceContentRef(
                uri = "test://mixed-content-resource",
                mimeType = Some("application/json"),
                text = Some("""{"test":"data","value":123}""")
              )
            )
          ).pure[IO]
        } |+|
      Tool("test_error_handling")
        .withDescription("Tests error response handling")
        .handleWith[IO] { (_, _) =>
          error("This tool intentionally returns an error for testing").pure[IO]
        } |+|
      Tool("test_reconnection")
        .withDescription("Tests SSE stream disconnection and client reconnection")
        .handle[IO] { _ =>
          IO.pure(
            ok(
              "Reconnection test completed successfully. If you received this, the client properly reconnected after stream closure."
            )
          )
        }

  val contextTools: Tools[IO] =
    Tool("test_tool_with_logging")
      .withDescription("Tests tool that emits log messages during execution")
      .handleWith[IO] { (_, ctx) =>
        for
          _ <- ctx.log(LogLevel.Info, "Starting tool execution")
          _ <- ctx.log(LogLevel.Debug, "Processing...")
          _ <- ctx.log(LogLevel.Info, "Completed")
        yield ok("Tool with logging executed successfully")
      } |+|
      Tool("test_tool_with_progress")
        .withDescription("Tests tool that reports progress notifications")
        .handleWith[IO] { (_, ctx) =>
          for
            _ <- ctx.progress(0, Some(100))
            _ <- IO.sleep(scala.concurrent.duration.Duration(50, "ms"))
            _ <- ctx.progress(50, Some(100))
            _ <- IO.sleep(scala.concurrent.duration.Duration(50, "ms"))
            _ <- ctx.progress(100, Some(100))
          yield ok(ctx.requestId.toString)
        } |+|
      Tool("test_sampling")
        .withDescription("Tests server-initiated sampling")
        .input[SamplingArgs]
        .handleWith[IO] { (args, ctx) =>
          if !ctx.sampling.supportsSampling then ok("Sampling not supported by client").pure[IO]
          else
            ctx.sampling
              .createMessage(
                CreateMessageParams(
                  messages = List(SamplingMessage(Role.User, SamplingTextContent(args.prompt))),
                  maxTokens = 100
                )
              )
              .map: result =>
                val responseText = result.content match
                  case SamplingTextContent(text) => text
                  case _                         => "Unexpected response type"
                ok(s"LLM response: $responseText")
              .handleError { err =>
                ok(s"Sampling error: ${err.getMessage}")
              }
        } |+|
      Tool("test_elicitation")
        .withDescription("Tests server-initiated elicitation")
        .input[ElicitationArgs]
        .handleWith[IO] { (args, ctx) =>
          ctx.elicitation
            .elicit(
              ElicitFormParams(
                message = args.message,
                requestedSchema =
                  JsonSchema.obj("confirmation" -> JsonSchema.boolean("Confirm action"))
              )
            )
            .map: result =>
              result.action match
                case ElicitAction.Accept =>
                  ok(s"User accepted: ${result.content.getOrElse(Map.empty)}")
                case ElicitAction.Decline => ok("User declined")
                case ElicitAction.Cancel  => ok("User cancelled")
            .handleError { err =>
              ok(s"Elicitation error: ${err.getMessage}")
            }
        } |+|
      Tool("test_elicitation_sep1034_defaults")
        .withDescription("Tests elicitation with default values per SEP-1034")
        .handleWith[IO] { (_, ctx) =>
          ctx.elicitation
            .elicit(
              ElicitFormParams(
                message = "Please confirm your details",
                requestedSchema = JsonSchema.obj(
                  Map(
                    "name"  -> JsonSchema.stringWithDefault("Your name", "John Doe"),
                    "age"   -> JsonSchema.integerWithDefault("Your age", 30),
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
            .map(result =>
              ok(
                s"Elicitation completed: action=${result.action}, content=${result.content.getOrElse(Map.empty)}"
              )
            )
            .handleError(err => ok(s"Elicitation error: ${err.getMessage}"))
        } |+|
      Tool("test_elicitation_sep1330_enums")
        .withDescription("Tests elicitation with enum schema per SEP-1330")
        .handleWith[IO] { (_, ctx) =>
          ctx.elicitation
            .elicit(
              ElicitFormParams(
                message = "Select your preferences",
                requestedSchema = JsonSchema.obj(
                  Map(
                    "untitledSingle" -> JsonSchema.stringEnum(
                      List("option1", "option2", "option3"),
                      Some("Untitled single-select")
                    ),
                    "titledSingle" -> JsonSchema.titledEnum(
                      List(
                        "value1" -> "First Option",
                        "value2" -> "Second Option",
                        "value3" -> "Third Option"
                      ),
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
            .map(result =>
              ok(
                s"Elicitation completed: action=${result.action}, content=${result.content.getOrElse(Map.empty)}"
              )
            )
            .handleError(err => ok(s"Elicitation error: ${err.getMessage}"))
        }

  val allTools: Tools[IO] = simpleTools |+| contextTools

  val staticResources: Resources[IO] =
    Resource.text[IO]("test://static-text", "Static Text Resource") {
      "This is the content of the static text resource."
    } |+|
      Resource.text[IO]("test://watched-resource", "Watched Resource") {
        "Watched resource content"
      }

  val binaryResource: Resources[IO] =
    Resource[IO]("test://static-binary", "Static Binary Resource") {
      IO.pure(ResourceContent.blob("test://static-binary", TestImageBase64, Some("image/png")))
    }

  val templateResource: Resources[IO] =
    Resource.template[IO](
      "test://template/{id}/data",
      "Resource Template",
      "A resource template with parameter substitution"
    ) { uri =>
      val parts = uri.split("/")
      val id    = if parts.length >= 4 then parts(3) else "unknown"
      IO.pure(text(uri, s"""{"id":"$id","templateTest":true,"data":"Data for ID: $id"}"""))
    }

  val allResources: Resources[IO] = staticResources |+| binaryResource |+| templateResource

  val simplePrompts: Prompts[IO] =
    Prompt("test_simple_prompt")
      .withDescription("A simple prompt without arguments")
      .static[IO](
        messages("Simple test prompt")(
          user("This is a simple prompt for testing.")
        )
      ) |+|
      Prompt("test_prompt_with_image")
        .withDescription("A prompt that includes image content")
        .static[IO](
          messages("Prompt with image")(
            user(imageContent(TestImageBase64, "image/png")),
            user("Please analyze the image above.")
          )
        )

  val argPrompts: Prompts[IO] =
    Prompt("test_prompt_with_arguments")
      .withDescription("A prompt with required arguments")
      .input[PromptWithArgsInput]
      .handle[IO] { args =>
        messages("Prompt with arguments")(
          user(s"Prompt with arguments: arg1='${args.arg1}', arg2='${args.arg2}'")
        ).pure[IO]
      } |+|
      Prompt("test_prompt_with_embedded_resource")
        .withDescription("A prompt that includes an embedded resource")
        .input[EmbeddedResourcePromptInput]
        .handle[IO] { args =>
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
              PromptMessage(Role.User, textContent("Please process the embedded resource above."))
            )
          ).pure[IO]
        }

  val allPrompts: Prompts[IO] = simplePrompts |+| argPrompts

  val jsonSchemaTool: Tools[IO] =
    Tools.single[IO](
      mcp4s.protocol.Tool(
        "json_schema_2020_12_tool",
        Some("Tool with JSON Schema 2020-12 features for conformance testing"),
        JsonSchema.obj(
          "name"   -> JsonSchema.string("Name field"),
          "street" -> JsonSchema.string("Street address"),
          "city"   -> JsonSchema.string("City name")
        )
      )
    ) { json =>
      ok(s"JSON Schema 2020-12 tool called with: ${json.noSpaces}").pure[IO]
    }

  val server: Server[IO] =
    Server.from[IO](
      ServerInfo("mcp-conformance-test-server", "1.0.0"),
      allTools |+| jsonSchemaTool,
      allResources,
      allPrompts
    )

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    val httpConfig   = HttpConfig[IO]()
    IO.println("Starting MCP Conformance Test Server on http://localhost:3000") *>
      IO.println("  - MCP endpoint: http://localhost:3000/mcp") *>
      IO.println("  - Health check: http://localhost:3000/health") *>
      HttpTransport.serve[IO](server, httpConfig).useForever
