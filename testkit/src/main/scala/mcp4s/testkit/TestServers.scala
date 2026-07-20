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

package mcp4s.testkit

import cats.effect.{Async, Ref, Temporal}
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*
import mcp4s.protocol.Resource as McpResource
import mcp4s.server.*
import scodec.bits.ByteVector

import scala.concurrent.duration.*

/** Configurable test servers for integration testing.
  *
  * These servers wrap a base server to add behaviors like:
  *   - Artificial delays
  *   - Configurable failures
  *   - Call counting
  *   - Random chaos
  */
object TestServers:

  /** Wrap a server to fail tool calls after N successful calls.
    *
    * @param base
    *   The server to wrap
    * @param failAfter
    *   Number of successful calls before failing
    * @param errorMessage
    *   Error message for failures
    */
  def failingAfter[F[_]: Async](
      base: Server[F],
      failAfter: Int,
      errorMessage: String = "Simulated failure"
  ): F[(Server[F], F[Int])] =
    Ref
      .of[F, Int](0)
      .map: counterRef =>
        val server = new Server[F]:
          val info: ServerInfo                 = base.info
          val capabilities: ServerCapabilities = base.capabilities

          def listTools: F[List[Tool]] = base.listTools

          def callTool(name: String, arguments: Json): F[ToolResult] =
            counterRef
              .getAndUpdate(_ + 1)
              .flatMap: count =>
                if count >= failAfter then Async[F].raiseError(new RuntimeException(errorMessage))
                else base.callTool(name, arguments)

          def listResources: F[List[McpResource]]              = base.listResources
          def listResourceTemplates: F[List[ResourceTemplate]] = base.listResourceTemplates
          def readResource(uri: String): F[ResourceContent]    = base.readResource(uri)
          def listPrompts: F[List[Prompt]]                     = base.listPrompts
          def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
            base.getPrompt(name, arguments)

        (server, counterRef.get)

  /** Wrap a server that tracks call counts for each operation.
    *
    * @param base
    *   The server to wrap
    * @return
    *   Server and effect to get current call counts
    */
  def counting[F[_]: Async](base: Server[F]): F[(Server[F], F[CallCounts])] =
    for
      toolCallCount      <- Ref.of[F, Int](0)
      listToolsCount     <- Ref.of[F, Int](0)
      resourceReadCount  <- Ref.of[F, Int](0)
      listResourcesCount <- Ref.of[F, Int](0)
      promptGetCount     <- Ref.of[F, Int](0)
      listPromptsCount   <- Ref.of[F, Int](0)
    yield
      val server = new Server[F]:
        val info: ServerInfo                 = base.info
        val capabilities: ServerCapabilities = base.capabilities

        def listTools: F[List[Tool]] =
          listToolsCount.update(_ + 1) *> base.listTools

        def callTool(name: String, arguments: Json): F[ToolResult] =
          toolCallCount.update(_ + 1) *> base.callTool(name, arguments)

        def listResources: F[List[McpResource]] =
          listResourcesCount.update(_ + 1) *> base.listResources

        def listResourceTemplates: F[List[ResourceTemplate]] =
          base.listResourceTemplates

        def readResource(uri: String): F[ResourceContent] =
          resourceReadCount.update(_ + 1) *> base.readResource(uri)

        def listPrompts: F[List[Prompt]] =
          listPromptsCount.update(_ + 1) *> base.listPrompts

        def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
          promptGetCount.update(_ + 1) *> base.getPrompt(name, arguments)

      val getCounts = for
        tc <- toolCallCount.get
        lt <- listToolsCount.get
        rr <- resourceReadCount.get
        lr <- listResourcesCount.get
        pg <- promptGetCount.get
        lp <- listPromptsCount.get
      yield CallCounts(tc, lt, rr, lr, pg, lp)

      (server, getCounts)

  /** Wrap a server with jittered response delays (chaos testing).
    *
    * @param base
    *   The server to wrap
    * @param minDelay
    *   Minimum delay
    * @param maxDelay
    *   Maximum delay
    */
  def jittered[F[_]: Async: Temporal](
      base: Server[F],
      minDelay: FiniteDuration,
      maxDelay: FiniteDuration
  ): F[Server[F]] =
    cats.effect.std.Random
      .scalaUtilRandom[F]
      .map(random =>
        new Server[F]:
          val info: ServerInfo                 = base.info
          val capabilities: ServerCapabilities = base.capabilities

          private def randomDelay: F[Unit] =
            val range = (maxDelay - minDelay).toMillis
            random
              .betweenLong(minDelay.toMillis, minDelay.toMillis + range.max(1))
              .flatMap(delayMs => Temporal[F].sleep(delayMs.millis))

          def listTools: F[List[Tool]]                               = randomDelay *> base.listTools
          def callTool(name: String, arguments: Json): F[ToolResult] =
            randomDelay *> base.callTool(name, arguments)
          def listResources: F[List[McpResource]]              = randomDelay *> base.listResources
          def listResourceTemplates: F[List[ResourceTemplate]] =
            randomDelay *> base.listResourceTemplates
          def readResource(uri: String): F[ResourceContent] = randomDelay *> base.readResource(uri)
          def listPrompts: F[List[Prompt]]                  = randomDelay *> base.listPrompts
          def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
            randomDelay *> base.getPrompt(name, arguments)
      )

  /** Create a simple test server with basic tools, resources, and prompts.
    *
    * Tools:
    *   - add(a, b): Add two numbers
    *   - slow_add(a, b): Add with 200ms delay
    *   - echo(message): Echo a message
    *
    * Resources:
    *   - file:///test.txt: A text file
    *   - file:///binary.bin: A binary file (base64)
    *
    * Prompts:
    *   - greeting(name): A greeting prompt
    */
  def simple[F[_]: Async: Temporal]: Server[F] =
    val addTool = Tool(
      name = "add",
      description = Some("Add two numbers"),
      inputSchema = JsonSchema.obj(
        Map(
          "a" -> JsonSchema.number(Some("First number")),
          "b" -> JsonSchema.number(Some("Second number"))
        ),
        List("a", "b")
      )
    )

    val slowAddTool = Tool(
      name = "slow_add",
      description = Some("Add two numbers with delay"),
      inputSchema = JsonSchema.obj(
        Map(
          "a" -> JsonSchema.number(Some("First number")),
          "b" -> JsonSchema.number(Some("Second number"))
        ),
        List("a", "b")
      )
    )

    val echoTool = Tool(
      name = "echo",
      description = Some("Echo a message"),
      inputSchema = JsonSchema.obj(
        Map("message" -> JsonSchema.string(Some("Message to echo"))),
        List("message")
      )
    )

    val failTool = Tool(
      name = "fail",
      description = Some("Always fails"),
      inputSchema = JsonSchema.obj(Map.empty, Nil)
    )

    val textResource = mcp4s.protocol.Resource(
      uri = "file:///test.txt",
      name = "Test File",
      mimeType = Some("text/plain")
    )

    val binaryResource = mcp4s.protocol.Resource(
      uri = "file:///binary.bin",
      name = "Binary File",
      mimeType = Some("application/octet-stream")
    )

    val greetingPrompt = Prompt(
      name = "greeting",
      description = Some("A greeting prompt"),
      arguments = List(PromptArgument("name", Some("Name to greet"), required = true))
    )

    new Server[F]:
      val info: ServerInfo                 = ServerInfo("test-server", "1.0.0")
      val capabilities: ServerCapabilities = ServerCapabilities(
        tools = Some(ToolsCapability()),
        resources = Some(ResourcesCapability()),
        prompts = Some(PromptsCapability())
      )

      def listTools: F[List[Tool]] = Async[F].pure(List(addTool, slowAddTool, echoTool, failTool))

      def callTool(name: String, arguments: Json): F[ToolResult] =
        val cursor = arguments.hcursor
        name match
          case "add" =>
            for
              a <- cursor.get[Double]("a").liftTo[F]
              b <- cursor.get[Double]("b").liftTo[F]
            yield ToolResult.text(s"${a + b}")

          case "slow_add" =>
            for
              a <- cursor.get[Double]("a").liftTo[F]
              b <- cursor.get[Double]("b").liftTo[F]
              _ <- Temporal[F].sleep(200.millis)
            yield ToolResult.text(s"${a + b}")

          case "echo" =>
            cursor.get[String]("message").liftTo[F].map(msg => ToolResult.text(msg))

          case "fail" =>
            Async[F].raiseError(new RuntimeException("Intentional failure"))

          case other =>
            Async[F].raiseError(McpError.ToolNotFound(other))

      def listResources: F[List[mcp4s.protocol.Resource]] =
        Async[F].pure(List(textResource, binaryResource))

      def listResourceTemplates: F[List[ResourceTemplate]] =
        Async[F].pure(
          List(
            ResourceTemplate(
              uriTemplate = "file:///docs/{name}",
              name = "Documents",
              description = Some("Access documents by name")
            )
          )
        )

      def readResource(uri: String): F[ResourceContent] =
        uri match
          case "file:///test.txt" =>
            Async[F].pure(ResourceContent.text("file:///test.txt", "Hello, World!"))
          case "file:///binary.bin" =>
            Async[F].pure(
              ResourceContent(
                uri = "file:///binary.bin",
                mimeType = Some("application/octet-stream"),
                blob = Some(ByteVector(0, 1, 2, 3).toBase64)
              )
            )
          case other =>
            Async[F].raiseError(McpError.ResourceNotFound(other))

      def listPrompts: F[List[Prompt]] =
        Async[F].pure(List(greetingPrompt))

      def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
        name match
          case "greeting" =>
            val nameArg = arguments.getOrElse("name", "World")
            Async[F].pure(
              GetPromptResult(
                Some("A friendly greeting"),
                List(PromptMessage(Role.User, TextContent(s"Hello, $nameArg!")))
              )
            )
          case other =>
            Async[F].raiseError(McpError.PromptNotFound(other))

  /** [[simple]] plus a context-aware `count` tool that emits three `notifications/progress` before
    * returning. Use this (with a `progressTool = ToolProbe("count")`) to exercise the
    * progress-callback compliance check end-to-end.
    */
  def withProgress[F[_]: Async: Temporal]: Server[F] =
    val base      = simple[F]
    val countTool = Tool(
      name = "count",
      description = Some("Counts to 3, emitting a progress notification each step"),
      inputSchema = JsonSchema.obj(Map.empty, Nil)
    )
    new Server[F]:
      val info: ServerInfo                 = base.info.copy(name = "test-progress-server")
      val capabilities: ServerCapabilities = base.capabilities

      def listTools: F[List[Tool]] = base.listTools.map(_ :+ countTool)

      def callTool(name: String, arguments: Json): F[ToolResult] =
        if name == "count" then Async[F].pure(ToolResult.text("done"))
        else base.callTool(name, arguments)

      override def callToolWithContext(
          name: String,
          arguments: Json,
          context: ToolContext[F]
      ): F[ToolResult] =
        if name == "count" then
          context.progress(1, Some(3)) *>
            context.progress(2, Some(3)) *>
            context.progress(3, Some(3)).as(ToolResult.text("done"))
        else base.callTool(name, arguments)

      def listResources: F[List[McpResource]]              = base.listResources
      def listResourceTemplates: F[List[ResourceTemplate]] = base.listResourceTemplates
      def readResource(uri: String): F[ResourceContent]    = base.readResource(uri)
      def listPrompts: F[List[Prompt]]                     = base.listPrompts
      def getPrompt(name: String, arguments: Map[String, String]): F[GetPromptResult] =
        base.getPrompt(name, arguments)

/** Call count statistics for a counting server. */
final case class CallCounts(
    toolCalls: Int,
    listTools: Int,
    resourceReads: Int,
    listResources: Int,
    promptGets: Int,
    listPrompts: Int
):
  def total: Int = toolCalls + listTools + resourceReads + listResources + promptGets + listPrompts
