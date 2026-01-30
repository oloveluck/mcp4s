package mcp4s.client

import cats.Applicative
import cats.effect.Concurrent
import io.circe.Json
import mcp4s.protocol.*

/** Unified DSL for MCP client construction.
  *
  * This object provides a single import (`import mcp4s.client.mcp.*`) that gives
  * access to all DSL constructors for building MCP clients with minimal boilerplate.
  *
  * Example usage:
  * {{{
  * import mcp4s.client.mcp.*
  *
  * val sampling = Sampling[IO] { params =>
  *   message("Hello from LLM", "mock-model").pure[IO]
  * }
  *
  * val elicitation = Elicitation[IO] { params =>
  *   accept(Map("confirmed" -> Json.fromBoolean(true))).pure[IO]
  * }
  *
  * val roots = Roots[IO](
  *   Root("file:///workspace", Some("Workspace")),
  *   Root("file:///home", Some("Home"))
  * )
  *
  * val client = McpClient.from[IO](
  *   info = ClientInfo("my-client", "1.0.0"),
  *   roots = Some(roots),
  *   sampling = Some(sampling),
  *   elicitation = Some(elicitation)
  * )
  * }}}
  */
object mcp:

  // === Result Builders ===

  /** Create a sampling result with text content.
    *
    * Example:
    * {{{
    * val result = message("Hello!", "gpt-4")
    * // CreateMessageResult with role=Assistant, text content, endTurn
    * }}}
    */
  def message(text: String, model: String = "default"): CreateMessageResult =
    CreateMessageResult(
      role = Role.Assistant,
      content = SamplingTextContent(text),
      model = model,
      stopReason = Some("endTurn")
    )

  /** Create a sampling result with image content.
    *
    * Example:
    * {{{
    * val result = messageImage(base64Data, "image/png", "gpt-4-vision")
    * }}}
    */
  def messageImage(data: String, mimeType: String, model: String = "default"): CreateMessageResult =
    CreateMessageResult(
      role = Role.Assistant,
      content = SamplingImageContent(data, mimeType),
      model = model,
      stopReason = Some("endTurn")
    )

  /** Create a sampling result with audio content.
    *
    * Example:
    * {{{
    * val result = messageAudio(base64Data, "audio/wav", "whisper-1")
    * }}}
    */
  def messageAudio(data: String, mimeType: String, model: String = "default"): CreateMessageResult =
    CreateMessageResult(
      role = Role.Assistant,
      content = SamplingAudioContent(data, mimeType),
      model = model,
      stopReason = Some("endTurn")
    )

  /** Create an accept elicitation result with form values.
    *
    * Example:
    * {{{
    * val result = accept(Map("name" -> Json.fromString("Alice")))
    * }}}
    */
  def accept(content: Map[String, Json]): ElicitResult =
    ElicitResult(ElicitAction.Accept, Some(content))

  /** Create an accept elicitation result without content.
    *
    * Example:
    * {{{
    * val result = accept
    * }}}
    */
  def accept: ElicitResult =
    ElicitResult(ElicitAction.Accept, None)

  /** Create a decline elicitation result.
    *
    * Example:
    * {{{
    * val result = decline
    * }}}
    */
  def decline: ElicitResult =
    ElicitResult(ElicitAction.Decline, None)

  /** Create a cancel elicitation result.
    *
    * Example:
    * {{{
    * val result = cancel
    * }}}
    */
  def cancel: ElicitResult =
    ElicitResult(ElicitAction.Cancel, None)

  // === Handler Constructors ===

  /** Namespaced sampling handler constructors. */
  object Sampling:

    /** Create a sampling handler from a function.
      *
      * Example:
      * {{{
      * val handler = Sampling[IO] { params =>
      *   val response = processMessages(params.messages)
      *   IO.pure(message(response, "my-model"))
      * }
      * }}}
      */
    def apply[F[_]: Concurrent](handler: CreateMessageParams => F[CreateMessageResult]): McpSamplings[F] =
      McpSamplings[F](handler)

    /** Create an empty sampling handler that handles nothing. */
    def empty[F[_]: Applicative]: McpSamplings[F] =
      McpSamplings.empty[F]

  /** Namespaced elicitation handler constructors. */
  object Elicitation:

    /** Create an elicitation handler from a function.
      *
      * Example:
      * {{{
      * val handler = Elicitation[IO] { params =>
      *   IO.pure(accept(Map("confirmed" -> Json.fromBoolean(true))))
      * }
      * }}}
      */
    def apply[F[_]: Concurrent](handler: ElicitParams => F[ElicitResult]): McpElicitations[F] =
      McpElicitations[F](handler)

    /** Create an elicitation handler with a complete notification handler.
      *
      * Example:
      * {{{
      * val handler = Elicitation.withComplete[IO](
      *   handler = params => IO.pure(accept),
      *   onComplete = params => IO.println(s"Elicitation ${params.elicitationId} completed")
      * )
      * }}}
      */
    def withComplete[F[_]: Concurrent](
        handler: ElicitParams => F[ElicitResult],
        onComplete: ElicitationCompleteParams => F[Unit]
    ): McpElicitations[F] =
      McpElicitations.withComplete[F](handler, onComplete)

    /** Create an empty elicitation handler that handles nothing. */
    def empty[F[_]: Applicative]: McpElicitations[F] =
      McpElicitations.empty[F]

  /** Namespaced roots constructors. */
  object Roots:

    /** Create a roots provider from varargs of roots.
      *
      * Example:
      * {{{
      * val roots = Roots[IO](
      *   Root("file:///workspace", Some("Workspace")),
      *   Root("file:///home", Some("Home"))
      * )
      * }}}
      */
    def apply[F[_]: Applicative](roots: Root*): McpRoots[F] =
      McpRoots[F](roots*)

    /** Create a roots provider from URI and name.
      *
      * Example:
      * {{{
      * val workspace = Roots[IO]("file:///workspace", "Workspace")
      * }}}
      */
    def apply[F[_]: Applicative](uri: String, name: String): McpRoots[F] =
      McpRoots[F](Root(uri, Some(name)))

    /** Create an empty roots provider with no roots. */
    def empty[F[_]: Applicative]: McpRoots[F] =
      McpRoots.empty[F]

  // === Pure Extension ===

  /** Extension to lift pure values into any Applicative context.
    *
    * Example:
    * {{{
    * val result: IO[CreateMessageResult] = message("Hello", "model").pure[IO]
    * }}}
    */
  extension [A](a: A)
    def pure[F[_]: Applicative]: F[A] = Applicative[F].pure(a)
