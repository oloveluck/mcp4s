package mcp4s.examples.fixtures

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import io.circe.Json
import mcp4s.client.*
import mcp4s.protocol.*

/** Deterministic client builders for testing bidirectional communication.
  *
  * These clients provide fixed responses to server-initiated requests
  * (sampling, elicitation) without requiring an actual LLM.
  */
object DeterministicClients:

  /** Create a client with configurable sampling and elicitation responses.
    *
    * @param samplingResponse Fixed response for sampling/createMessage requests
    * @param elicitationResponse Fixed response for elicitation/create requests
    * @param trackCalls Whether to track calls for verification
    */
  def withResponses[F[_]: Async](
      samplingResponse: CreateMessageResult,
      elicitationResponse: ElicitResult,
      trackCalls: Boolean = false
  ): F[(McpClient[F], Option[F[ClientCallStats]])] =
    if trackCalls then
      for
        samplingCount <- Ref.of[F, Int](0)
        elicitationCount <- Ref.of[F, Int](0)
        elicitationCompleteCount <- Ref.of[F, Int](0)
      yield
        val client = McpClient
          .builder[F]
          .withInfo(ClientInfo("test-client", "1.0.0"))
          .withSamplingHandler: _ =>
            samplingCount.update(_ + 1).as(samplingResponse)
          .withElicitationHandler: _ =>
            elicitationCount.update(_ + 1).as(elicitationResponse)
          .withElicitationCompleteHandler: _ =>
            elicitationCompleteCount.update(_ + 1)
          .build

        val getStats = for
          s <- samplingCount.get
          e <- elicitationCount.get
          ec <- elicitationCompleteCount.get
        yield ClientCallStats(s, e, ec)

        (client, Some(getStats))
    else
      Async[F].pure {
        val client = McpClient
          .builder[F]
          .withInfo(ClientInfo("test-client", "1.0.0"))
          .withSamplingHandler(_ => Async[F].pure(samplingResponse))
          .withElicitationHandler(_ => Async[F].pure(elicitationResponse))
          .build
        (client, None)
      }

  /** Create a simple client with default sampling/elicitation responses. */
  def simple[F[_]: Async]: McpClient[F] =
    McpClient
      .builder[F]
      .withInfo(ClientInfo("test-client", "1.0.0"))
      .withSamplingHandler: params =>
        val prompt = params.messages.lastOption.map: msg =>
          msg.content match
            case SamplingTextContent(text) => text
            case _                         => ""
        .getOrElse("")
        Async[F].pure(CreateMessageResult(
          role = Role.Assistant,
          content = SamplingTextContent(s"Echo: $prompt"),
          model = "mock-model",
          stopReason = Some("endTurn")
        ))
      .withElicitationHandler: _ =>
        Async[F].pure(ElicitResult(ElicitAction.Accept, Some(Map("response" -> Json.fromString("test")))))
      .build

  /** Create a client without sampling support (no handler). */
  def withoutSampling[F[_]: Async]: McpClient[F] =
    McpClient
      .builder[F]
      .withInfo(ClientInfo("no-sampling-client", "1.0.0"))
      .build

  /** Create a client without elicitation support (no handler). */
  def withoutElicitation[F[_]: Async]: McpClient[F] =
    McpClient
      .builder[F]
      .withInfo(ClientInfo("no-elicitation-client", "1.0.0"))
      .withSamplingHandler: _ =>
        Async[F].pure(CreateMessageResult(
          role = Role.Assistant,
          content = SamplingTextContent("response"),
          model = "mock-model"
        ))
      .build

  /** Create a client that delays responses for timeout testing.
    *
    * @param delay Delay before responding
    */
  def delayed[F[_]: Async](delay: scala.concurrent.duration.FiniteDuration): McpClient[F] =
    import cats.effect.Temporal
    McpClient
      .builder[F]
      .withInfo(ClientInfo("slow-client", "1.0.0"))
      .withSamplingHandler: _ =>
        Temporal[F].sleep(delay) *>
          Async[F].pure(CreateMessageResult(
            role = Role.Assistant,
            content = SamplingTextContent("delayed response"),
            model = "mock-model"
          ))
      .withElicitationHandler: _ =>
        Temporal[F].sleep(delay) *>
          Async[F].pure(ElicitResult(ElicitAction.Accept, None))
      .build

  /** Create a client that tracks root requests. */
  def withRoots[F[_]: Async](roots: List[Root]): McpClient[F] =
    McpClient
      .builder[F]
      .withInfo(ClientInfo("roots-client", "1.0.0"))
      .withRoots(roots)
      .withSamplingHandler(_ => Async[F].pure(defaultSamplingResponse))
      .withElicitationHandler(_ => Async[F].pure(defaultElicitationResponse))
      .build

  /** Default sampling response for testing. */
  val defaultSamplingResponse: CreateMessageResult = CreateMessageResult(
    role = Role.Assistant,
    content = SamplingTextContent("Test response from mock LLM"),
    model = "mock-model-v1",
    stopReason = Some("endTurn")
  )

  /** Default elicitation response for testing. */
  val defaultElicitationResponse: ElicitResult = ElicitResult(
    action = ElicitAction.Accept,
    content = Some(Map(
      "field1" -> Json.fromString("value1"),
      "field2" -> Json.fromInt(42)
    ))
  )

  /** Elicitation decline response. */
  val declineElicitationResponse: ElicitResult = ElicitResult(
    action = ElicitAction.Decline,
    content = None
  )

/** Statistics for client calls (for verification in tests). */
final case class ClientCallStats(
    samplingCalls: Int,
    elicitationCalls: Int,
    elicitationCompleteCalls: Int
):
  def total: Int = samplingCalls + elicitationCalls + elicitationCompleteCalls
