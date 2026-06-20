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

package mcp4s.examples.fixtures

import cats.effect.{Async, Ref}
import cats.syntax.all.*
import io.circe.Json
import mcp4s.client.*
import mcp4s.protocol.*

/** Deterministic client builders for testing bidirectional communication.
  *
  * These clients provide fixed responses to server-initiated requests (sampling, elicitation)
  * without requiring an actual LLM.
  */
object DeterministicClients:

  /** Create a client with configurable sampling and elicitation responses.
    *
    * @param samplingResponse
    *   Fixed response for sampling/createMessage requests
    * @param elicitationResponse
    *   Fixed response for elicitation/create requests
    * @param trackCalls
    *   Whether to track calls for verification
    */
  def withResponses[F[_]: Async](
      samplingResponse: CreateMessageResult,
      elicitationResponse: ElicitResult,
      trackCalls: Boolean = false
  ): F[(McpClient[F], Option[F[ClientCallStats]])] =
    if trackCalls then
      for
        samplingCount            <- Ref.of[F, Int](0)
        elicitationCount         <- Ref.of[F, Int](0)
        elicitationCompleteCount <- Ref.of[F, Int](0)
      yield
        val client = McpClient.from[F](
          ClientInfo("test-client", "1.0.0"),
          sampling = Some(Samplings[F](_ => samplingCount.update(_ + 1).as(samplingResponse))),
          elicitation = Some(
            Elicitations.withComplete[F](
              _ => elicitationCount.update(_ + 1).as(elicitationResponse),
              _ => elicitationCompleteCount.update(_ + 1)
            )
          )
        )

        val getStats = for
          s  <- samplingCount.get
          e  <- elicitationCount.get
          ec <- elicitationCompleteCount.get
        yield ClientCallStats(s, e, ec)

        (client, Some(getStats))
    else
      Async[F].pure {
        val client = McpClient.from[F](
          ClientInfo("test-client", "1.0.0"),
          sampling = Some(Samplings[F](_ => Async[F].pure(samplingResponse))),
          elicitation = Some(Elicitations[F](_ => Async[F].pure(elicitationResponse)))
        )
        (client, None)
      }

  /** Create a simple client with default sampling/elicitation responses. */
  def simple[F[_]: Async]: McpClient[F] =
    McpClient.from[F](
      ClientInfo("test-client", "1.0.0"),
      sampling = Some(Samplings[F] { params =>
        val prompt = params.messages.lastOption
          .map: msg =>
            msg.content match
              case SamplingTextContent(text) => text
              case _                         => ""
          .getOrElse("")
        Async[F].pure(
          CreateMessageResult(
            role = Role.Assistant,
            content = SamplingTextContent(s"Echo: $prompt"),
            model = "mock-model",
            stopReason = Some("endTurn")
          )
        )
      }),
      elicitation = Some(Elicitations[F] { _ =>
        Async[F].pure(
          ElicitResult(ElicitAction.Accept, Some(Map("response" -> Json.fromString("test"))))
        )
      })
    )

  /** Create a client without sampling support (no handler). */
  def withoutSampling[F[_]: Async]: McpClient[F] =
    McpClient.from[F](ClientInfo("no-sampling-client", "1.0.0"))

  /** Create a client without elicitation support (no handler). */
  def withoutElicitation[F[_]: Async]: McpClient[F] =
    McpClient.from[F](
      ClientInfo("no-elicitation-client", "1.0.0"),
      sampling = Some(Samplings[F] { _ =>
        Async[F].pure(
          CreateMessageResult(
            role = Role.Assistant,
            content = SamplingTextContent("response"),
            model = "mock-model"
          )
        )
      })
    )

  /** Create a client that delays responses for timeout testing.
    *
    * @param delay
    *   Delay before responding
    */
  def delayed[F[_]: Async](delay: scala.concurrent.duration.FiniteDuration): McpClient[F] =
    import cats.effect.Temporal
    McpClient.from[F](
      ClientInfo("slow-client", "1.0.0"),
      sampling = Some(Samplings[F] { _ =>
        Temporal[F].sleep(delay) *>
          Async[F].pure(
            CreateMessageResult(
              role = Role.Assistant,
              content = SamplingTextContent("delayed response"),
              model = "mock-model"
            )
          )
      }),
      elicitation = Some(Elicitations[F] { _ =>
        Temporal[F].sleep(delay) *>
          Async[F].pure(ElicitResult(ElicitAction.Accept, None))
      })
    )

  /** Create a client that tracks root requests. */
  def withRoots[F[_]: Async](roots: List[Root]): McpClient[F] =
    McpClient.from[F](
      ClientInfo("roots-client", "1.0.0"),
      roots = Some(Roots[F](roots*)),
      sampling = Some(Samplings[F](_ => Async[F].pure(defaultSamplingResponse))),
      elicitation = Some(Elicitations[F](_ => Async[F].pure(defaultElicitationResponse)))
    )

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
    content = Some(
      Map(
        "field1" -> Json.fromString("value1"),
        "field2" -> Json.fromInt(42)
      )
    )
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
