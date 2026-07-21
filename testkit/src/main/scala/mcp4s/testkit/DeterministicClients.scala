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

import cats.effect.Async
import io.circe.Json
import mcp4s.client.*
import mcp4s.protocol.*

/** Deterministic client builders for testing bidirectional communication — they answer
  * server-initiated requests (sampling, elicitation) with fixed responses, no LLM required.
  */
object DeterministicClients:

  /** A client that echoes sampling prompts and accepts elicitations. Suitable for connecting to a
    * server-under-test in compliance and performance runs.
    */
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
