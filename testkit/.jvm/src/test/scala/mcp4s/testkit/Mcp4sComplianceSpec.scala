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

import cats.effect.IO
import io.circe.Json
import io.circe.syntax.*
import mcp4s.protocol.TextContent
import mcp4s.server.Server

/** mcp4s's own compliance profile, exercised over HTTP and WebSocket against
  * [[TestServers.simple]]. Also serves as the worked example of the testkit harness.
  */
object Mcp4sComplianceSpec extends McpComplianceSuite:

  def serverUnderTest: Server[IO] = TestServers.withProgress[IO]

  def profile: ComplianceProfile = ComplianceProfile(
    sampleTool = Some(
      ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson), _.textContent == "5.0")
    ),
    progressTool = Some(ToolProbe("count")),
    cancellationTool = Some(ToolProbe("slow_add", Json.obj("a" -> 1.asJson, "b" -> 2.asJson))),
    sampleResource = Some(ResourceProbe("file:///test.txt", _.text.contains("Hello, World!"))),
    samplePrompt = Some(
      PromptProbe(
        "greeting",
        Map("name" -> "Ada"),
        _.messages.exists(_.content match
          case TextContent(t, _, _) => t.contains("Ada")
          case _                    => false)
      )
    )
  )
