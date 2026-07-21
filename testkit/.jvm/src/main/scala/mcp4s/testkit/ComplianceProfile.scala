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

import io.circe.Json
import mcp4s.protocol.{GetPromptResult, ResourceContent, ToolResult}

/** A known-good `tools/call` the compliance suite can issue to exercise the tools capability. */
final case class ToolProbe(
    name: String,
    arguments: Json = Json.obj(),
    expect: ToolResult => Boolean = _ => true
)

/** A known-good `resources/read` the compliance suite can issue. */
final case class ResourceProbe(
    uri: String,
    expect: ResourceContent => Boolean = _ => true
)

/** A known-good `prompts/get` the compliance suite can issue. */
final case class PromptProbe(
    name: String,
    arguments: Map[String, String] = Map.empty,
    expect: GetPromptResult => Boolean = _ => true
)

/** Describes how to exercise a specific server-under-test. The compliance suite reads the server's
  * declared `ServerCapabilities` to decide *which* capability groups to check; this profile
  * supplies the concrete, valid inputs (the "toolkit") for those groups, plus the negative-case
  * names used to assert error handling.
  *
  * Any field left empty causes the corresponding checks to be skipped (reported as ignored), so the
  * same suite adapts to whatever a given server supports.
  */
final case class ComplianceProfile(
    sampleTool: Option[ToolProbe] = None,
    /** A tool that emits `notifications/progress`; enables the progress-callback check. */
    progressTool: Option[ToolProbe] = None,
    /** A slow tool used to start-then-cancel; enables the cancellation check. */
    cancellationTool: Option[ToolProbe] = None,
    sampleResource: Option[ResourceProbe] = None,
    samplePrompt: Option[PromptProbe] = None,
    unknownToolName: String = "mcp4s_testkit_no_such_tool",
    unknownResourceUri: String = "file:///mcp4s-testkit/no-such-resource",
    unknownPromptName: String = "mcp4s_testkit_no_such_prompt",
    checkPagination: Boolean = true
)
