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
import mcp4s.server.Server

/** mcp4s's own performance profile over HTTP against [[TestServers.simple]].
  *
  * Asserts only a zero-failure SLO (the report always prints throughput/latency); throughput and
  * p99 SLOs are intentionally left unset so the check is not flaky on shared CI hardware.
  */
object Mcp4sPerformanceSpec extends McpPerformanceSuite:

  def serverUnderTest: Server[IO] = TestServers.simple[IO]

  def perfProfile: PerfProfile = PerfProfile(
    toolProbe = ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson)),
    calls = 2000,
    concurrency = 8,
    maxFailureRate = Some(0.0)
  )

  // HTTP for a deterministic default; the WebSocket client (http4s JdkWSClient) is actually the
  // faster transport under load (see benchmarks/README.md) — set `transport = McpTransport.WebSocket`
  // to profile it.
  override def transport: McpTransport = McpTransport.Http
