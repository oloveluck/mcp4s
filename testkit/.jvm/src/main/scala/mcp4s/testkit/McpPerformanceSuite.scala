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
import mcp4s.server.Server
import weaver.SimpleIOSuite

/** A reusable performance gate: runs a load benchmark against a server-under-test and asserts the
  * [[PerfProfile]]'s SLOs (failure rate / throughput / p99). The full report is always printed, so
  * it's useful even with no SLOs set.
  *
  * Usage:
  * {{{
  * object MyServerPerfSpec extends McpPerformanceSuite:
  *   def serverUnderTest = MyServer.build[IO]
  *   def perfProfile = PerfProfile(
  *     toolProbe = ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson)),
  *     calls = 2000, concurrency = 8, maxFailureRate = Some(0.0)
  *   )
  * }}}
  *
  * To get numbers ad-hoc (outside a test), call [[McpBenchmark.run]] directly from an `IOApp`.
  */
abstract class McpPerformanceSuite extends SimpleIOSuite:

  def serverUnderTest: Server[IO]
  def perfProfile: PerfProfile
  def transport: McpTransport = McpTransport.Http

  test("performance profile meets SLOs") {
    McpBenchmark.run(serverUnderTest, perfProfile, transport).flatMap { report =>
      IO.println(report.render).as {
        List(
          perfProfile.maxFailureRate.map(m => expect(report.failureRate <= m)),
          perfProfile.minThroughput.map(m => expect(report.throughput >= m)),
          perfProfile.maxP99.map(m => expect(report.p99 <= m))
        ).flatten.foldLeft(success)(_ and _)
      }
    }
  }
