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

package mcp4s.benchmarks

import scala.concurrent.duration.*

import cats.effect.{IO, IOApp}
import io.circe.Json
import mcp4s.testkit.{McpBenchmark, McpTransport, PerfProfile, TestServers, ToolProbe}

/** End-to-end throughput / latency driver: starts a real server, opens N concurrent client
  * connections over a chosen transport, and drives a fixed call budget — a thin CLI over
  * `mcp4s.testkit.McpBenchmark`, which does the actual load run.
  *
  * Unlike the JMH microbenchmarks, this exercises the full stack: codecs, fs2 streaming, ember,
  * session management, and concurrency. Use it to compare transports and to probe resilience: it
  * counts failed/stalled calls (each call is bounded by `bench.callTimeoutSeconds`), so a non-zero
  * failure count signals a correctness/resilience problem under load, not just a slow path.
  *
  * Run (defaults: http, 20000 calls, concurrency 8):
  * {{{
  * sbt "benchmarks/runMain mcp4s.benchmarks.ThroughputDriver"
  * sbt -Dbench.transport=ws -Dbench.calls=50000 -Dbench.concurrency=16 \
  *     "benchmarks/runMain mcp4s.benchmarks.ThroughputDriver"
  * }}}
  */
object ThroughputDriver extends IOApp.Simple:

  private val transport = sys.props.getOrElse("bench.transport", "http") match
    case "ws" => McpTransport.WebSocket
    case _    => McpTransport.Http

  private val profile = PerfProfile(
    toolProbe = ToolProbe("add", Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))),
    calls = sys.props.get("bench.calls").map(_.toInt).getOrElse(20000),
    concurrency = sys.props.get("bench.concurrency").map(_.toInt).getOrElse(8),
    callTimeout = sys.props.get("bench.callTimeoutSeconds").map(_.toInt).getOrElse(10).seconds,
    // The driver reports; it does not assert SLOs.
    maxFailureRate = None
  )

  def run: IO[Unit] =
    for
      _ <- IO.println(
        s"transport=$transport connections=${profile.concurrency} calls=${profile.calls}"
      )
      report <- McpBenchmark.run(TestServers.simple[IO], profile, transport)
      _      <- IO.println(report.render)
    yield ()
