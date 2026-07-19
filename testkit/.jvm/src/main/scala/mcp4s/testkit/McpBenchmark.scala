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

import scala.concurrent.duration.*

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import org.HdrHistogram.Histogram
import mcp4s.client.McpConnection
import mcp4s.server.Server

/** A load run plus optional SLOs.
  *
  * @param toolProbe
  *   the tool call issued under load
  * @param calls
  *   total calls (split across `concurrency` connections)
  * @param concurrency
  *   number of concurrent client connections
  * @param callTimeout
  *   per-call timeout; a call that exceeds it is counted as a failure rather than hanging the run
  * @param minThroughput
  *   optional SLO: required calls/sec
  * @param maxP99
  *   optional SLO: maximum p99 latency
  * @param maxFailureRate
  *   optional SLO: maximum fraction of failed calls (defaults to 0.0)
  */
final case class PerfProfile(
    toolProbe: ToolProbe,
    calls: Int = 5000,
    concurrency: Int = 8,
    callTimeout: FiniteDuration = 10.seconds,
    minThroughput: Option[Double] = None,
    maxP99: Option[FiniteDuration] = None,
    maxFailureRate: Option[Double] = Some(0.0)
)

/** Result of a load run: counts, wall time, throughput, and latency percentiles (successes). */
final case class PerfReport(
    successful: Long,
    failed: Long,
    elapsed: FiniteDuration,
    p50: FiniteDuration,
    p95: FiniteDuration,
    p99: FiniteDuration,
    p999: FiniteDuration,
    max: FiniteDuration,
    throughput: Double
):
  def total: Long         = successful + failed
  def failureRate: Double = if total == 0 then 0.0 else failed.toDouble / total.toDouble

  def render: String =
    def ms(d: FiniteDuration): Double = d.toNanos.toDouble / 1e6
    f"""=== mcp4s PerfReport ===
       |successful calls: $successful%d
       |failed calls    : $failed%d  (${failureRate * 100}%.2f%%)
       |wall time       : ${elapsed.toNanos.toDouble / 1e9}%.2f s
       |throughput      : $throughput%,.0f calls/sec
       |latency p50     : ${ms(p50)}%.3f ms
       |latency p95     : ${ms(p95)}%.3f ms
       |latency p99     : ${ms(p99)}%.3f ms
       |latency p99.9   : ${ms(p999)}%.3f ms
       |latency max     : ${ms(max)}%.3f ms""".stripMargin

/** Runs a load benchmark against an MCP server and produces a [[PerfReport]].
  *
  * Easiest entry point — point it at your server and a probe:
  * {{{
  * import cats.effect.{IO, IOApp}
  * import io.circe.Json, io.circe.syntax.*
  * import mcp4s.testkit.*
  *
  * object Bench extends IOApp.Simple:
  *   def run = McpBenchmark
  *     .run(MyServer.build[IO], PerfProfile(ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson))))
  *     .flatMap(r => IO.println(r.render))
  * }}}
  *
  * Each call is bounded by `profile.callTimeout`, so a stalled connection is counted as a failure
  * instead of hanging the run — making this a resilience probe as well as a throughput meter.
  */
object McpBenchmark:

  private val maxTrackedMicros = 3600000000L // 1 hour, in microseconds

  /** Start `server` over `transport` on an ephemeral port, run the load, and return the report. */
  def run(
      server: Server[IO],
      profile: PerfProfile,
      transport: McpTransport = McpTransport.Http
  ): IO[PerfReport] =
    McpHarness.serve(server, transport).use(endpoint => run(endpoint.connect, profile))

  /** Run the load against connections obtained from `connect` (one per `profile.concurrency`). */
  def run(connect: Resource[IO, McpConnection[IO]], profile: PerfProfile): IO[PerfReport] =
    val callsPerConn = (profile.calls / profile.concurrency).max(1)
    val warmupCalls  = (callsPerConn / 10).max(20)
    val probe        = profile.toolProbe

    def driveOne(conn: McpConnection[IO], n: Int): IO[(Histogram, Long)] =
      IO.ref(0L)
        .flatMap: failures =>
          val hist = new Histogram(maxTrackedMicros, 3)
          (1 to n).toList
            .traverse_ { _ =>
              for
                t0 <- IO.monotonic
                res <- conn
                  .callTool(probe.name, probe.arguments)
                  .timeout(profile.callTimeout)
                  .attempt
                t1 <- IO.monotonic
                _ <- res match
                  case Right(r) if !r.isError.getOrElse(false) =>
                    IO.delay(hist.recordValue((t1 - t0).toMicros.max(1L)))
                  case _ => failures.update(_ + 1)
              yield ()
            }
            .flatMap(_ => failures.get.map(hist -> _))

    List
      .fill(profile.concurrency)(connect)
      .sequence
      .use: conns =>
        for
          _       <- conns.parTraverse_(driveOne(_, warmupCalls))
          start   <- IO.monotonic
          results <- conns.parTraverse(driveOne(_, callsPerConn))
          end     <- IO.monotonic
        yield
          val merged = new Histogram(maxTrackedMicros, 3)
          results.foreach((h, _) => merged.add(h))
          val elapsed                        = end - start
          val seconds                        = elapsed.toNanos.toDouble / 1e9
          def pct(p: Double): FiniteDuration = merged.getValueAtPercentile(p).micros
          PerfReport(
            successful = merged.getTotalCount,
            failed = results.map(_._2).sum,
            elapsed = elapsed,
            p50 = pct(50.0),
            p95 = pct(95.0),
            p99 = pct(99.0),
            p999 = pct(99.9),
            max = pct(100.0),
            throughput = if seconds > 0 then merged.getTotalCount.toDouble / seconds else 0.0
          )
