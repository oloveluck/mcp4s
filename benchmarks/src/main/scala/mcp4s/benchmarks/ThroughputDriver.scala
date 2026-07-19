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

import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.port
import io.circe.Json
import org.HdrHistogram.Histogram
import org.http4s.server.Server as Http4sServer
import mcp4s.client.{McpClient, McpConnection}
import mcp4s.client.syntax.*
import mcp4s.server.Server
import mcp4s.server.syntax.*
import mcp4s.testkit.{DeterministicClients, TestServers}

/** End-to-end throughput / latency driver: starts a real server, opens N concurrent client
  * connections over a chosen transport, and drives a fixed call budget while recording per-call
  * latency into an HdrHistogram.
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

  private val transport   = sys.props.getOrElse("bench.transport", "http")
  private val totalCalls  = sys.props.get("bench.calls").map(_.toInt).getOrElse(20000)
  private val concurrency = sys.props.get("bench.concurrency").map(_.toInt).getOrElse(8)

  // Per-call timeout so a hung connection is counted as a failure rather than blocking the
  // whole run forever (sustained-load WebSocket runs have been observed to stall mid-stream).
  private val callTimeout =
    sys.props.get("bench.callTimeoutSeconds").map(_.toInt).getOrElse(10).seconds

  private val addArgs = Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))

  private def serverResource: Resource[IO, Http4sServer] =
    val server: Server[IO] = TestServers.simple[IO]
    transport match
      case "ws" => server.serveWebSocket(port"0")
      case _    => server.serveHttp(port"0")

  private def connect(port: Int): Resource[IO, McpConnection[IO]] =
    val client: McpClient[IO] = DeterministicClients.simple[IO]
    transport match
      case "ws" => client.connectWebSocket(s"ws://localhost:$port")
      case _    => client.connectHttp(s"http://localhost:$port")

  /** Outcome of driving one connection: latency histogram (µs, successes only) + failure count. */
  final private case class DriveResult(hist: Histogram, failures: Long)

  /** Drive `n` sequential calls on one connection. Records successful-call latency and counts
    * failures (errored results or dropped connections) rather than aborting — so the driver doubles
    * as a resilience probe instead of crashing on the first error.
    */
  private def drive(conn: McpConnection[IO], n: Int): IO[DriveResult] =
    IO.ref(0L)
      .flatMap: failures =>
        val hist = new Histogram(3600000000L, 3) // up to 1h in µs, 3 significant digits
        (1 to n).toList
          .traverse_ { _ =>
            for
              t0  <- IO.monotonic
              res <- conn.callTool("add", addArgs).timeout(callTimeout).attempt
              t1  <- IO.monotonic
              _ <- res match
                case Right(r) if !r.isError.getOrElse(false) =>
                  IO.delay(hist.recordValue((t1 - t0).toMicros.max(1L)))
                case _ => failures.update(_ + 1)
            yield ()
          } *> failures.get.map(DriveResult(hist, _))

  def run: IO[Unit] =
    val callsPerConn = (totalCalls / concurrency).max(1)
    val warmupCalls  = (callsPerConn / 10).max(50)

    serverResource.use: srv =>
      val serverPort = srv.address.getPort
      List
        .fill(concurrency)(connect(serverPort))
        .sequence
        .use: conns =>
          for
            _     <- IO.println(s"transport=$transport connections=$concurrency calls=$totalCalls")
            _     <- IO.println(s"warming up (${warmupCalls * concurrency} calls)...")
            _     <- conns.parTraverse_(drive(_, warmupCalls))
            _     <- IO.println("measuring...")
            start <- IO.monotonic
            results <- conns.parTraverse(drive(_, callsPerConn))
            end     <- IO.monotonic
            merged <- IO.delay:
              val h = new Histogram(3600000000L, 3)
              results.foreach(r => h.add(r.hist))
              h
            failures = results.map(_.failures).sum
            _ <- report(merged, failures, end - start)
          yield ()

  private def report(
      h: Histogram,
      failures: Long,
      elapsed: scala.concurrent.duration.FiniteDuration
  ): IO[Unit] =
    val seconds               = elapsed.toNanos.toDouble / 1e9
    val throughput            = h.getTotalCount.toDouble / seconds
    def ms(p: Double): Double = h.getValueAtPercentile(p) / 1000.0
    IO.println(
      f"""
         |=== mcp4s ThroughputDriver ($transport) ===
         |successful calls: ${h.getTotalCount}%d
         |failed calls    : $failures%d
         |wall time       : $seconds%.2f s
         |throughput      : $throughput%,.0f calls/sec
         |latency p50     : ${ms(50.0)}%.3f ms
         |latency p95     : ${ms(95.0)}%.3f ms
         |latency p99     : ${ms(99.0)}%.3f ms
         |latency p99.9   : ${ms(99.9)}%.3f ms
         |latency max     : ${ms(100.0)}%.3f ms
         |""".stripMargin
    )
