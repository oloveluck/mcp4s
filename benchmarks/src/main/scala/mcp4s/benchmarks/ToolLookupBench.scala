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

import java.util.concurrent.TimeUnit

import scala.compiletime.uninitialized

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.openjdk.jmh.annotations.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.{Dispatcher, Server, Tools}

/** How tool dispatch scales with the number of registered tools.
  *
  * Statically-composed tools resolve through a name-keyed handler map built at composition time, so
  * dispatch should stay flat as N grows (only dynamic `Tools` implementations fall back to the
  * left-biased `OptionT.orElse` scan). This calls the *last* tool of `toolCount` — the scan's worst
  * case — to guard that property: allocations/op growing from N=1 to N=100 means the map path has
  * regressed to the scan.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput, Mode.SampleTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(value = 2)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
class ToolLookupBench:

  private given Tracer[IO] = Tracer.noop[IO]

  @Param(Array("1", "10", "100"))
  var toolCount: Int = uninitialized

  private var dispatcher: Dispatcher[IO] = uninitialized
  private var callMsg: JsonRpcMessage    = uninitialized

  @Setup
  def setup(): Unit =
    val tools = (0 until toolCount)
      .map { i =>
        Tools.single[IO](Tool(s"tool_$i", Some("bench tool"), JsonSchema.empty)) { _ =>
          IO.pure(ToolResult.text("ok"))
        }
      }
      .reduce(_ |+| _)

    val server = Server.fromTools[IO](ServerInfo("lookup-bench", "1.0.0"), tools)
    val d      = Dispatcher[IO](server).unsafeRunSync()
    d.dispatch(
      JsonRpcRequest(
        RequestId.NumberId(0L),
        McpMethod.Initialize,
        Some(
          InitializeParams(
            McpVersion.Current,
            ClientCapabilities(),
            ClientInfo("bench", "1.0.0")
          ).asJson
        )
      )
    ).unsafeRunSync()
    dispatcher = d

    // Call the last-registered tool: worst case for the left-biased lookup chain.
    val callParams = Json.obj(
      "name"      -> Json.fromString(s"tool_${toolCount - 1}"),
      "arguments" -> Json.obj()
    )
    callMsg = JsonRpcRequest(RequestId.NumberId(1L), McpMethod.ToolsCall, Some(callParams))

  @Benchmark
  def dispatchToolsCallWorstCase(): Option[JsonRpcMessage] =
    dispatcher.dispatch(callMsg).unsafeRunSync()
