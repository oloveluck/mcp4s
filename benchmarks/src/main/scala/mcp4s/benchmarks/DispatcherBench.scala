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
import io.circe.Json
import io.circe.syntax.*
import org.openjdk.jmh.annotations.*
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.protocol.Codecs.given
import mcp4s.server.{Dispatcher, Server}
import mcp4s.testkit.TestServers

/** Microbenchmarks for the in-memory server request hot path (no network/transport).
  *
  * Measures the cost of routing a JSON-RPC message through the [[Dispatcher]] to a tool/resource
  * handler and encoding the response, plus the raw codec decode. This is the truest "server core"
  * latency — transport overhead is benchmarked separately by `ThroughputDriver`.
  *
  * Run:
  * {{{
  * sbt "benchmarks/Jmh/run mcp4s.benchmarks.DispatcherBench"
  * // allocation profile (bytes/op):
  * sbt "benchmarks/Jmh/run -prof gc mcp4s.benchmarks.DispatcherBench"
  * }}}
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput, Mode.SampleTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(value = 2)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
class DispatcherBench:

  private given Tracer[IO] = Tracer.noop[IO]

  private var dispatcher: Dispatcher[IO]   = uninitialized
  private var toolsCallMsg: JsonRpcMessage = uninitialized
  private var toolsListMsg: JsonRpcMessage = uninitialized
  private var toolsCallJson: String        = uninitialized
  private var toolResultValue: ToolResult  = uninitialized
  private var toolList: List[Tool]         = uninitialized

  @Setup
  def setup(): Unit =
    val server: Server[IO] = TestServers.simple[IO]
    val d                  = Dispatcher[IO](server).unsafeRunSync()

    // Drive the initialize handshake so tools/call is accepted.
    val initParams = InitializeParams(
      McpVersion.Current,
      ClientCapabilities(),
      ClientInfo("bench-client", "1.0.0")
    )
    d.dispatch(
      JsonRpcRequest(RequestId.NumberId(0L), McpMethod.Initialize, Some(initParams.asJson))
    ).unsafeRunSync()
    dispatcher = d

    val callParams = Json.obj(
      "name"      -> Json.fromString("add"),
      "arguments" -> Json.obj("a" -> Json.fromInt(2), "b" -> Json.fromInt(3))
    )
    toolsCallMsg = JsonRpcRequest(RequestId.NumberId(1L), McpMethod.ToolsCall, Some(callParams))
    toolsListMsg = JsonRpcRequest(RequestId.NumberId(2L), McpMethod.ToolsList, None)
    toolsCallJson = (toolsCallMsg: JsonRpcMessage).asJson.noSpaces
    toolResultValue = ToolResult.text("the quick brown fox jumped over the lazy dog")
    toolList = server.listTools.unsafeRunSync()

  /** Full in-memory round-trip for tools/call: param extraction + tool lookup + execute + encode.
    */
  @Benchmark
  def dispatchToolsCall(): Option[JsonRpcMessage] =
    dispatcher.dispatch(toolsCallMsg).unsafeRunSync()

  /** tools/list dispatch: capability check + list + encode. */
  @Benchmark
  def dispatchToolsList(): Option[JsonRpcMessage] =
    dispatcher.dispatch(toolsListMsg).unsafeRunSync()

  /** Raw codec cost: decode a tools/call request from its wire string. */
  @Benchmark
  def decodeToolsCallRequest(): Either[io.circe.Error, JsonRpcMessage] =
    io.circe.parser.decode[JsonRpcMessage](toolsCallJson)

  /** Encode cost for a single ToolResult (runs the codec's per-message `dropNullValues`). */
  @Benchmark
  def encodeToolResult(): Json = toolResultValue.asJson

  /** Encode cost for a tools/list result — exercises optional-field dropping across many objects.
    */
  @Benchmark
  def encodeToolListResponse(): Json = Json.obj("tools" -> toolList.asJson)
