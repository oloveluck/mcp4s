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
import mcp4s.server.{mcp, Dispatcher, Prompts, Server, Tools}

/** A `resources/read` against a URI-template resource.
  *
  * `McpResource.matchesTemplate` rebuilds and compiles the match regex on every read, so this
  * benchmark quantifies that allocation hotspot — and would show the win of precompiling the
  * pattern once per template.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput, Mode.SampleTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(value = 2)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
class ResourceTemplateBench:

  private given Tracer[IO] = Tracer.noop[IO]

  private var dispatcher: Dispatcher[IO] = uninitialized
  private var readMsg: JsonRpcMessage    = uninitialized

  @Setup
  def setup(): Unit =
    val resources = mcp.Resource.template[IO]("bench://item/{id}", "Item", "Benchmark item") {
      uri =>
        IO.pure(ResourceContent.text(uri, "ok"))
    }
    val server =
      Server.from[IO](
        ServerInfo("resource-bench", "1.0.0"),
        Tools.empty[IO],
        resources,
        Prompts.empty[IO]
      )
    val d = Dispatcher[IO](server).unsafeRunSync()
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

    val readParams = Json.obj("uri" -> Json.fromString("bench://item/42"))
    readMsg = JsonRpcRequest(RequestId.NumberId(1L), McpMethod.ResourcesRead, Some(readParams))

  @Benchmark
  def dispatchResourcesRead(): Option[JsonRpcMessage] =
    dispatcher.dispatch(readMsg).unsafeRunSync()
