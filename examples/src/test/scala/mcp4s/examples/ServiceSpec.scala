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

package mcp4s.examples

import cats.effect.IO
import com.comcast.ip4s.port
import mcp4s.client.{McpClientBuilder, TypedClient}
import mcp4s.client.syntax.*
import mcp4s.protocol.*
import mcp4s.schema.{McpService, Schema, Tool}
import mcp4s.server.{McpServer, ServiceRoutes}
import mcp4s.server.transport.HttpConfig
import munit.CatsEffectSuite
import org.typelevel.otel4s.trace.Tracer

// === The service, defined once and shared by server and client ===

case class AddArgs(a: Double, b: Double) derives Schema
case class AddResult(sum: Double) derives Schema

@description("Greet someone by name")
case class GreetArgs(name: String) derives Schema

object Calculator extends McpService("calculator", "1.0.0"):
  val add   = Tool("add").withDescription("Add two numbers").input[AddArgs].output[AddResult]
  val greet = Tool.from[GreetArgs]

  def endpoints = List(add, greet)

/** End-to-end test of the service-algebra layer: one McpService definition drives server routing
  * (with construction-time completeness checking) and a typed client (no stringly-typed names, no
  * hand-rolled JSON).
  */
class ServiceSpec extends CatsEffectSuite:

  import mcp4s.server.dsl.*
  import TypedClient.*

  given Tracer[IO] = Tracer.noop[IO]

  private val routes = ServiceRoutes(Calculator)(
    Calculator.add.handle[IO](args => IO.pure(AddResult(args.a + args.b))),
    Calculator.greet.handle[IO](args => IO.pure(ok(s"Hello, ${args.name}!")))
  )

  test("typed client round-trips a structured-output endpoint over HTTP") {
    McpServer[IO](Calculator.info)
      .withTools(routes)
      .http(HttpConfig(port = port"0"))
      .resource
      .use: http =>
        McpClientBuilder[IO](ClientInfo("svc-client", "1.0.0"))
          .http(s"http://localhost:${http.address.getPort}/mcp")
          .use: conn =>
            for
              sum   <- conn.call(Calculator.add)(AddArgs(19, 23))
              greet <- conn.call(Calculator.greet)(GreetArgs("Ada"))
            yield
              assertEquals(sum, AddResult(42.0))
              assertEquals(greet.textContent, "Hello, Ada!")
  }

  test("service advertises the endpoint definitions on the wire") {
    McpServer[IO](Calculator.info)
      .withTools(routes)
      .http(HttpConfig(port = port"0"))
      .resource
      .use: http =>
        McpClientBuilder[IO](ClientInfo("svc-client", "1.0.0"))
          .http(s"http://localhost:${http.address.getPort}/mcp")
          .use: conn =>
            for tools <- conn.listAllTools
            yield
              assertEquals(tools.map(_.name).toSet, Set("add", "greet"))
              val add = tools.find(_.name == "add").get
              assert(add.outputSchema.isDefined)
              assertEquals(add.description, Some("Add two numbers"))
              val greet = tools.find(_.name == "greet").get
              assertEquals(greet.description, Some("Greet someone by name"))
  }

  test("ServiceRoutes fails fast when an endpoint has no handler") {
    val err = intercept[IllegalArgumentException] {
      ServiceRoutes(Calculator)(
        Calculator.add.handle[IO](args => IO.pure(AddResult(args.a + args.b)))
      )
    }
    assert(err.getMessage.contains("greet"))
  }

  test("ServiceRoutes fails fast on a handler for an undeclared tool") {
    val err = intercept[IllegalArgumentException] {
      ServiceRoutes(Calculator)(
        Calculator.add.handle[IO](args => IO.pure(AddResult(args.a + args.b))),
        Calculator.greet.handle[IO](args => IO.pure(ok(s"Hello, ${args.name}!"))),
        Tool("rogue").withDescription("Not in the service").handle[IO](_ => IO.pure(ok("?")))
      )
    }
    assert(err.getMessage.contains("rogue"))
  }

  test("ServiceRoutes fails fast when an endpoint is bound twice") {
    val err = intercept[IllegalArgumentException] {
      ServiceRoutes(Calculator)(
        Calculator.add.handle[IO](args => IO.pure(AddResult(args.a + args.b))),
        Calculator.add.handle[IO](args => IO.pure(AddResult(args.a - args.b))),
        Calculator.greet.handle[IO](args => IO.pure(ok(s"Hello, ${args.name}!")))
      )
    }
    assert(err.getMessage.contains("add"))
  }
