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

package mcp4s.schema

import mcp4s.protocol.ServerInfo

/** A named group of endpoint definitions shared between server and client — the mcp4s analogue of a
  * smithy4s service.
  *
  * Declare each endpoint as a `val` and list them in [[endpoints]]. The same object then drives
  * both sides:
  *   - server: implement every endpoint with `MyService.routes(...)` (from
  *     `mcp4s.server.ServiceRoutes`), which fails fast if any endpoint in [[endpoints]] is missing
  *     a handler or a handler doesn't belong to the service;
  *   - client: call endpoints in a typed way with `connection.call(MyService.add)(AddArgs(1, 2))` —
  *     no stringly-typed tool names, no hand-rolled JSON.
  *
  * {{{
  * object Calculator extends McpService("calculator", "1.0.0"):
  *   val add   = Tool("add").withDescription("Add two numbers").input[AddArgs].output[AddResult]
  *   val greet = Tool.from[GreetArgs]        // name + description derived from the type
  *
  *   def endpoints = List(add, greet)
  * }}}
  */
abstract class McpService(val name: String, val version: String):

  /** Every tool endpoint this service exposes. `routes` verifies handlers cover exactly this list,
    * so an endpoint missing here is caught the moment the server is assembled.
    */
  def endpoints: List[ToolEndpoint[?, ?]]

  /** Server identity for this service. */
  def info: ServerInfo = ServerInfo(name, version)
