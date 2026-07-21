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

package mcp4s.server

import munit.{Assertions, Location}
import mcp4s.protocol.{Content, TextContent}

/** Shared assertion helpers for server specs. */
object TestSyntax:

  /** Text of a [[Content]] expected to be [[TextContent]]; fails the test (at the caller's
    * location) otherwise.
    */
  def textOf(c: Content)(using Location): String = c match
    case t: TextContent => t.text
    case other          => Assertions.fail(s"Expected TextContent, got: $other")
