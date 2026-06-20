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

/** Cross-platform double formatting for test fixtures.
  *
  * Scala.js renders whole-valued doubles without a trailing ".0" (JS number semantics), e.g.
  * `5.0.toString == "5"`, whereas the JVM and Scala Native render "5.0". The MCP library passes
  * tool output through verbatim, so these test tools must format numbers identically on every
  * platform.
  */
object TestNum:
  def str(d: Double): String =
    if d.isInfinite || d.isNaN then d.toString
    else if d == d.toLong.toDouble then s"${d.toLong}.0"
    else d.toString
