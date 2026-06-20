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

import scala.util.matching.Regex

/** Shared utility for matching URI template patterns against concrete URIs.
  *
  * Converts patterns like "api://users/{id}/data" to a regex that matches concrete URIs. Used by
  * both `Resources.template` and `BuiltServer` template handlers.
  */
private[server] object UriTemplate:

  /** Compile a URI template pattern to a Regex.
    *
    * Converts `{placeholder}` segments to `[^/]+` matchers and escapes literal dots and slashes.
    */
  def compile(pattern: String): Regex =
    pattern
      .replace(".", "\\.")
      .replace("/", "\\/")
      .replaceAll("\\{[^}]+\\}", "[^/]+")
      .r

  /** Check if a concrete URI matches a URI template pattern. */
  def matches(pattern: String, uri: String): Boolean =
    compile(pattern).matches(uri)
