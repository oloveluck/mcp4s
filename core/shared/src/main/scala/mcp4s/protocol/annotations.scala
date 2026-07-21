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

package mcp4s.protocol

import scala.annotation.StaticAnnotation

/** Annotation to describe a field for JSON schema generation.
  *
  * Example:
  * {{{
  * case class AddArgs(
  *   @description("First number to add") a: Double,
  *   @description("Second number to add") b: Double
  * ) derives Schema
  * }}}
  */
final class description(val value: String) extends StaticAnnotation
