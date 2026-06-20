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

import munit.FunSuite

class ToolInputMetaSpec extends FunSuite:

  // === deriveName Tests ===

  test("deriveName converts simple PascalCase to snake_case") {
    assertEquals(ToolInput.deriveName("Add"), "add")
  }

  test("deriveName strips Args suffix") {
    assertEquals(ToolInput.deriveName("AddArgs"), "add")
  }

  test("deriveName strips Input suffix") {
    assertEquals(ToolInput.deriveName("PromptWithArgsInput"), "prompt_with_args")
  }

  test("deriveName strips Params suffix") {
    assertEquals(ToolInput.deriveName("SearchParams"), "search")
  }

  test("deriveName strips Request suffix") {
    assertEquals(ToolInput.deriveName("QueryRequest"), "query")
  }

  test("deriveName handles multi-word PascalCase") {
    assertEquals(ToolInput.deriveName("SmartCalcArgs"), "smart_calc")
  }

  test("deriveName handles consecutive uppercase letters") {
    assertEquals(ToolInput.deriveName("HTTPClient"), "http_client")
  }

  test("deriveName does not strip suffix if it would empty the name") {
    assertEquals(ToolInput.deriveName("Args"), "args")
  }

  // === typeName Macro Tests ===

  case class TestTool(@description("x") x: Int) derives ToolInput

  test("typeName returns simple class name") {
    assertEquals(ToolInput.typeName[TestTool], "TestTool")
  }

  // === classDescription Macro Tests ===

  @description("A test tool")
  case class DescribedTool(@description("x") x: Int) derives ToolInput

  test("classDescription extracts class-level @description") {
    assertEquals(ToolInput.classDescription[DescribedTool], Some("A test tool"))
  }

  case class UndescribedTool(x: Int) derives ToolInput

  test("classDescription returns None when no annotation") {
    assertEquals(ToolInput.classDescription[UndescribedTool], None)
  }
