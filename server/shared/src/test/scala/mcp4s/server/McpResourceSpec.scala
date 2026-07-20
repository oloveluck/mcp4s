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

import cats.effect.IO
import cats.syntax.semigroup.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

class McpResourceSpec extends CatsEffectSuite:

  test("McpResource creates static text resource") {
    val readme = McpResource[IO]("file:///readme", "README")("Hello world")

    for
      resources <- readme.list
      _ = assertEquals(resources.size, 1)
      _ = assertEquals(resources.head.uri, "file:///readme")
      _ = assertEquals(resources.head.name, "README")
      content <- readme.read("file:///readme").value
      _ = assertEquals(content.map(_.text), Some(Some("Hello world")))
    yield ()
  }

  test("McpResource returns None for unknown URI") {
    val readme = McpResource[IO]("file:///readme", "README")("Hello")

    for
      content <- readme.read("file:///other").value
      _ = assertEquals(content, None)
    yield ()
  }

  test("McpResource.handler creates resource with custom handler") {
    val config = McpResource.handler[IO]("file:///config", "Config", "application/json") { _ =>
      IO.pure(ResourceContent.text("file:///config", """{"key": "value"}"""))
    }

    for
      resources <- config.list
      _ = assertEquals(resources.head.mimeType, Some("application/json"))
      content <- config.read("file:///config").value
      _ = assertEquals(content.flatMap(_.text), Some("""{"key": "value"}"""))
    yield ()
  }

  test("Resources compose with |+|") {
    val readme = McpResource[IO]("file:///readme", "README")("Hello")
    val config = McpResource[IO]("file:///config", "Config")("{}")

    val all = readme |+| config

    for
      resources <- all.list
      _ = assertEquals(resources.map(_.uri).toSet, Set("file:///readme", "file:///config"))
      r1 <- all.read("file:///readme").value
      _ = assertEquals(r1.flatMap(_.text), Some("Hello"))
      r2 <- all.read("file:///config").value
      _ = assertEquals(r2.flatMap(_.text), Some("{}"))
    yield ()
  }

  test("Resources left takes precedence on URI conflict") {
    val v1 = McpResource[IO]("file:///readme", "v1")("version 1")
    val v2 = McpResource[IO]("file:///readme", "v2")("version 2")

    val combined = v1 |+| v2

    for
      resources <- combined.list
      _ = assertEquals(resources.size, 1)
      _ = assertEquals(resources.head.name, "v1")
      content <- combined.read("file:///readme").value
      _ = assertEquals(content.flatMap(_.text), Some("version 1"))
    yield ()
  }

  test("static resources have empty changes stream") {
    val readme = McpResource[IO]("file:///readme", "README")("Hello")

    for
      changes <- readme.changes.compile.toList.timeout(100.millis).attempt
      _ = assertEquals(changes, Right(Nil))
    yield ()
  }

  test("template with '?' in the pattern matches it literally") {
    val tmpl = Resources.template[IO]("api://search?q={query}", "Search") { uri =>
      IO.pure(ResourceContent.text(uri, "ok"))
    }
    for
      hit <- tmpl.read("api://search?q=hello").value
      _ = assert(hit.isDefined, "literal '?' in the pattern should match a literal '?' in the URI")
      // With '?' treated as a regex quantifier, "api://searcq=..." would match ('h' optional).
      miss <- tmpl.read("api://searcq=hello").value
      _ = assertEquals(miss, None)
    yield ()
  }

  test("template with '+' and parentheses in the pattern matches them literally") {
    val tmpl = Resources.template[IO]("files://a+(b)/{id}", "Weird") { uri =>
      IO.pure(ResourceContent.text(uri, "ok"))
    }
    for
      hit <- tmpl.read("files://a+(b)/42").value
      _ = assert(hit.isDefined)
      // With '+' treated as a regex quantifier, "files://aab/42" would match.
      miss <- tmpl.read("files://aab/42").value
      _ = assertEquals(miss, None)
    yield ()
  }
