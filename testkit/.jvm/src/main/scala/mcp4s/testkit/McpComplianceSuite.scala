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

package mcp4s.testkit

import scala.concurrent.duration.*

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import io.circe.Json
import mcp4s.client.McpConnection
import mcp4s.server.Server
import weaver.IOSuite

/** A reusable, capability-parameterized MCP compliance suite. Point it at any `Server[IO]` and a
  * [[ComplianceProfile]]; it runs the applicable protocol checks over live HTTP and WebSocket
  * connections, skipping (ignoring) checks for capabilities the server doesn't declare or inputs
  * the profile doesn't supply.
  *
  * Usage:
  * {{{
  * object MyServerComplianceSpec extends McpComplianceSuite:
  *   def serverUnderTest = MyServer.build[IO]
  *   def profile = ComplianceProfile(
  *     sampleTool = Some(ToolProbe("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson))),
  *     sampleResource = Some(ResourceProbe("file:///readme")),
  *     samplePrompt = Some(PromptProbe("greet", Map("name" -> "Ada")))
  *   )
  * }}}
  *
  * Override members with `def` (not `val`) — `transports` is read during suite construction.
  *
  * Scope: this exercises post-initialize protocol behavior over real transports. Pre-initialize
  * state-gating (`NotInitialized`/`AlreadyInitialized`) is a dispatcher-level concern covered by
  * mcp4s's own unit tests, and the official `conformance` submodule remains the authority for
  * wire-level SSE/DNS-rebinding scenarios.
  */
abstract class McpComplianceSuite extends IOSuite:

  /** The server implementation to check for MCP compliance. */
  def serverUnderTest: Server[IO]

  /** Sample inputs + toggles describing how to exercise the server. */
  def profile: ComplianceProfile

  /** Transports to run every applicable check over. Override as a `def`. */
  def transports: List[McpTransport] = List(McpTransport.Http, McpTransport.WebSocket)

  type Res = Map[McpTransport, McpConnection[IO]]

  def sharedResource: Resource[IO, Res] =
    transports
      .traverse(t => McpHarness.serve(serverUnderTest, t).flatMap(_.connect).map(c => t -> c))
      .map(_.toMap)

  transports.foreach { t =>
    test(s"[$t] server advertises identity and capabilities") { res =>
      val conn = res(t)
      IO(expect(conn.serverInfo.name.nonEmpty))
    }

    test(s"[$t] ping succeeds (repeatedly)") { res =>
      val conn = res(t)
      (1 to 3).toList.traverse_(_ => conn.ping).as(success)
    }

    test(s"[$t] capability flags are consistent with declared capabilities") { res =>
      val conn = res(t)
      val caps = conn.serverCapabilities
      IO(
        expect.all(
          conn.supportsTools == caps.tools.isDefined,
          conn.supportsResources == caps.resources.isDefined,
          conn.supportsPrompts == caps.prompts.isDefined
        )
      )
    }

    // === Tools ===

    test(s"[$t] tools/list returns tools when the tools capability is declared") { res =>
      val conn = res(t)
      if !conn.supportsTools then ignore("server does not declare the tools capability")
      else conn.listAllTools.map(ts => expect(ts.nonEmpty))
    }

    test(s"[$t] tools/call with valid arguments succeeds") { res =>
      val conn = res(t)
      profile.sampleTool match
        case None    => ignore("profile.sampleTool not provided")
        case Some(p) =>
          conn
            .callTool(p.name, p.arguments)
            .map(r => expect(!r.isError.getOrElse(false)) and expect(p.expect(r)))
    }

    test(s"[$t] tools/call for an unknown tool returns an error") { res =>
      val conn = res(t)
      if !conn.supportsTools then ignore("server does not declare the tools capability")
      else conn.callTool(profile.unknownToolName, Json.obj()).attempt.map(e => expect(e.isLeft))
    }

    test(s"[$t] concurrent tools/call all succeed") { res =>
      val conn = res(t)
      profile.sampleTool match
        case None    => ignore("profile.sampleTool not provided")
        case Some(p) =>
          (1 to 10).toList
            .parTraverse(_ => conn.callTool(p.name, p.arguments))
            .map(rs => expect(rs.forall(!_.isError.getOrElse(false))))
    }

    test(s"[$t] progress notifications are delivered to the callback") { res =>
      val conn = res(t)
      profile.progressTool match
        case None    => ignore("profile.progressTool not provided")
        case Some(p) =>
          for
            seen <- IO.ref(0)
            _    <- conn.callTool(p.name, p.arguments, _ => seen.update(_ + 1))
            n    <- seen.get
          yield expect(n > 0)
    }

    // === Resources ===

    test(s"[$t] resources/list returns resources when declared") { res =>
      val conn = res(t)
      if !conn.supportsResources then ignore("server does not declare the resources capability")
      else conn.listAllResources.map(rs => expect(rs.nonEmpty))
    }

    test(s"[$t] resources/read returns content for a known resource") { res =>
      val conn = res(t)
      profile.sampleResource match
        case None    => ignore("profile.sampleResource not provided")
        case Some(p) =>
          conn.readResource(p.uri).map(c => expect(c.uri == p.uri) and expect(p.expect(c)))
    }

    test(s"[$t] resources/read for an unknown uri returns an error") { res =>
      val conn = res(t)
      if !conn.supportsResources then ignore("server does not declare the resources capability")
      else conn.readResource(profile.unknownResourceUri).attempt.map(e => expect(e.isLeft))
    }

    test(s"[$t] resources/templates/list is callable when resources are declared") { res =>
      val conn = res(t)
      if !conn.supportsResources then ignore("server does not declare the resources capability")
      else conn.listAllResourceTemplates.attempt.map(e => expect(e.isRight))
    }

    // === Prompts ===

    test(s"[$t] prompts/list returns prompts when declared") { res =>
      val conn = res(t)
      if !conn.supportsPrompts then ignore("server does not declare the prompts capability")
      else conn.listAllPrompts.map(ps => expect(ps.nonEmpty))
    }

    test(s"[$t] prompts/get returns a result for a known prompt") { res =>
      val conn = res(t)
      profile.samplePrompt match
        case None    => ignore("profile.samplePrompt not provided")
        case Some(p) =>
          conn
            .getPrompt(p.name, p.arguments)
            .map(r => expect(r.messages.nonEmpty) and expect(p.expect(r)))
    }

    test(s"[$t] prompts/get for an unknown prompt returns an error") { res =>
      val conn = res(t)
      if !conn.supportsPrompts then ignore("server does not declare the prompts capability")
      else
        conn
          .getPrompt(profile.unknownPromptName, Map.empty[String, String])
          .attempt
          .map(e => expect(e.isLeft))
    }

    // === Pagination ===

    test(s"[$t] auto-pagination drains list endpoints") { res =>
      val conn = res(t)
      if !profile.checkPagination then ignore("pagination checks disabled in profile")
      else
        for
          tools <- conn.listAllTools.attempt
          rs    <- conn.listAllResources.attempt
          ps    <- conn.listAllPrompts.attempt
        yield expect.all(tools.isRight, rs.isRight, ps.isRight)
    }

    // === Cancellation ===

    test(s"[$t] connection remains usable after a cancelled call") { res =>
      val conn = res(t)
      profile.cancellationTool match
        case None    => ignore("profile.cancellationTool not provided")
        case Some(p) =>
          for
            fiber <- conn.callTool(p.name, p.arguments).start
            _     <- IO.sleep(50.millis)
            _     <- fiber.cancel
            still <- profile.sampleTool match
              case Some(s) => conn.callTool(s.name, s.arguments).attempt.map(_.isRight)
              case None    => conn.ping.attempt.map(_.isRight)
          yield expect(still)
    }
  }
