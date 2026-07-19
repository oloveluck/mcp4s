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

package mcp4s.client.transport

import cats.Applicative
import cats.syntax.all.*
import org.http4s.{Header, Headers}
import org.typelevel.ci.CIString

/** Authentication for network transports (HTTP and WebSocket).
  *
  * The resolved token is sent as an `Authorization: Bearer <token>` header — on every request for
  * HTTP, and on the upgrade request for WebSocket.
  */
enum McpAuth[F[_]]:
  /** Static bearer token. */
  case Bearer(token: String) extends McpAuth[F]

  /** Dynamic token provider called before each request. Use for token refresh, Ref-based tokens,
    * etc.
    */
  case TokenProvider(provide: F[String]) extends McpAuth[F]

object McpAuth:
  /** Add the Authorization header for `auth` (if any) to `headers`. */
  private[client] def applyTo[F[_]: Applicative](
      auth: Option[McpAuth[F]],
      headers: Headers
  ): F[Headers] =
    auth match
      case Some(McpAuth.Bearer(token)) =>
        headers.put(Header.Raw(CIString("Authorization"), s"Bearer $token")).pure[F]
      case Some(McpAuth.TokenProvider(provide)) =>
        provide.map(token => headers.put(Header.Raw(CIString("Authorization"), s"Bearer $token")))
      case None =>
        headers.pure[F]
