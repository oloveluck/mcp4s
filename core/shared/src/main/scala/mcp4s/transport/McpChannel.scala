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

package mcp4s.transport

import fs2.Stream
import mcp4s.protocol.JsonRpcMessage

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** A duplex, message-level MCP transport channel.
  *
  * This is the single contract every transport implements: deliver outbound JSON-RPC messages
  * ([[send]]) and surface inbound ones as a stream ([[incoming]]). Everything above the channel —
  * request/response correlation, the initialize handshake, timeouts, routing of server-initiated
  * requests — lives in one shared runner, so transports only deal with framing and I/O.
  *
  * Implementations must make [[send]] safe to call concurrently. [[incoming]] is consumed by a
  * single reader; the stream terminating signals that the connection is closed.
  */
trait McpChannel[F[_]]:
  /** Send one message to the peer. */
  def send(message: JsonRpcMessage): F[Unit]

  /** All messages arriving from the peer. Terminates when the connection closes. */
  def incoming: Stream[F, JsonRpcMessage]

/** Timeouts shared by every transport.
  *
  * @param request
  *   how long to wait for the response to an outstanding request (also applies to server-initiated
  *   requests such as sampling, which can involve a human or an LLM — hence the generous default)
  * @param init
  *   how long to wait for the whole connect + initialize handshake
  */
final case class Timeouts(
    request: FiniteDuration = 5.minutes,
    init: FiniteDuration = 30.seconds
)

object Timeouts:
  val default: Timeouts = Timeouts()
