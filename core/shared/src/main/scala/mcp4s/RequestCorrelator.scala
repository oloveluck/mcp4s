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

package mcp4s

import cats.effect.{Deferred, Ref, Temporal}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*

import scala.concurrent.duration.FiniteDuration

/** Correlates request/response pairs over an asynchronous, bidirectional transport.
  *
  * Every duplex MCP transport (HTTP SSE sessions, WebSocket — on both the server and client side)
  * needs the same machinery: allocate a fresh request id, park a [[cats.effect.Deferred]] keyed by
  * that id, send the request, await the matching response (deregistering on completion), and fail
  * everything still in flight when the connection drops. This captures that pattern once.
  */
final class RequestCorrelator[F[_]] private (
    pending: Ref[F, Map[RequestId, Deferred[F, Either[JsonRpcError, Json]]]],
    counter: Ref[F, Long]
)(using F: Temporal[F]):

  /** Allocate the next monotonically-increasing numeric request id. */
  def nextId: F[RequestId] =
    counter.updateAndGet(_ + 1).map(RequestId.NumberId(_))

  /** Number of requests currently awaiting a response. */
  def inFlightCount: F[Int] = pending.get.map(_.size)

  /** Complete a pending request with a successful result. No-op if the id is unknown. */
  def complete(id: RequestId, result: Json): F[Unit] =
    pending.flatModify: map =>
      map.get(id) match
        case Some(deferred) => (map - id, deferred.complete(Right(result)).void)
        case None           => (map, F.unit)

  /** Complete a pending request with an error. No-op if the id is unknown. */
  def fail(id: RequestId, error: JsonRpcError): F[Unit] =
    pending.flatModify: map =>
      map.get(id) match
        case Some(deferred) => (map - id, deferred.complete(Left(error)).void)
        case None           => (map, F.unit)

  /** Fail every in-flight request with `error` and clear the registry (e.g. on disconnect). */
  def cancelAll(error: JsonRpcError): F[Unit] =
    pending
      .getAndSet(Map.empty)
      .flatMap: inflight =>
        inflight.values.toList.traverse_(_.complete(Left(error)).attempt.void)

  /** Register `id`, run `send`, and await the correlated response, always deregistering when done.
    *
    * Raises [[mcp4s.protocol.McpError]] if the response is an error, or if no response arrives
    * within `timeout`.
    */
  def request(id: RequestId, timeout: FiniteDuration)(send: F[Unit]): F[Json] =
    register(id).bracket { deferred =>
      F.race(F.sleep(timeout), send *> deferred.get)
        .flatMap:
          case Left(_) => F.raiseError(McpError.InternalError(s"Request timed out after $timeout"))
          case Right(Right(json)) => F.pure(json)
          case Right(Left(err))   => F.raiseError(McpError.fromJsonRpcError(err, id))
    }(_ => remove(id))

  /** Like [[request]] but without a timeout — awaits the response indefinitely. */
  def requestUntimed(id: RequestId)(send: F[Unit]): F[Json] =
    register(id).bracket { deferred =>
      (send *> deferred.get)
        .flatMap:
          case Right(json) => F.pure(json)
          case Left(err)   => F.raiseError(McpError.fromJsonRpcError(err, id))
    }(_ => remove(id))

  private def register(id: RequestId): F[Deferred[F, Either[JsonRpcError, Json]]] =
    Deferred[F, Either[JsonRpcError, Json]].flatTap(d => pending.update(_ + (id -> d)))

  private def remove(id: RequestId): F[Unit] = pending.update(_ - id)

object RequestCorrelator:
  def apply[F[_]: Temporal]: F[RequestCorrelator[F]] =
    for
      pending <- Ref.of[F, Map[RequestId, Deferred[F, Either[JsonRpcError, Json]]]](Map.empty)
      counter <- Ref.of[F, Long](0L)
    yield new RequestCorrelator(pending, counter)
