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

import cats.effect.{Deferred, Fiber, Ref, Temporal}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*

import scala.concurrent.duration.FiniteDuration

/** The session's task table (SEP-1686): runs task-augmented requests in background fibers, tracks
  * their lifecycle, retains settled outcomes for their TTL, and answers `tasks/get`,
  * `tasks/result`, `tasks/cancel` and `tasks/list`.
  */
final private[server] class TaskRegistry[F[_]](
    config: Dispatcher.TaskConfig,
    tasks: Ref[F, Map[String, TaskRegistry.Entry[F]]],
    counter: Ref[F, Long]
)(using F: Temporal[F]):
  import TaskRegistry.*

  /** Register a new task and start `work` in its own fiber. `work` must not fail: it reports how
    * the task settled. Returns the task in its initial `working` state.
    */
  def create(ttl: Option[Long])(work: F[Settled]): F[Task] =
    for
      now <- F.realTime
      _   <- pruneExpired(now)
      n   <- counter.updateAndGet(_ + 1)
      id      = s"${now.toMillis.toHexString}-$n"
      created = Iso8601.format(now.toMillis)
      task    = Task(
        taskId = id,
        status = TaskStatus.Working,
        createdAt = created,
        lastUpdatedAt = created,
        ttl = ttl,
        pollInterval = Some(config.pollInterval.toMillis)
      )
      outcome  <- Deferred[F, Either[JsonRpcError, Json]]
      admitted <- tasks.modify { current =>
        if current.size >= config.maxTasks then (current, false)
        else (current + (id -> Entry(task, now.toMillis, outcome, None)), true)
      }
      _ <- F.raiseUnless(admitted)(
        McpError.InternalError(s"Task limit (${config.maxTasks}) reached")
      )
      fiber <- work.flatMap(settle(id, _)).start
      _     <- tasks.update(current =>
        current.get(id).fold(current)(e => current.updated(id, e.copy(fiber = Some(fiber))))
      )
    yield task

  /** The task's current state; `InvalidParams` for unknown (or expired) ids. */
  def get(id: String): F[Task] = entry(id).map(_.task)

  /** The task's outcome, awaited until it settles; the task's error if it failed or was cancelled.
    */
  def result(id: String): F[Json] =
    entry(id)
      .flatMap(_.outcome.get)
      .flatMap:
        case Right(json) => json.pure[F]
        case Left(err)   => McpError.fromJsonRpcError(err).raiseError[F, Json]

  /** Cancel a running task. Terminal tasks cannot be cancelled (`InvalidParams`). */
  def cancel(id: String): F[Task] =
    entry(id).flatMap { e =>
      F.raiseWhen(e.task.status.isTerminal)(
        McpError.InvalidParams(s"Task $id is already ${e.task.status.toString.toLowerCase}")
      ) *>
        e.fiber.traverse_(_.cancel) *>
        transition(id, TaskStatus.Cancelled, Some("Cancelled by client")).flatMap { moved =>
          e.outcome
            .complete(Left(JsonRpcError(JsonRpcErrorCode.RequestCancelled, "Task cancelled", None)))
            .as(moved.fold(e.task)(_.task))
        }
    }

  /** One page of tasks in creation order; the cursor is an offset. */
  def list(cursor: Option[String]): F[ListTasksResult] =
    val offset = cursor.flatMap(_.toIntOption).getOrElse(0)
    F.realTime.flatMap(pruneExpired) *> tasks.get.map { current =>
      val all  = current.values.toList.sortBy(_.createdAtMillis).map(_.task)
      val next = offset + config.pageSize
      ListTasksResult(all.slice(offset, next), Option.when(next < all.size)(next.toString))
    }

  /** Cancel every running task and drop the table. */
  def close: F[Unit] =
    tasks
      .getAndSet(Map.empty)
      .flatMap(_.values.toList.traverse_ { e =>
        e.fiber.traverse_(_.cancel) *>
          e.outcome.complete(Left(JsonRpcError.internalError("Session closed"))).void
      })

  private def entry(id: String): F[Entry[F]] =
    F.realTime.flatMap(pruneExpired) *> tasks.get
      .map(_.get(id))
      .flatMap:
        case Some(e) => e.pure[F]
        case None    => McpError.InvalidParams(s"Task not found: $id").raiseError[F, Entry[F]]

  private def settle(id: String, settled: Settled): F[Unit] =
    transition(id, settled.status, settled.message).flatMap(
      _.traverse_(_.outcome.complete(settled.outcome).void)
    )

  /** Move a task to `status` unless it is already terminal; the updated entry if it moved. */
  private def transition(
      id: String,
      status: TaskStatus,
      message: Option[String]
  ): F[Option[Entry[F]]] =
    F.realTime.flatMap { now =>
      tasks.modify { current =>
        current.get(id) match
          case Some(e) if !e.task.status.isTerminal =>
            val updated = e.copy(task =
              e.task.copy(
                status = status,
                lastUpdatedAt = Iso8601.format(now.toMillis),
                statusMessage = message
              )
            )
            (current.updated(id, updated), Some(updated))
          case _ => (current, None)
      }
    }

  private def pruneExpired(now: FiniteDuration): F[Unit] =
    tasks
      .modify { current =>
        val (expired, live) = current.partition { (_, e) =>
          e.task.ttl.exists(ttl => now.toMillis - e.createdAtMillis > ttl)
        }
        (live, expired.values.toList)
      }
      .flatMap(_.traverse_(_.fiber.traverse_(_.cancel)))

private[server] object TaskRegistry:

  def apply[F[_]: Temporal](config: Dispatcher.TaskConfig): F[TaskRegistry[F]] =
    for
      tasks   <- Ref.of[F, Map[String, Entry[F]]](Map.empty)
      counter <- Ref.of[F, Long](0L)
    yield new TaskRegistry(config, tasks, counter)

  /** How a task's work ended: its terminal status plus the outcome `tasks/result` returns. */
  final case class Settled(
      status: TaskStatus,
      outcome: Either[JsonRpcError, Json],
      message: Option[String] = None
  )

  final case class Entry[F[_]](
      task: Task,
      createdAtMillis: Long,
      outcome: Deferred[F, Either[JsonRpcError, Json]],
      fiber: Option[Fiber[F, Throwable, Unit]]
  )

/** Minimal ISO 8601 (UTC, millisecond precision) formatting without `java.time`, which is not
  * available on every platform mcp4s targets.
  */
private[server] object Iso8601:
  def format(epochMillis: Long): String =
    val days    = Math.floorDiv(epochMillis, 86400000L)
    val msOfDay = Math.floorMod(epochMillis, 86400000L)
    // Civil-from-days (Howard Hinnant's algorithm).
    val z    = days + 719468
    val era  = Math.floorDiv(z, 146097L)
    val doe  = z - era * 146097
    val yoe  = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    val y    = yoe + era * 400
    val doy  = doe - (365 * yoe + yoe / 4 - yoe / 100)
    val mp   = (5 * doy + 2) / 153
    val d    = doy - (153 * mp + 2) / 5 + 1
    val m    = if mp < 10 then mp + 3 else mp - 9
    val year = if m <= 2 then y + 1 else y
    val hh   = msOfDay / 3600000
    val mm   = (msOfDay / 60000) % 60
    val ss   = (msOfDay / 1000)  % 60
    val ms   = msOfDay           % 1000
    f"$year%04d-$m%02d-$d%02dT$hh%02d:$mm%02d:$ss%02d.$ms%03dZ"
