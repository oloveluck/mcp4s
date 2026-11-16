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

import io.circe.Json
import mcp4s.protocol.Codecs.given

/** A notification a server pushes to a client outside the scope of any request (progress and
  * cancellation are routed per request and never surface here).
  */
enum ServerNotification:
  /** `notifications/resources/updated`: a subscribed resource changed; re-read it. */
  case ResourceUpdated(uri: String)

  /** `notifications/resources/list_changed` */
  case ResourcesListChanged

  /** `notifications/tools/list_changed` */
  case ToolsListChanged

  /** `notifications/prompts/list_changed` */
  case PromptsListChanged

  /** `notifications/message`: a log message at or above the level set with `logging/setLevel`. */
  case Log(message: LogMessage)

  /** `notifications/tasks/status`: a task changed state. */
  case TaskStatusChanged(task: Task)

  /** Any other notification, including custom ones. */
  case Other(method: String, params: Option[Json])

object ServerNotification:
  /** Classify a raw JSON-RPC notification. Malformed params fall through to [[Other]]. */
  def fromJsonRpc(notification: JsonRpcNotification): ServerNotification =
    val JsonRpcNotification(method, params)     = notification
    def decoded[A: io.circe.Decoder]: Option[A] = params.flatMap(_.as[A].toOption)
    method match
      case McpMethod.ResourcesUpdated =>
        params.flatMap(_.hcursor.get[String]("uri").toOption) match
          case Some(uri) => ResourceUpdated(uri)
          case None      => Other(method, params)
      case McpMethod.ResourcesListChanged => ResourcesListChanged
      case McpMethod.ToolsListChanged     => ToolsListChanged
      case McpMethod.PromptsListChanged   => PromptsListChanged
      case McpMethod.LoggingMessage       =>
        decoded[LogMessage].map(Log(_)).getOrElse(Other(method, params))
      case McpMethod.TasksStatus =>
        decoded[Task].map(TaskStatusChanged(_)).getOrElse(Other(method, params))
      case _ => Other(method, params)
