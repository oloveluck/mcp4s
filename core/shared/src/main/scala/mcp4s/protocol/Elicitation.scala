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

/** Elicitation types for server-initiated user input requests Spec ref:
  * https://modelcontextprotocol.io/specification/2025-11-25/client/elicitation Spec ref: schema.ts
  * elicitation/create
  */

/** Parameters for elicitation/create request Spec ref: schema.ts ElicitRequestParams
  */
sealed trait ElicitParams

/** Form-based elicitation parameters Spec ref: schema.ts ElicitRequestFormParams
  *
  * The wire-level `mode` discriminator ("form"/"url") is implied by the variant and handled by the
  * codecs; it is not part of the model.
  */
final case class ElicitFormParams(
    message: String,
    requestedSchema: JsonSchema
) extends ElicitParams

/** URL-based elicitation parameters Spec ref: schema.ts ElicitRequestURLParams
  */
final case class ElicitUrlParams(
    message: String,
    elicitationId: String,
    url: String
) extends ElicitParams

/** User action in response to elicitation Spec ref: schema.ts ElicitResult action field
  */
enum ElicitAction:
  case Accept, Decline, Cancel

/** Result of elicitation/create Spec ref: schema.ts ElicitResult
  */
final case class ElicitResult(
    action: ElicitAction,
    content: Option[Map[String, Json]] = None // Form field values when accepted
)

/** Parameters for notifications/elicitation/complete Sent by server to notify client that URL mode
  * elicitation completed Spec ref: schema.ts ElicitationCompleteNotification
  */
final case class ElicitationCompleteParams(
    elicitationId: String,
    result: ElicitResult
)
