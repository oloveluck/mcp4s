package mcp4s.protocol

import io.circe.Json

/** Elicitation types for server-initiated user input requests
  * Spec ref: https://modelcontextprotocol.io/specification/2025-11-25/client/elicitation
  * Spec ref: schema.ts elicitation/create
  */

/** Parameters for elicitation/create request
  * Spec ref: schema.ts ElicitRequestParams
  */
sealed trait ElicitParams

/** Form-based elicitation parameters
  * Spec ref: schema.ts ElicitRequestFormParams
  */
final case class ElicitFormParams(
    message: String,
    requestedSchema: JsonSchema,
    mode: Option[String] = Some("form")
) extends ElicitParams

/** URL-based elicitation parameters
  * Spec ref: schema.ts ElicitRequestURLParams
  */
final case class ElicitUrlParams(
    message: String,
    elicitationId: String,
    url: String,
    mode: String = "url"
) extends ElicitParams

/** User action in response to elicitation
  * Spec ref: schema.ts ElicitResult action field
  */
sealed trait ElicitAction

object ElicitAction:
  case object Accept extends ElicitAction
  case object Decline extends ElicitAction
  case object Cancel extends ElicitAction

/** Result of elicitation/create
  * Spec ref: schema.ts ElicitResult
  */
final case class ElicitResult(
    action: ElicitAction,
    content: Option[Map[String, Json]] = None  // Form field values when accepted
)

/** Parameters for notifications/elicitation/complete
  * Sent by server to notify client that URL mode elicitation completed
  * Spec ref: schema.ts ElicitationCompleteNotification
  */
final case class ElicitationCompleteParams(
    elicitationId: String,
    result: ElicitResult
)
