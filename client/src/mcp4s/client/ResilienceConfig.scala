package mcp4s.client

import cats.effect.Temporal
import io.circe.Json
import mcp4s.client.retry.{Retry, RetryPolicy}
import mcp4s.protocol.JsonRpcRequest

import scala.concurrent.duration.*

/** Configuration for resilient MCP connections.
  *
  * Applied at transport connect time to wrap the underlying `sendRequest` function
  * with retry and timeout before constructing `McpConnection`.
  *
  * Per-attempt order:
  *   1. Timeout — applied to each individual attempt
  *   2. Retry — retries the entire operation on failure
  *
  * Example:
  * {{{
  * HttpClientTransport.connect(client, config, httpClient,
  *   resilience = Some(ResilienceConfig())
  * )
  * }}}
  */
final case class ResilienceConfig(
    retry: RetryPolicy = RetryPolicy.exponentialBackoff(),
    timeout: Option[FiniteDuration] = Some(30.seconds)
):
  def withRetry(policy: RetryPolicy): ResilienceConfig = copy(retry = policy)
  def withTimeout(duration: FiniteDuration): ResilienceConfig = copy(timeout = Some(duration))
  def withoutTimeout: ResilienceConfig = copy(timeout = None)

object ResilienceConfig:
  val default: ResilienceConfig = ResilienceConfig()

  private[client] def wrapSendRequest[F[_]: Temporal](
      sendRequest: JsonRpcRequest => F[Json],
      config: ResilienceConfig
  ): JsonRpcRequest => F[Json] = { req =>
    val withTimeout = config.timeout match
      case Some(duration) => Temporal[F].timeout(sendRequest(req), duration)
      case None           => sendRequest(req)
    Retry(config.retry)(withTimeout)
  }
