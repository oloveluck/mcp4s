package mcp4s.server

import cats.{Applicative, ApplicativeError}
import io.circe.Json
import mcp4s.protocol.*

/** Context passed to tool handlers for advanced operations.
  *
  * Provides access to server-to-client capabilities like sampling,
  * elicitation, progress reporting, and logging within tool handlers.
  *
  * @example
  * {{{
  * McpServer.builder[IO]
  *   .toolWithContext[Args]("smart-calc", "Calculate with LLM help") { (args, ctx) =>
  *     for
  *       _ <- ctx.log(LogLevel.Info, "Starting calculation")
  *       _ <- ctx.progress(0.5, Some(1.0))
  *       result <- ctx.sampling.createMessage(CreateMessageParams(
  *         messages = List(SamplingMessage(Role.User, SamplingTextContent(args.query))),
  *         maxTokens = 100
  *       ))
  *     yield ToolResult.text(result.content match {
  *       case SamplingTextContent(text) => text
  *       case _ => "Unexpected response type"
  *     })
  *   }
  *   .build
  * }}}
  */
trait ToolContext[F[_]]:

  /** Access to server-to-client sampling requests */
  def sampling: SamplingRequester[F]

  /** Access to server-to-client elicitation requests */
  def elicitation: ElicitationRequester[F]

  /** Report progress for the current tool execution.
    *
    * @param progress Current progress value
    * @param total Optional total value (for percentage calculation)
    */
  def progress(progress: Double, total: Option[Double]): F[Unit]

  /** Emit a log message to the client.
    *
    * @param level The log level
    * @param message The log message
    * @param data Optional additional data as JSON
    */
  def log(level: LogLevel, message: String, data: Option[Json] = None): F[Unit]

  /** The request ID for the current tool call, used for progress tokens */
  def requestId: RequestId

object ToolContext:

  /** Create a full tool context with all capabilities.
    *
    * @param sampler The sampling requester to use
    * @param eliciter The elicitation requester to use
    * @param reqId The request ID for progress reporting
    * @param progressFn Function to send progress notifications
    * @param loggingFn Function to send logging notifications
    */
  def apply[F[_]: Applicative](
      sampler: SamplingRequester[F],
      eliciter: ElicitationRequester[F],
      reqId: RequestId,
      progressFn: (RequestId, Double, Option[Double]) => F[Unit],
      loggingFn: (LogLevel, String, Option[Json]) => F[Unit]
  ): ToolContext[F] =
    new ToolContext[F]:
      def sampling: SamplingRequester[F] = sampler
      def elicitation: ElicitationRequester[F] = eliciter
      def requestId: RequestId = reqId
      def progress(prog: Double, total: Option[Double]): F[Unit] =
        progressFn(reqId, prog, total)
      def log(level: LogLevel, message: String, data: Option[Json]): F[Unit] =
        loggingFn(level, message, data)

  /** Create a minimal tool context without progress/logging/elicitation support */
  def minimal[F[_]](sampler: SamplingRequester[F], reqId: RequestId)(using
      F: ApplicativeError[F, Throwable]
  ): ToolContext[F] =
    MinimalToolContext(sampler, ElicitationRequester.unsupported[F], reqId)

  /** Create a minimal tool context with elicitation support */
  def minimal[F[_]: Applicative](
      sampler: SamplingRequester[F],
      eliciter: ElicitationRequester[F],
      reqId: RequestId
  ): ToolContext[F] =
    MinimalToolContext(sampler, eliciter, reqId)

  private final class MinimalToolContext[F[_]](
      val sampler: SamplingRequester[F],
      val eliciter: ElicitationRequester[F],
      val reqId: RequestId
  )(using F: Applicative[F]) extends ToolContext[F]:
    def sampling: SamplingRequester[F] = sampler
    def elicitation: ElicitationRequester[F] = eliciter
    def requestId: RequestId = reqId
    def progress(prog: Double, total: Option[Double]): F[Unit] = F.unit
    def log(level: LogLevel, message: String, data: Option[Json]): F[Unit] = F.unit
