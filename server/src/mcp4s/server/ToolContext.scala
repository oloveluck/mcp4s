package mcp4s.server

import cats.Applicative
import mcp4s.protocol.*

/** Context passed to tool handlers for advanced operations.
  *
  * Provides access to server-to-client capabilities like sampling
  * and progress reporting within tool handlers.
  *
  * @example
  * {{{
  * McpServer.builder[IO]
  *   .toolWithContext[Args]("smart-calc", "Calculate with LLM help") { (args, ctx) =>
  *     for
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

  /** Report progress for the current tool execution.
    *
    * @param progress Current progress value
    * @param total Optional total value (for percentage calculation)
    */
  def progress(progress: Double, total: Option[Double]): F[Unit]

  /** The request ID for the current tool call, used for progress tokens */
  def requestId: RequestId

object ToolContext:

  /** Create a basic tool context with the given sampling requester.
    *
    * @param sampler The sampling requester to use
    * @param reqId The request ID for progress reporting
    * @param progressFn Function to send progress notifications
    */
  def apply[F[_]: Applicative](
      sampler: SamplingRequester[F],
      reqId: RequestId,
      progressFn: (RequestId, Double, Option[Double]) => F[Unit]
  ): ToolContext[F] =
    new ToolContext[F]:
      def sampling: SamplingRequester[F] = sampler
      def requestId: RequestId = reqId
      def progress(prog: Double, total: Option[Double]): F[Unit] =
        progressFn(reqId, prog, total)

  /** Create a minimal tool context without progress support */
  def minimal[F[_]: Applicative](sampler: SamplingRequester[F], reqId: RequestId): ToolContext[F] =
    MinimalToolContext(sampler, reqId)

  private final case class MinimalToolContext[F[_]: Applicative](
      sampler: SamplingRequester[F],
      reqId: RequestId
  ) extends ToolContext[F]:
    def sampling: SamplingRequester[F] = sampler
    def requestId: RequestId = reqId
    def progress(prog: Double, total: Option[Double]): F[Unit] = Applicative[F].unit
