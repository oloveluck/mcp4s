package mcp4s.server

import cats.effect.{Clock, Concurrent}
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*

import scala.concurrent.duration.FiniteDuration

/** Middleware for MCP tools that can intercept tool calls.
  *
  * Middlewares wrap tool execution to add cross-cutting concerns like:
  *   - Logging
  *   - Metrics
  *   - Error handling
  *   - Timing
  *   - Validation
  *
  * {{{
  * val logging = Middleware.logging[IO](msg => IO.println(msg))
  * val timed = Middleware.timed[IO](d => IO.println(s"Took ${d.toMillis}ms"))
  *
  * val server = Server.fromTools[IO](
  *   info = ServerInfo("calc", "1.0.0"),
  *   tools = (myTools |+| moreTools).withMiddleware(logging, timed)
  * )
  * }}}
  */
trait Middleware[F[_]]:

  /** Wrap a tool call with middleware logic.
    *
    * @param name Tool name
    * @param args Tool arguments
    * @param next The next handler to call
    * @return Modified result
    */
  def apply(name: String, args: Json)(next: => F[ToolResult]): F[ToolResult]

object Middleware:

  /** Create a logging middleware.
    *
    * Logs tool calls before and after execution.
    */
  def logging[F[_]: Concurrent](log: String => F[Unit]): Middleware[F] =
    new Middleware[F]:
      def apply(name: String, args: Json)(next: => F[ToolResult]): F[ToolResult] =
        for
          _ <- log(s"[MCP] Calling tool: $name")
          result <- next.attempt
          _ <- result match
            case Right(r) if r.isError.getOrElse(false) =>
              log(s"[MCP] Tool $name returned error: ${r.textContent}")
            case Right(_) =>
              log(s"[MCP] Tool $name completed successfully")
            case Left(e) =>
              log(s"[MCP] Tool $name failed with exception: ${e.getMessage}")
          finalResult <- result.liftTo[F]
        yield finalResult

  /** Create a timing middleware.
    *
    * Measures and reports tool execution duration.
    */
  def timed[F[_]: Concurrent: Clock](
      onComplete: (String, FiniteDuration) => F[Unit]
  ): Middleware[F] =
    new Middleware[F]:
      def apply(name: String, args: Json)(next: => F[ToolResult]): F[ToolResult] =
        for
          start <- Clock[F].monotonic
          result <- next
          end <- Clock[F].monotonic
          duration = end - start
          _ <- onComplete(name, duration)
        yield result

  /** Create an error-catching middleware.
    *
    * Converts exceptions to ToolResult.error instead of failing.
    */
  def catchErrors[F[_]: Concurrent]: Middleware[F] =
    new Middleware[F]:
      def apply(name: String, args: Json)(next: => F[ToolResult]): F[ToolResult] =
        next.handleError { e =>
          ToolResult.error(s"Tool '$name' failed: ${e.getMessage}")
        }

  /** Create a middleware that catches specific exception types.
    *
    * @param handler Partial function to convert exceptions to error messages
    */
  def catchErrorsPartial[F[_]: Concurrent](
      handler: PartialFunction[Throwable, String]
  ): Middleware[F] =
    new Middleware[F]:
      def apply(name: String, args: Json)(next: => F[ToolResult]): F[ToolResult] =
        next.handleErrorWith { e =>
          if handler.isDefinedAt(e) then
            Concurrent[F].pure(ToolResult.error(handler(e)))
          else
            Concurrent[F].raiseError(e)
        }

  /** Create a validation middleware.
    *
    * Validates tool calls before execution.
    */
  def validate[F[_]: Concurrent](
      validator: (String, Json) => F[Option[String]]
  ): Middleware[F] =
    new Middleware[F]:
      def apply(name: String, args: Json)(next: => F[ToolResult]): F[ToolResult] =
        validator(name, args).flatMap {
          case Some(error) => Concurrent[F].pure(ToolResult.error(error))
          case None => next
        }

  /** Combine multiple middlewares.
    *
    * Middlewares are applied in order: first middleware is outermost.
    */
  def combine[F[_]](middlewares: Middleware[F]*): Middleware[F] =
    middlewares.reduceLeft { (outer, inner) =>
      new Middleware[F]:
        def apply(name: String, args: Json)(next: => F[ToolResult]): F[ToolResult] =
          outer(name, args)(inner(name, args)(next))
    }

  /** Identity middleware that does nothing */
  def identity[F[_]]: Middleware[F] =
    new Middleware[F]:
      def apply(name: String, args: Json)(next: => F[ToolResult]): F[ToolResult] = next

/** Extension methods for applying middleware to tools */
extension [F[_]: Concurrent](tools: Tools[F])

  /** Apply middleware to all tool calls.
    *
    * {{{
    * val myTools = add |+| subtract
    * val withLogging = myTools.withMiddleware(Middleware.logging[IO](println))
    * }}}
    */
  def withMiddleware(middleware: Middleware[F]): Tools[F] =
    new Tools[F]:
      def list: F[List[Tool]] = tools.list

      def call(name: String, args: Json): cats.data.OptionT[F, ToolResult] =
        // Check if tool exists by looking at tool list, then apply middleware
        cats.data.OptionT(
          tools.list.flatMap { toolList =>
            if toolList.exists(_.name == name) then
              // Tool exists, apply middleware around the actual call
              middleware(name, args)(
                tools.call(name, args).getOrElse(
                  ToolResult.error(s"Tool not found: $name")
                )
              ).map(Some(_))
            else
              // Tool doesn't exist
              Concurrent[F].pure(None)
          }
        )

      override def callWithContext(
          name: String,
          args: Json,
          context: ToolContext[F]
      ): cats.data.OptionT[F, ToolResult] =
        cats.data.OptionT(
          tools.list.flatMap { toolList =>
            if toolList.exists(_.name == name) then
              middleware(name, args)(
                tools.callWithContext(name, args, context).getOrElse(
                  ToolResult.error(s"Tool not found: $name")
                )
              ).map(Some(_))
            else
              Concurrent[F].pure(None)
          }
        )

  /** Apply multiple middlewares to all tool calls.
    *
    * Middlewares are applied in order: first is outermost.
    */
  def withMiddleware(first: Middleware[F], rest: Middleware[F]*): Tools[F] =
    tools.withMiddleware(Middleware.combine(first +: rest*))
