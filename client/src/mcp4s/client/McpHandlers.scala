package mcp4s.client

import cats.{Applicative, Semigroup}
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import mcp4s.protocol.*

/** Composable sampling handler for server-initiated LLM requests.
  *
  * Handlers can be combined using the Semigroup instance, which will
  * try each handler in order until one succeeds.
  *
  * Example:
  * {{{
  * val mock = McpSamplings[IO](params => IO.pure(result))
  * val fallback = McpSamplings[IO](params => IO.pure(defaultResult))
  * val combined = mock |+| fallback
  * }}}
  */
trait McpSamplings[F[_]]:

  /** Handle a sampling/createMessage request.
    *
    * @return OptionT with Some(result) if handled, None to try next handler
    */
  def handle(params: CreateMessageParams): OptionT[F, CreateMessageResult]

object McpSamplings:

  /** Create a sampling handler from a function.
    *
    * The function is wrapped to always return Some, indicating it handles
    * all requests.
    */
  def apply[F[_]: Concurrent](handler: CreateMessageParams => F[CreateMessageResult]): McpSamplings[F] =
    new McpSamplings[F]:
      def handle(params: CreateMessageParams): OptionT[F, CreateMessageResult] =
        OptionT.liftF(handler(params))

  /** Create an empty sampling handler that handles nothing. */
  def empty[F[_]: Applicative]: McpSamplings[F] =
    new McpSamplings[F]:
      def handle(params: CreateMessageParams): OptionT[F, CreateMessageResult] =
        OptionT.none[F, CreateMessageResult]

  /** Semigroup instance that combines handlers by trying each in order. */
  given [F[_]: Concurrent]: Semigroup[McpSamplings[F]] with
    def combine(x: McpSamplings[F], y: McpSamplings[F]): McpSamplings[F] =
      new McpSamplings[F]:
        def handle(params: CreateMessageParams): OptionT[F, CreateMessageResult] =
          x.handle(params).orElse(y.handle(params))

/** Composable elicitation handler for server-initiated user input requests.
  *
  * Handlers can be combined using the Semigroup instance, which will
  * try each handler in order until one succeeds.
  *
  * Example:
  * {{{
  * val formHandler = McpElicitations[IO](params => IO.pure(accept))
  * val urlHandler = McpElicitations[IO](params => IO.pure(decline))
  * val combined = formHandler |+| urlHandler
  * }}}
  */
trait McpElicitations[F[_]]:

  /** Handle an elicitation/create request.
    *
    * @return OptionT with Some(result) if handled, None to try next handler
    */
  def handle(params: ElicitParams): OptionT[F, ElicitResult]

  /** Handle an elicitation complete notification. */
  def onComplete(params: ElicitationCompleteParams): F[Unit]

object McpElicitations:

  /** Create an elicitation handler from a function.
    *
    * The function is wrapped to always return Some, indicating it handles
    * all requests. The onComplete handler is a no-op.
    */
  def apply[F[_]: Concurrent](handler: ElicitParams => F[ElicitResult]): McpElicitations[F] =
    new McpElicitations[F]:
      def handle(params: ElicitParams): OptionT[F, ElicitResult] =
        OptionT.liftF(handler(params))
      def onComplete(params: ElicitationCompleteParams): F[Unit] =
        Concurrent[F].unit

  /** Create an elicitation handler with a complete handler. */
  def withComplete[F[_]: Concurrent](
      handler: ElicitParams => F[ElicitResult],
      completeHandler: ElicitationCompleteParams => F[Unit]
  ): McpElicitations[F] =
    new McpElicitations[F]:
      def handle(params: ElicitParams): OptionT[F, ElicitResult] =
        OptionT.liftF(handler(params))
      def onComplete(params: ElicitationCompleteParams): F[Unit] =
        completeHandler(params)

  /** Create an empty elicitation handler that handles nothing. */
  def empty[F[_]: Applicative]: McpElicitations[F] =
    new McpElicitations[F]:
      def handle(params: ElicitParams): OptionT[F, ElicitResult] =
        OptionT.none[F, ElicitResult]
      def onComplete(params: ElicitationCompleteParams): F[Unit] =
        Applicative[F].unit

  /** Semigroup instance that combines handlers by trying each in order. */
  given [F[_]: Concurrent]: Semigroup[McpElicitations[F]] with
    def combine(x: McpElicitations[F], y: McpElicitations[F]): McpElicitations[F] =
      new McpElicitations[F]:
        def handle(params: ElicitParams): OptionT[F, ElicitResult] =
          x.handle(params).orElse(y.handle(params))
        def onComplete(params: ElicitationCompleteParams): F[Unit] =
          x.onComplete(params) *> y.onComplete(params)

/** Composable roots provider for exposing filesystem boundaries to servers.
  *
  * Roots can be combined using the Semigroup instance, which will
  * concatenate the root lists.
  *
  * Example:
  * {{{
  * val workspace = McpRoots[IO](Root("file:///workspace", Some("Workspace")))
  * val home = McpRoots[IO](Root("file:///home", Some("Home")))
  * val combined = workspace |+| home
  * }}}
  */
trait McpRoots[F[_]]:

  /** List all roots. */
  def list: F[List[Root]]

object McpRoots:

  /** Create a roots provider from varargs of roots. */
  def apply[F[_]: Applicative](roots: Root*): McpRoots[F] =
    new McpRoots[F]:
      def list: F[List[Root]] = Applicative[F].pure(roots.toList)

  /** Convenience constructor for a single root with URI and name. */
  def apply[F[_]: Applicative](uri: String, name: String): McpRoots[F] =
    apply[F](Root(uri, Some(name)))

  /** Create an empty roots provider with no roots. */
  def empty[F[_]: Applicative]: McpRoots[F] =
    new McpRoots[F]:
      def list: F[List[Root]] = Applicative[F].pure(Nil)

  /** Semigroup instance that concatenates root lists. */
  given [F[_]: Applicative]: Semigroup[McpRoots[F]] with
    def combine(x: McpRoots[F], y: McpRoots[F]): McpRoots[F] =
      new McpRoots[F]:
        def list: F[List[Root]] =
          (x.list, y.list).mapN(_ ++ _)
