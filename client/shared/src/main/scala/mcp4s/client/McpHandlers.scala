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

package mcp4s.client

import cats.{Applicative, Semigroup}
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import mcp4s.protocol.*

/** Composable sampling handler for server-initiated LLM requests.
  *
  * Handlers can be combined using the Semigroup instance, which will try each handler in order
  * until one succeeds.
  *
  * Example:
  * {{{
  * val mock = Samplings[IO](params => IO.pure(result))
  * val fallback = Samplings[IO](params => IO.pure(defaultResult))
  * val combined = mock |+| fallback
  * }}}
  */
trait Samplings[F[_]]:

  /** Handle a sampling/createMessage request.
    *
    * @return
    *   OptionT with Some(result) if handled, None to try next handler
    */
  def handle(params: CreateMessageParams): OptionT[F, CreateMessageResult]

object Samplings:

  /** Create a sampling handler from a function.
    *
    * The function is wrapped to always return Some, indicating it handles all requests.
    */
  def apply[F[_]: Concurrent](
      handler: CreateMessageParams => F[CreateMessageResult]
  ): Samplings[F] =
    new Samplings[F]:
      def handle(params: CreateMessageParams): OptionT[F, CreateMessageResult] =
        OptionT.liftF(handler(params))

  /** Create an empty sampling handler that handles nothing. */
  def empty[F[_]: Applicative]: Samplings[F] =
    new Samplings[F]:
      def handle(params: CreateMessageParams): OptionT[F, CreateMessageResult] =
        OptionT.none[F, CreateMessageResult]

  /** Semigroup instance that combines handlers by trying each in order. */
  given [F[_]: Concurrent]: Semigroup[Samplings[F]] with
    def combine(x: Samplings[F], y: Samplings[F]): Samplings[F] =
      new Samplings[F]:
        def handle(params: CreateMessageParams): OptionT[F, CreateMessageResult] =
          x.handle(params).orElse(y.handle(params))

/** Composable elicitation handler for server-initiated user input requests.
  *
  * Handlers can be combined using the Semigroup instance, which will try each handler in order
  * until one succeeds.
  *
  * Example:
  * {{{
  * val formHandler = Elicitations[IO](params => IO.pure(accept))
  * val urlHandler = Elicitations[IO](params => IO.pure(decline))
  * val combined = formHandler |+| urlHandler
  * }}}
  */
trait Elicitations[F[_]]:

  /** Handle an elicitation/create request.
    *
    * @return
    *   OptionT with Some(result) if handled, None to try next handler
    */
  def handle(params: ElicitParams): OptionT[F, ElicitResult]

  /** Handle an elicitation complete notification. */
  def onComplete(params: ElicitationCompleteParams): F[Unit]

object Elicitations:

  /** Create an elicitation handler from a function.
    *
    * The function is wrapped to always return Some, indicating it handles all requests. The
    * onComplete handler is a no-op.
    */
  def apply[F[_]: Concurrent](handler: ElicitParams => F[ElicitResult]): Elicitations[F] =
    new Elicitations[F]:
      def handle(params: ElicitParams): OptionT[F, ElicitResult] =
        OptionT.liftF(handler(params))
      def onComplete(params: ElicitationCompleteParams): F[Unit] =
        Concurrent[F].unit

  /** Create an elicitation handler with a complete handler. */
  def withComplete[F[_]: Concurrent](
      handler: ElicitParams => F[ElicitResult],
      completeHandler: ElicitationCompleteParams => F[Unit]
  ): Elicitations[F] =
    new Elicitations[F]:
      def handle(params: ElicitParams): OptionT[F, ElicitResult] =
        OptionT.liftF(handler(params))
      def onComplete(params: ElicitationCompleteParams): F[Unit] =
        completeHandler(params)

  /** Create an empty elicitation handler that handles nothing. */
  def empty[F[_]: Applicative]: Elicitations[F] =
    new Elicitations[F]:
      def handle(params: ElicitParams): OptionT[F, ElicitResult] =
        OptionT.none[F, ElicitResult]
      def onComplete(params: ElicitationCompleteParams): F[Unit] =
        Applicative[F].unit

  /** Semigroup instance that combines handlers by trying each in order. */
  given [F[_]: Concurrent]: Semigroup[Elicitations[F]] with
    def combine(x: Elicitations[F], y: Elicitations[F]): Elicitations[F] =
      new Elicitations[F]:
        def handle(params: ElicitParams): OptionT[F, ElicitResult] =
          x.handle(params).orElse(y.handle(params))
        def onComplete(params: ElicitationCompleteParams): F[Unit] =
          x.onComplete(params) *> y.onComplete(params)

/** Composable roots provider for exposing filesystem boundaries to servers.
  *
  * Roots can be combined using the Semigroup instance, which will concatenate the root lists.
  *
  * Example:
  * {{{
  * val workspace = Roots[IO](Root("file:///workspace", Some("Workspace")))
  * val home = Roots[IO](Root("file:///home", Some("Home")))
  * val combined = workspace |+| home
  * }}}
  */
trait Roots[F[_]]:

  /** List all roots. */
  def list: F[List[Root]]

object Roots:

  /** Create a roots provider from varargs of roots. */
  def apply[F[_]: Applicative](roots: Root*): Roots[F] =
    new Roots[F]:
      def list: F[List[Root]] = Applicative[F].pure(roots.toList)

  /** Convenience constructor for a single root with URI and name. */
  def apply[F[_]: Applicative](uri: String, name: String): Roots[F] =
    apply[F](Root(uri, Some(name)))

  /** Create an empty roots provider with no roots. */
  def empty[F[_]: Applicative]: Roots[F] =
    new Roots[F]:
      def list: F[List[Root]] = Applicative[F].pure(Nil)

  /** Semigroup instance that concatenates root lists. */
  given [F[_]: Applicative]: Semigroup[Roots[F]] with
    def combine(x: Roots[F], y: Roots[F]): Roots[F] =
      new Roots[F]:
        def list: F[List[Root]] =
          (x.list, y.list).mapN(_ ++ _)
