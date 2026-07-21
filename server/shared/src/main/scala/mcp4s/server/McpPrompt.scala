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

import cats.{Applicative, Semigroup}
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import mcp4s.protocol.*

/** Composable prompt routes for MCP servers.
  *
  * Prompts are standalone typed values that compose via `|+|`.
  *
  * {{{
  * import mcp4s.server.dsl.*
  *
  * case class GreetArgs(name: String) derives Schema
  *
  * val greeting = Prompt.from[GreetArgs].withDescription("Greet someone").handle[IO] { args =>
  *   IO.pure(messages(user(s"Hi ${args.name}")))
  * }
  * }}}
  */
trait Prompts[F[_]]:
  /** List all prompts */
  def list: F[List[Prompt]]

  /** Get a prompt by name, returning None if not handled */
  def get(name: String, arguments: Map[String, String]): OptionT[F, GetPromptResult]

  /** True when no prompts are registered. Used to derive server capabilities. */
  def isEmpty: Boolean = false

  /** Name-indexed handlers when statically known; `None` for dynamic implementations. Lets
    * [[Prompts.combine]] dispatch by Map lookup instead of scanning the composition chain.
    */
  private[server] def handlers: Option[Map[String, Map[String, String] => F[GetPromptResult]]] =
    None

object Prompts:

  def empty[F[_]: Applicative]: Prompts[F] =
    new Prompts[F]:
      def list: F[List[Prompt]] = Applicative[F].pure(Nil)
      def get(name: String, arguments: Map[String, String]): OptionT[F, GetPromptResult] =
        OptionT.none
      override def isEmpty: Boolean         = true
      override private[server] val handlers = Some(Map.empty)

  /** Create prompt routes from a raw Prompt definition and a map-based handler. */
  def single[F[_]: Concurrent](prompt: Prompt)(
      handler: Map[String, String] => F[GetPromptResult]
  ): Prompts[F] =
    McpPrompt.raw[F](prompt.name, prompt.description.getOrElse(""), prompt.arguments)(handler)

  def combine[F[_]: Concurrent](x: Prompts[F], y: Prompts[F]): Prompts[F] =
    new Prompts[F]:
      override def isEmpty: Boolean = x.isEmpty && y.isEmpty
      // Left side wins on duplicate names, matching the orElse chain's shadowing.
      override private[server] val handlers =
        (x.handlers, y.handlers).mapN((xh, yh) => yh ++ xh)
      def list: F[List[Prompt]] =
        for
          xPrompts <- x.list
          yPrompts <- y.list
          xNames = xPrompts.map(_.name).toSet
        yield xPrompts ++ yPrompts.filterNot(p => xNames.contains(p.name))

      def get(name: String, arguments: Map[String, String]): OptionT[F, GetPromptResult] =
        handlers match
          case Some(table) => OptionT(table.get(name).traverse(h => h(arguments)))
          case None        => x.get(name, arguments).orElse(y.get(name, arguments))

  /** Semigroup instance for Prompts composition via |+| */
  given [F[_]: Concurrent]: Semigroup[Prompts[F]] with
    def combine(x: Prompts[F], y: Prompts[F]): Prompts[F] =
      Prompts.combine(x, y)

/** Internal prompt factory. Use `Prompt` from `import mcp4s.server.dsl.*` instead. */
private[server] object McpPrompt:

  /** Create a prompt from a raw map handler */
  def raw[F[_]: Concurrent](
      name: String,
      description: String,
      arguments: List[PromptArgument] = Nil
  )(
      handler: Map[String, String] => F[GetPromptResult]
  ): Prompts[F] =
    val prompt = Prompt(name, Some(description), arguments)
    new Prompts[F]:
      def list: F[List[Prompt]]             = Applicative[F].pure(List(prompt))
      override private[server] val handlers = Some(Map(name -> handler))
      def get(promptName: String, args: Map[String, String]): OptionT[F, GetPromptResult] =
        if promptName == name then OptionT.liftF(handler(args))
        else OptionT.none[F, GetPromptResult]
