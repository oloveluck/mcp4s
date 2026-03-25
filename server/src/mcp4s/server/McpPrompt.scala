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
  * case class GreetArgs(name: String) derives PromptInput
  *
  * val greeting = McpPrompt[IO, GreetArgs]("greet", "Greet someone") { args =>
  *   IO.pure(GetPromptResult(None, List(PromptMessage(Role.User, TextContent(s"Hi ${args.name}")))))
  * }
  * }}}
  */
trait Prompts[F[_]]:
  /** List all prompts */
  def list: F[List[Prompt]]

  /** Get a prompt by name, returning None if not handled */
  def get(name: String, arguments: Map[String, String]): OptionT[F, GetPromptResult]

object Prompts:

  def empty[F[_]: Applicative]: Prompts[F] =
    new Prompts[F]:
      def list: F[List[Prompt]] = Applicative[F].pure(Nil)
      def get(name: String, arguments: Map[String, String]): OptionT[F, GetPromptResult] = OptionT.none

  def combine[F[_]: Concurrent](x: Prompts[F], y: Prompts[F]): Prompts[F] =
    new Prompts[F]:
      def list: F[List[Prompt]] =
        for
          xPrompts <- x.list
          yPrompts <- y.list
          xNames = xPrompts.map(_.name).toSet
        yield xPrompts ++ yPrompts.filterNot(p => xNames.contains(p.name))

      def get(name: String, arguments: Map[String, String]): OptionT[F, GetPromptResult] =
        x.get(name, arguments).orElse(y.get(name, arguments))

  /** Semigroup instance for Prompts composition via |+| */
  given [F[_]: Concurrent]: Semigroup[Prompts[F]] with
    def combine(x: Prompts[F], y: Prompts[F]): Prompts[F] =
      Prompts.combine(x, y)

  extension [F[_]: Concurrent](prompts: Prompts[F])
    def <+>(other: Prompts[F]): Prompts[F] =
      combine(prompts, other)

/** Factory for creating standalone prompt values */
object McpPrompt:

  /** Create a prompt with PromptInput-based arguments */
  def apply[F[_]: Concurrent, A: PromptInput](name: String, description: String)(
      handler: A => F[GetPromptResult]
  ): Prompts[F] =
    val pi = summon[PromptInput[A]]
    val prompt = Prompt(name, Some(description), pi.arguments)
    new Prompts[F]:
      def list: F[List[Prompt]] = Applicative[F].pure(List(prompt))
      def get(promptName: String, args: Map[String, String]): OptionT[F, GetPromptResult] =
        if promptName == name then
          pi.decode(args) match
            case Right(a)  => OptionT.liftF(handler(a))
            case Left(err) => OptionT.liftF(Concurrent[F].raiseError(McpError.InvalidPromptArguments(name, err)))
        else OptionT.none[F, GetPromptResult]

  /** Create a prompt with no arguments */
  def noArgs[F[_]: Concurrent](name: String, description: String)(
      handler: F[GetPromptResult]
  ): Prompts[F] =
    val prompt = Prompt(name, Some(description), Nil)
    new Prompts[F]:
      def list: F[List[Prompt]] = Applicative[F].pure(List(prompt))
      def get(promptName: String, args: Map[String, String]): OptionT[F, GetPromptResult] =
        if promptName == name then OptionT.liftF(handler)
        else OptionT.none[F, GetPromptResult]

  /** Create a prompt from a raw map handler */
  def raw[F[_]: Concurrent](name: String, description: String, arguments: List[PromptArgument] = Nil)(
      handler: Map[String, String] => F[GetPromptResult]
  ): Prompts[F] =
    val prompt = Prompt(name, Some(description), arguments)
    new Prompts[F]:
      def list: F[List[Prompt]] = Applicative[F].pure(List(prompt))
      def get(promptName: String, args: Map[String, String]): OptionT[F, GetPromptResult] =
        if promptName == name then OptionT.liftF(handler(args))
        else OptionT.none[F, GetPromptResult]
