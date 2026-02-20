package mcp4s.server

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*

/** First-class tool values that compose via `|+|`.
  *
  * Tools are standalone typed values, not builder method calls.
  * Compose them with Semigroup / `<+>` and pass to `McpServer`.
  *
  * {{{
  * // Simple tools use convenience constructors
  * val add = McpTool.twoNumbers[IO]("add", "Add two numbers") { (a, b) =>
  *   IO.pure(ToolResult.text(s"${a + b}"))
  * }
  *
  * // Complex tools use case classes with derives ToolInput
  * case class SearchArgs(query: String, limit: Option[Int]) derives ToolInput
  * val search = McpTool[IO, SearchArgs]("search", "Search") { args =>
  *   IO.pure(ToolResult.text(s"Searching: ${args.query}"))
  * }
  *
  * val allTools: McpTools[IO] = add |+| search
  * }}}
  */
object McpTool:

  /** Create a tool from derived ToolInput */
  def apply[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => F[ToolResult]
  ): McpTools[F] =
    val ti = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    McpTools.single(tool) { json =>
      ti.decode(json) match
        case Right(a)  => handler(a)
        case Left(err) => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a tool with typed output */
  def typed[F[_]: Concurrent, A: ToolInput, B](name: String, description: String)(
      handler: A => F[B]
  )(using to: ToolOutput[B]): McpTools[F] =
    val ti = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema, outputSchema = Some(to.schema))
    McpTools.single(tool) { json =>
      ti.decode(json) match
        case Right(a) =>
          Concurrent[F].map(handler(a))(to.encode)
        case Left(err) =>
          Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a context-aware tool.
    *
    * Context-aware tools can access server-to-client operations like sampling,
    * progress notifications, and logging via the ToolContext.
    *
    * These tools can be composed with regular tools using `|+|`.
    */
  def withContext[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: (A, ToolContext[F]) => F[ToolResult]
  ): McpTools[F] =
    val ti = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema)
    McpTools.singleWithContext(tool) { (json, ctx) =>
      ti.decode(json) match
        case Right(a)  => handler(a, ctx)
        case Left(err) => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a context-aware tool with no typed arguments.
    *
    * Context-aware tools can access server-to-client operations like sampling,
    * progress notifications, and logging via the ToolContext.
    *
    * These tools can be composed with regular tools using `|+|`.
    */
  def withContextNoArgs[F[_]: Concurrent](name: String, description: String)(
      handler: ToolContext[F] => F[ToolResult]
  ): McpTools[F] =
    val tool = Tool(name, Some(description), JsonSchema.empty)
    McpTools.singleWithContext(tool) { (_, ctx) => handler(ctx) }

  /** Create a tool with annotations */
  def annotated[F[_]: Concurrent, A: ToolInput](
      name: String,
      description: String,
      annotations: ToolAnnotations
  )(handler: A => F[ToolResult]): McpTools[F] =
    val ti = summon[ToolInput[A]]
    val tool = Tool(name, Some(description), ti.schema, annotations = Some(annotations))
    McpTools.single(tool) { json =>
      ti.decode(json) match
        case Right(a)  => handler(a)
        case Left(err) => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err))
    }

  /** Create a no-argument tool */
  def noArgs[F[_]: Concurrent](name: String, description: String)(
      handler: F[ToolResult]
  ): McpTools[F] =
    val tool = Tool(name, Some(description), JsonSchema.empty)
    McpTools.single(tool)(_ => handler)

  // === Convenience Constructors ===

  /** Create a tool with a single string parameter.
    *
    * Example:
    * {{{
    * val echo = McpTool.singleString[IO]("echo", "Echo input", "message") { msg =>
    *   IO.pure(ToolResult.text(msg))
    * }
    * }}}
    */
  def singleString[F[_]: Concurrent](
      name: String,
      description: String,
      param: String = "input",
      paramDesc: String = ""
  )(handler: String => F[ToolResult]): McpTools[F] =
    val prop = JsonSchemaProperty.make("string", if paramDesc.isEmpty then None else Some(paramDesc))
    val schema = JsonSchema("object", Some(Map(param -> prop)), Some(List(param)))
    val tool = Tool(name, Some(description), schema)
    McpTools.single(tool) { json =>
      json.hcursor.get[String](param) match
        case Right(value) => handler(value)
        case Left(err)    => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err.getMessage))
    }

  /** Create a tool with a single number parameter.
    *
    * Example:
    * {{{
    * val double = McpTool.singleNumber[IO]("double", "Double a number") { n =>
    *   IO.pure(ToolResult.text(s"${n * 2}"))
    * }
    * }}}
    */
  def singleNumber[F[_]: Concurrent](
      name: String,
      description: String,
      param: String = "value",
      paramDesc: String = ""
  )(handler: Double => F[ToolResult]): McpTools[F] =
    val prop = JsonSchemaProperty.make("number", if paramDesc.isEmpty then None else Some(paramDesc))
    val schema = JsonSchema("object", Some(Map(param -> prop)), Some(List(param)))
    val tool = Tool(name, Some(description), schema)
    McpTools.single(tool) { json =>
      json.hcursor.get[Double](param) match
        case Right(value) => handler(value)
        case Left(err)    => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err.getMessage))
    }

  /** Create a tool with a single boolean parameter.
    *
    * Example:
    * {{{
    * val toggle = McpTool.singleBoolean[IO]("toggle", "Toggle a flag") { flag =>
    *   IO.pure(ToolResult.text(s"Flag is $flag"))
    * }
    * }}}
    */
  def singleBoolean[F[_]: Concurrent](
      name: String,
      description: String,
      param: String = "flag",
      paramDesc: String = ""
  )(handler: Boolean => F[ToolResult]): McpTools[F] =
    val prop = JsonSchemaProperty.make("boolean", if paramDesc.isEmpty then None else Some(paramDesc))
    val schema = JsonSchema("object", Some(Map(param -> prop)), Some(List(param)))
    val tool = Tool(name, Some(description), schema)
    McpTools.single(tool) { json =>
      json.hcursor.get[Boolean](param) match
        case Right(value) => handler(value)
        case Left(err)    => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err.getMessage))
    }

  /** Create a tool with two number parameters.
    *
    * Example:
    * {{{
    * val add = McpTool.twoNumbers[IO]("add", "Add two numbers") { (a, b) =>
    *   IO.pure(ToolResult.text(s"${a + b}"))
    * }
    * }}}
    */
  def twoNumbers[F[_]: Concurrent](
      name: String,
      description: String,
      param1: String = "a",
      param2: String = "b",
      desc1: String = "",
      desc2: String = ""
  )(handler: (Double, Double) => F[ToolResult]): McpTools[F] =
    val prop1 = JsonSchemaProperty.make("number", if desc1.isEmpty then None else Some(desc1))
    val prop2 = JsonSchemaProperty.make("number", if desc2.isEmpty then None else Some(desc2))
    val schema = JsonSchema("object", Some(Map(param1 -> prop1, param2 -> prop2)), Some(List(param1, param2)))
    val tool = Tool(name, Some(description), schema)
    McpTools.single(tool) { json =>
      val cursor = json.hcursor
      (cursor.get[Double](param1), cursor.get[Double](param2)) match
        case (Right(a), Right(b)) => handler(a, b)
        case (Left(err), _)       => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err.getMessage))
        case (_, Left(err))       => Concurrent[F].raiseError(McpError.InvalidToolArguments(name, err.getMessage))
    }

  // === Pure Result Helpers ===

  /** Create a tool with a pure string handler (auto-wrapped in F[ToolResult]).
    *
    * Example:
    * {{{
    * val echo = McpTool.pureText[IO, EchoArgs]("echo", "Echo input") { args =>
    *   args.message
    * }
    * }}}
    */
  def pureText[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => String
  ): McpTools[F] =
    apply[F, A](name, description)(a => Concurrent[F].pure(ToolResult.text(handler(a))))

  /** Create a no-argument tool with a pure string result.
    *
    * Example:
    * {{{
    * val version = McpTool.pureTextNoArgs[IO]("version", "Get version") {
    *   "1.0.0"
    * }
    * }}}
    */
  def pureTextNoArgs[F[_]: Concurrent](name: String, description: String)(
      result: => String
  ): McpTools[F] =
    noArgs[F](name, description)(Concurrent[F].pure(ToolResult.text(result)))

  /** Create a tool with a single string param and pure string result.
    *
    * Example:
    * {{{
    * val upper = McpTool.singleStringPure[IO]("upper", "Uppercase") { s =>
    *   s.toUpperCase
    * }
    * }}}
    */
  def singleStringPure[F[_]: Concurrent](
      name: String,
      description: String,
      param: String = "input",
      paramDesc: String = ""
  )(handler: String => String): McpTools[F] =
    singleString[F](name, description, param, paramDesc)(s => Concurrent[F].pure(ToolResult.text(handler(s))))

  /** Create a tool with a single number param and pure string result.
    *
    * Example:
    * {{{
    * val double = McpTool.singleNumberPure[IO]("double", "Double a number") { n =>
    *   s"${n * 2}"
    * }
    * }}}
    */
  def singleNumberPure[F[_]: Concurrent](
      name: String,
      description: String,
      param: String = "value",
      paramDesc: String = ""
  )(handler: Double => String): McpTools[F] =
    singleNumber[F](name, description, param, paramDesc)(n => Concurrent[F].pure(ToolResult.text(handler(n))))

  /** Create a tool with two number params and pure string result.
    *
    * Example:
    * {{{
    * val add = McpTool.twoNumbersPure[IO]("add", "Add two numbers") { (a, b) =>
    *   s"${a + b}"
    * }
    * }}}
    */
  def twoNumbersPure[F[_]: Concurrent](
      name: String,
      description: String,
      param1: String = "a",
      param2: String = "b",
      desc1: String = "",
      desc2: String = ""
  )(handler: (Double, Double) => String): McpTools[F] =
    twoNumbers[F](name, description, param1, param2, desc1, desc2) { (a, b) =>
      Concurrent[F].pure(ToolResult.text(handler(a, b)))
    }

  // === Auto-Error Handling Helpers ===

  /** Create a tool that automatically converts exceptions to error results.
    *
    * Unlike regular tools that propagate exceptions, `attempt` tools catch
    * all errors and return them as `ToolResult.error`. This is useful for
    * tools that interact with external systems where failures are expected.
    *
    * Example:
    * {{{
    * val fetch = McpTool.attempt[IO, FetchArgs]("fetch", "Fetch URL") { args =>
    *   httpClient.get(args.url).map(_.body)  // F[String] - errors become ToolResult.error
    * }
    * }}}
    */
  def attempt[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => F[String]
  ): McpTools[F] =
    apply[F, A](name, description) { args =>
      handler(args)
        .map(ToolResult.text)
        .handleError(e => ToolResult.error(e.getMessage))
    }

  /** Create a no-argument tool that automatically converts exceptions to error results. */
  def attemptNoArgs[F[_]: Concurrent](name: String, description: String)(
      handler: F[String]
  ): McpTools[F] =
    noArgs[F](name, description) {
      handler
        .map(ToolResult.text)
        .handleError(e => ToolResult.error(e.getMessage))
    }

  /** Create a tool with custom error message formatting.
    *
    * Example:
    * {{{
    * val query = McpTool.attemptWith[IO, QueryArgs]("query", "Run query") { args =>
    *   db.execute(args.sql).map(_.toString)
    * } { e =>
    *   s"Query failed: ${e.getMessage}"
    * }
    * }}}
    */
  def attemptWith[F[_]: Concurrent, A: ToolInput](name: String, description: String)(
      handler: A => F[String]
  )(formatError: Throwable => String): McpTools[F] =
    apply[F, A](name, description) { args =>
      handler(args)
        .map(ToolResult.text)
        .handleError(e => ToolResult.error(formatError(e)))
    }
