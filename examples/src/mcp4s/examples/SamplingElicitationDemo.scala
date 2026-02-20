package mcp4s.examples

import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import com.comcast.ip4s.*
import io.circe.Json
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.mcp
import mcp4s.server.mcp.{ok, pure}
import mcp4s.server.transport.*
import mcp4s.client.*
import mcp4s.client.{mcp => clientMcp}
import mcp4s.client.mcp.{message, accept}
import org.typelevel.otel4s.trace.Tracer

// === Mock LLM ===

/** Simple mock LLM that pattern-matches on prompts.
  * In a real application, this would call an actual LLM API.
  */
object MockLLM:

  def interpret(prompt: String): String =
    prompt.toLowerCase match
      case p if p.contains("calculate") && p.contains("+") =>
        extractAndCalculate(p, _ + _, "+")
      case p if p.contains("calculate") && p.contains("-") =>
        extractAndCalculate(p, _ - _, "-")
      case p if p.contains("calculate") && p.contains("*") =>
        extractAndCalculate(p, _ * _, "*")
      case p if p.contains("calculate") && p.contains("/") =>
        extractAndCalculate(p, (a, b) => if b != 0 then a / b else Double.NaN, "/")
      case p if p.contains("hello") || p.contains("hi") =>
        "Hello! I'm a mock LLM. How can I help you today?"
      case p if p.contains("weather") =>
        "I'm a mock LLM and don't have real-time weather data. It's probably nice outside!"
      case _ =>
        s"I received your message: '$prompt'. As a mock LLM, I can help with simple calculations."

  private def extractAndCalculate(prompt: String, op: (Double, Double) => Double, opStr: String): String =
    val numbers = """\d+\.?\d*""".r.findAllIn(prompt).toList.flatMap(_.toDoubleOption)
    numbers match
      case a :: b :: _ =>
        val result = op(a, b)
        if result.isNaN then "Cannot divide by zero"
        else s"The result of $a $opStr $b = $result"
      case _ =>
        "I couldn't find two numbers in your request. Please try again."

// === Sampling Handler ===

/** Creates a sampling handler that uses the mock LLM */
object SamplingHandlers:

  def mockLLMHandler: CreateMessageParams => IO[CreateMessageResult] =
    params =>
      val prompt = params.messages.lastOption.map { msg =>
        msg.content match
          case SamplingTextContent(text) => text
          case _                         => ""
      }.getOrElse("")

      val response = MockLLM.interpret(prompt)

      IO.pure(CreateMessageResult(
        role = Role.Assistant,
        content = SamplingTextContent(response),
        model = "mock-llm-v1",
        stopReason = Some("endTurn")
      ))

// === Elicitation Handler ===

/** Creates handlers for form and URL elicitation */
object ElicitationHandlers:

  /** Handler for elicitation requests - simulates user filling a form or completing OAuth */
  def simulatedHandler: ElicitParams => IO[ElicitResult] = {
    case ElicitFormParams(message, schema, _) =>
      IO.println(s"[Elicitation] Form request: $message") *>
      IO.println(s"[Elicitation] Schema properties: ${schema.properties.map(_.keys.mkString(", ")).getOrElse("none")}") *> {
        // Simulate user filling in the form with mock values
        val values = schema.properties.getOrElse(Map.empty).map { (key, prop) =>
          key -> (prop.`type`.get match
            case "string"  => Json.fromString(s"value_for_$key")
            case "number"  => Json.fromDoubleOrNull(42.0)
            case "boolean" => Json.fromBoolean(true)
            case _         => Json.fromString(s"mock_$key")
          )
        }
        IO.pure(ElicitResult(ElicitAction.Accept, Some(values)))
      }

    case ElicitUrlParams(message, elicitationId, url, _) =>
      IO.println(s"[Elicitation] URL request: $message") *>
      IO.println(s"[Elicitation] OAuth URL: $url (ID: $elicitationId)") *> {
        // Simulate user completing OAuth and returning a token
        IO.pure(ElicitResult(
          ElicitAction.Accept,
          Some(Map("access_token" -> Json.fromString("mock_oauth_token_12345")))
        ))
      }
  }

  /** Handler for elicitation complete notifications */
  def elicitationCompleteHandler: ElicitationCompleteParams => IO[Unit] =
    params =>
      IO.println(s"[ElicitationComplete] ID: ${params.elicitationId}, Action: ${params.result.action}") *>
      params.result.content.traverse_(content =>
        IO.println(s"[ElicitationComplete] Content: ${content.keys.mkString(", ")}")
      )

// === Demo Server with Smart Tools ===

case class DemoAddArgs(
    @description("First number") a: Double,
    @description("Second number") b: Double
) derives ToolInput

case class SmartCalcArgs(
    @description("Mathematical expression or question for the LLM") query: String
) derives ToolInput

/** Demo server that uses sampling for smart calculations */
object DemoServer extends IOApp.Simple:

  // === Tools using new DSL ===

  val tools: McpTools[IO] =
    // Regular tool using new DSL
    mcp.Tool[IO, DemoAddArgs]("add", "Add two numbers") { args =>
      ok(s"${args.a + args.b}").pure[IO]
    } |+|
    // Context-aware tool that uses sampling
    mcp.Tool.withContext[IO, SmartCalcArgs]("smart-calc", "Calculate using LLM assistance") { (args, ctx) =>
      for
        _ <- IO.println(s"[Server] smart-calc called with: ${args.query}")
        _ <- IO.println(s"[Server] Requesting LLM completion from client...")
        result <- ctx.sampling.createMessage(CreateMessageParams(
          messages = List(SamplingMessage(Role.User, SamplingTextContent(args.query))),
          maxTokens = 200
        ))
        text = result.content match
          case SamplingTextContent(t) => t
          case _                      => "Unexpected response type"
        _ <- IO.println(s"[Server] Got LLM response: $text")
      yield ok(text)
    }

  def createServer: McpServer[IO] = McpServer.fromTools[IO](
    info = ServerInfo(
      "sampling-demo-server",
      "1.0.0",
      description = Some("Demo server showing sampling and elicitation features")
    ),
    tools = tools
  )

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    IO.println("Starting Sampling Demo Server on ws://localhost:3001/ws") *>
      WebSocketTransport.serve[IO](createServer, WebSocketConfig(port = port"3001")).useForever

// === Demo Client ===

/** Demo client with sampling and elicitation handlers - using legacy builder API */
object DemoClient extends IOApp.Simple:

  def createClient: McpClient[IO] = McpClient
    .builder[IO]
    .withInfo(ClientInfo("sampling-demo-client", "1.0.0"))
    .withSamplingHandler(SamplingHandlers.mockLLMHandler)
    .withElicitationHandler(ElicitationHandlers.simulatedHandler)
    .withElicitationCompleteHandler(ElicitationHandlers.elicitationCompleteHandler)
    .build

  def run: IO[Unit] =
    IO.println("Demo Client (Builder API) created with:") *>
    IO.println("  - Sampling handler (mock LLM)") *>
    IO.println("  - Elicitation handler (simulated form/URL)") *>
    IO.println("  - Elicitation complete handler") *>
    IO.println("") *>
    IO.println("Client capabilities:") *>
    IO.println(s"  - Sampling: ${createClient.capabilities.sampling.isDefined}") *>
    IO.println(s"  - Elicitation: ${createClient.capabilities.elicitation.isDefined}")

// === Demo Client using new DSL ===

/** Demo client using the new composable DSL */
object DemoClientDsl extends IOApp.Simple:

  // Composable sampling handler using DSL - returns McpSamplings[IO]
  val sampling = clientMcp.Sampling[IO] { params =>
    val prompt = params.messages.lastOption.map { msg =>
      msg.content match
        case SamplingTextContent(text) => text
        case _                         => ""
    }.getOrElse("")

    val response = MockLLM.interpret(prompt)
    IO.pure(message(response, "mock-llm-v1"))
  }

  // Composable elicitation handler using DSL - returns McpElicitations[IO]
  val elicitation = clientMcp.Elicitation.withComplete[IO](
    handler = {
      case ElicitFormParams(msg, schema, _) =>
        val values = schema.properties.getOrElse(Map.empty).map { (key, prop) =>
          key -> (prop.`type`.get match
            case "string"  => Json.fromString(s"value_for_$key")
            case "number"  => Json.fromDoubleOrNull(42.0)
            case "boolean" => Json.fromBoolean(true)
            case _         => Json.fromString(s"mock_$key")
          )
        }
        IO.pure(accept(values))

      case ElicitUrlParams(_, _, _, _) =>
        IO.pure(accept(Map("access_token" -> Json.fromString("mock_oauth_token"))))
    },
    onComplete = params =>
      IO.println(s"[ElicitationComplete] ID: ${params.elicitationId}, Action: ${params.result.action}")
  )

  // Composable roots
  val roots = clientMcp.Roots[IO](
    Root("file:///workspace", Some("Workspace")),
    Root("file:///home", Some("Home"))
  )

  // Create client using McpClient.from
  def createClient: McpClient[IO] = McpClient.from[IO](
    info = ClientInfo("dsl-demo-client", "1.0.0"),
    roots = Some(roots),
    sampling = Some(sampling),
    elicitation = Some(elicitation)
  )

  def run: IO[Unit] =
    IO.println("Demo Client (DSL API) created with:") *>
    IO.println("  - Sampling handler (mock LLM) via mcp.Sampling") *>
    IO.println("  - Elicitation handler via mcp.Elicitation.withComplete") *>
    IO.println("  - Roots via mcp.Roots") *>
    IO.println("") *>
    IO.println("Client capabilities:") *>
    IO.println(s"  - Sampling: ${createClient.capabilities.sampling.isDefined}") *>
    IO.println(s"  - Elicitation: ${createClient.capabilities.elicitation.isDefined}") *>
    IO.println(s"  - Roots: ${createClient.capabilities.roots.isDefined}") *>
    IO.println("") *>
    IO.println("Testing the client:") *>
    testClient(createClient)

  private def testClient(client: McpClient[IO]): IO[Unit] =
    val params = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("calculate 5 + 3"))),
      maxTokens = 100
    )
    for
      samplingResult <- client.createMessage(params)
      _ <- IO.println(s"  Sampling result: model=${samplingResult.model}")
      text = samplingResult.content match
        case SamplingTextContent(t) => t
        case _                      => "non-text"
      _ <- IO.println(s"  Response: $text")
      rootsResult <- client.listRoots
      _ <- IO.println(s"  Roots: ${rootsResult.roots.map(_.uri).mkString(", ")}")
    yield ()

// === Handler Tests ===

/** Standalone tests for the handlers */
object DemoHandlersTest extends IOApp.Simple:

  def run: IO[Unit] =
    for
      _ <- IO.println("=== Testing Mock LLM ===")
      _ <- testMockLLM()
      _ <- IO.println("")
      _ <- IO.println("=== Testing Sampling Handler ===")
      _ <- testSamplingHandler()
      _ <- IO.println("")
      _ <- IO.println("=== Testing Elicitation Handler ===")
      _ <- testElicitationHandler()
      _ <- IO.println("")
      _ <- IO.println("=== All tests passed! ===")
    yield ()

  private def testMockLLM(): IO[Unit] =
    for
      _ <- IO.println(s"  'calculate 5 + 3' -> ${MockLLM.interpret("calculate 5 + 3")}")
      _ <- IO.println(s"  'calculate 10 - 4' -> ${MockLLM.interpret("calculate 10 - 4")}")
      _ <- IO.println(s"  'calculate 6 * 7' -> ${MockLLM.interpret("calculate 6 * 7")}")
      _ <- IO.println(s"  'calculate 15 / 3' -> ${MockLLM.interpret("calculate 15 / 3")}")
      _ <- IO.println(s"  'hello' -> ${MockLLM.interpret("hello")}")
    yield ()

  private def testSamplingHandler(): IO[Unit] =
    val handler = SamplingHandlers.mockLLMHandler
    val params = CreateMessageParams(
      messages = List(SamplingMessage(Role.User, SamplingTextContent("calculate 2 + 2"))),
      maxTokens = 100
    )
    for
      result <- handler(params)
      _ <- IO.println(s"  Model: ${result.model}")
      _ <- IO.println(s"  Role: ${result.role}")
      text = result.content match
        case SamplingTextContent(t) => t
        case _                      => "non-text"
      _ <- IO.println(s"  Content: $text")
    yield ()

  private def testElicitationHandler(): IO[Unit] =
    val handler = ElicitationHandlers.simulatedHandler
    val formParams = ElicitFormParams(
      message = "Please enter your API key",
      requestedSchema = JsonSchema.obj(
        "api_key" -> JsonSchema.string("Your API key")
      )
    )
    val urlParams = ElicitUrlParams(
      message = "Please complete OAuth",
      elicitationId = "oauth-123",
      url = "https://example.com/oauth"
    )
    for
      formResult <- handler(formParams)
      _ <- IO.println(s"  Form result: action=${formResult.action}, content=${formResult.content}")
      urlResult <- handler(urlParams)
      _ <- IO.println(s"  URL result: action=${urlResult.action}, content=${urlResult.content}")
    yield ()
