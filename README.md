# MCP4S

A Scala implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) for the Typelevel ecosystem.

## Overview

MCP4S provides a type-safe, functional implementation of MCP for Scala 3 using cats-effect, fs2, http4s, and circe. It enables Scala applications to act as MCP servers (providing tools, resources, and prompts) or clients (consuming MCP servers).

## Modules

- **core** - Protocol types, JSON-RPC messages, and codec definitions
- **server** - MCP server implementation with Streamable HTTP and stdio transports
- **client** - MCP client for connecting to MCP servers
- **agent** - Autonomous agent loop connecting LLMs to MCP tools

## Installation

Add to your `build.mill`:

```scala
def ivyDeps = Agg(
  ivy"io.github.mcp4s::mcp4s-core::0.1.4",
  ivy"io.github.mcp4s::mcp4s-server::0.1.4",  // for servers
  ivy"io.github.mcp4s::mcp4s-client::0.1.4",  // for clients
  ivy"io.github.mcp4s::mcp4s-agent::0.1.4"    // for agents
)
```

Or in sbt:

```scala
libraryDependencies ++= Seq(
  "io.github.mcp4s" %% "mcp4s-core" % "0.1.4",
  "io.github.mcp4s" %% "mcp4s-server" % "0.1.4", // for servers
  "io.github.mcp4s" %% "mcp4s-client" % "0.1.4", // for clients
  "io.github.mcp4s" %% "mcp4s-agent" % "0.1.4"   // for agents
)
```

## Quick Start

### Creating an MCP Server

```scala
import cats.effect.{IO, IOApp}
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.transport.*

object MyServer extends IOApp.Simple:

  val server: Server[IO] = Server.builder[IO]
    .withInfo(ServerInfo("my-server", "1.0.0"))
    .withTool(
      Tool(
        name = "greet",
        description = Some("Greet someone"),
        inputSchema = JsonSchema.obj(
          properties = Map("name" -> JsonSchema.string(Some("Name to greet"))),
          required = List("name")
        )
      ),
      args => {
        val name = args.hcursor.get[String]("name").getOrElse("World")
        IO.pure(ToolResult.text(s"Hello, $name!"))
      }
    )
    .build

  def run: IO[Unit] =
    HttpTransport.serve[IO](server).useForever
```

### Unified DSL (Alternative)

For a more concise API, use the unified DSL:

```scala
import mcp4s.server.mcp.*
import cats.syntax.semigroup.*

val tools = Tool.text[IO]("greet", "Greet someone") { args =>
  val name = args.hcursor.get[String]("name").getOrElse("World")
  ok(s"Hello, $name!").pure[IO]
}

val resources = Resource.text[IO]("info", "info://app", "Application info") {
  ok("MCP4S Server v1.0").pure[IO]
}

// Combine multiple tools/resources with |+|
val allTools = tools |+| moreTools

val server = Server.from[IO](
  info = ServerInfo("my-server", "1.0.0"),
  tools = allTools,
  resources = resources
)
```

### Creating an MCP Client

```scala
import cats.effect.{IO, IOApp}
import io.circe.Json
import mcp4s.client.*
import mcp4s.client.transport.*
import mcp4s.protocol.*

object MyClient extends IOApp.Simple:

  val client: McpClient[IO] = McpClient.builder[IO]
    .withInfo(ClientInfo("my-client", "1.0.0"))
    .build

  def run: IO[Unit] =
    HttpClientTransport.connect[IO](
      client,
      HttpClientConfig("http://localhost:3000")
    ).use { conn =>
      for
        tools <- conn.listTools
        _ <- IO.println(s"Available tools: ${tools.map(_.name).mkString(", ")}")
        result <- conn.callTool("greet", Json.obj("name" -> Json.fromString("MCP")))
        _ <- IO.println(s"Result: $result")
      yield ()
    }
```

### Stdio Transport (for Claude Desktop)

```scala
import cats.effect.{IO, IOApp}
import mcp4s.server.transport.*

object StdioServer extends IOApp.Simple:
  def run: IO[Unit] = StdioTransport.run[IO](MyServer.server)
```

### Configuring for Claude Code

To use an MCP server with Claude Code, create a `.mcp.json` file in your project root:

```json
{
  "mcpServers": {
    "calculator": {
      "command": "java",
      "args": [
        "-cp",
        "out/examples/assembly.dest/out.jar",
        "mcp4s.examples.CalculatorStdio"
      ]
    }
  }
}
```

First build the assembly jar:

```bash
mill examples.assembly
```

Then restart Claude Code to pick up the new server configuration.

### Authentication

MCP4S supports OAuth 2.0 bearer token authentication:

```scala
import mcp4s.server.auth.*

val authConfig = AuthConfig[IO](
  metadata = ProtectedResourceMetadata(
    resource = "http://localhost:3000",
    authorizationServers = List("https://auth.example.com"),
    scopesSupported = Some(List("mcp:read", "mcp:write"))
  ),
  validator = TokenValidator.jwt[IO],  // or .apiKey, .allowAll
  requiredScopes = Some(Set("mcp:read"))
)

HttpTransport.serve[IO](server, auth = Some(authConfig)).useForever
```

Available validators:
- `TokenValidator.jwt` - Parse JWT tokens (dev mode, no signature verification)
- `TokenValidator.apiKey(keys)` - Validate against a set of API keys
- `TokenValidator.allowAll` - Accept any token (dev only)

### Middleware

Add cross-cutting concerns like logging, metrics, and error handling to tools:

```scala
import mcp4s.server.*

val logging = Middleware.logging[IO](msg => IO.println(msg))
val timed = Middleware.timed[IO] { (name, duration) =>
  IO.println(s"Tool $name took ${duration.toMillis}ms")
}
val catchErrors = Middleware.catchErrors[IO]  // Convert exceptions to error results

val tools = (add |+| subtract).withMiddleware(logging, timed, catchErrors)
```

### Simpler Server Startup

Use extension methods for common server configurations:

```scala
import mcp4s.server.syntax.*

// Run on stdio (for Claude Desktop)
server.runStdio

// Run on HTTP with defaults (port 3000)
server.serveHttp.useForever

// Run on HTTP with custom port
server.serveHttp(port"8080").useForever

// Without explicit Tracer (uses noop)
server.serveHttpNoTrace.useForever
```

### Auto-Error Handling Tools

Create tools that automatically convert exceptions to error results:

```scala
// Exceptions become ToolResult.error instead of failing
val fetch = McpTool.attempt[IO, FetchArgs]("fetch", "Fetch URL") { args =>
  httpClient.get(args.url).map(_.body)  // F[String]
}

// With custom error formatting
val query = McpTool.attemptWith[IO, QueryArgs]("query", "Run query") { args =>
  db.execute(args.sql).map(_.toString)
} {
  case e: SQLException => s"Database error: ${e.getMessage}"
  case e => s"Unexpected: ${e.getMessage}"
}
```

### Testing Utilities

Test MCP servers and tools without network overhead:

```scala
import mcp4s.server.testing.*

class MyServerSpec extends CatsEffectSuite:

  test("tool calls work") {
    val tools = McpTool.twoNumbers[IO]("add", "Add") { (a, b) =>
      IO.pure(ToolResult.text(s"${a + b}"))
    }

    for
      result <- tools.testCall("add", args("a" -> 2.0, "b" -> 3.0))
      _ = assertEquals(result.textContent, "5.0")
    yield ()
  }

  test("server integration") {
    ServerTest(server).use { client =>
      for
        tools <- client.listTools
        result <- client.callTool("add", AddArgs(1, 2))
        _ = assertEquals(result.textContent, "3.0")
      yield ()
    }
  }
```

## Agent Module

The agent module provides an autonomous tool-calling loop that connects any LLM to MCP tools. It extends `McpClient`, so an agent is itself a first-class MCP client capable of handling server-initiated requests (sampling, elicitation, roots).

### Builder API

```scala
import cats.effect.IO
import mcp4s.agent.*

val agent: IO[Agent[IO]] = Agent.builder[IO](llmClient, mcpConnection)
  .withConfig(LlmConfig.default.withModel("gpt-4").withMaxTurns(20))
  .withDefaultSampling
  .withChainOfThought(ChainOfThoughtConfig(thinkBeforeEveryTurn = true))
  .withReflection(ReflectionConfig(reflectEveryNTurns = 3))
  .withContextWindow(TokenBudget(8000, 1000), _ => ContextPolicy.keepSystemAndRecent)
  .build
```

### LlmClient Interface

Implement the `LlmClient[F]` trait to adapt your LLM provider:

```scala
trait LlmClient[F[_]]:
  def complete(request: LlmRequest): F[LlmResponse]
  def stream(request: LlmRequest)(using Concurrent[F]): fs2.Stream[F, LlmResponseChunk]
```

`complete` returns a full response. `stream` returns incremental chunks and has a default implementation that wraps `complete`. Responses carry optional `stopReason` and `usage` metadata:

```scala
val response = LlmResponse.Text("Hello!", stopReason = Some("endTurn"), usage = Some(Usage(promptTokens = Some(100))))
```

### Hooks

Hooks inject behavior before and after each tool-calling turn:

- **Chain-of-thought** (`withChainOfThought`) - forces explicit LLM reasoning before tool calls
- **Reflection** (`withReflection`) - periodic self-reflection after every N turns
- **Context management** (`withContextWindow`) - compresses messages when they exceed a token budget

Hooks compose via `Semigroup` (the `|+|` operator), so multiple hooks can be combined freely.

### Running the Agent

```scala
agent.run("What is 2 + 3?")
  .evalTap {
    case AgentEvent.ToolCalled(call)    => IO.println(s"Calling ${call.name}")
    case AgentEvent.Finished(content)   => IO.println(s"Done: $content")
    case _                              => IO.unit
  }
  .compile
  .drain
```

### Agent as Server

An agent can also **be** an MCP server, exposing tools/resources/prompts that other agents or clients can call. Server-side tools registered via `withAgentTools` receive an `AgentContext` with access to the agent's LLM, configuration, and shared conversation state.

```scala
import mcp4s.agent.*
import mcp4s.server.mcp.*

val agent = Agent.builder[IO](llmClient, mcpConnection)
  // Expose the agent loop as a callable tool
  .asTool("ask", "Ask the agent a question")
  // Plain server-side tools (no agent context)
  .withServerTools(Tool.text[IO]("status", "Get status") { "running" })
  // Tools with access to the agent's internals
  .withAgentTools { ctx =>
    Tool[IO]("chat", "Chat with the agent") {
      ctx.llmClient.complete(LlmRequest(List(Message.User("hello")), Nil, ctx.config))
        .map { case LlmResponse.Text(text, _, _) => ok(text) }
    }
  }
  .build

// Create an MCP Server from the agent
val server: IO[Server[IO]] = agent.flatMap(_.toServer)
```

### Agent Examples

```bash
mill examples.runMain mcp4s.examples.AgentQuickStart
mill examples.runMain mcp4s.examples.AgentHooksDemo
mill examples.runMain mcp4s.examples.AgentAsServer
```

## Building

```bash
mill __.compile       # Compile all modules
mill __.publishLocal  # Publish locally
```

## Testing

### Unit Tests

Unit tests use munit-cats-effect for async testing and munit-scalacheck for property-based testing.

```bash
mill __.test          # Run all unit tests
mill unitTests        # Run core, server, client tests
mill core.test        # Run tests for a single module
mill core.test.testOnly mcp4s.protocol.PropertySpec  # Run a specific test class
```

### Conformance Tests

MCP conformance tests verify protocol compliance using the [official MCP Conformance Test Framework](https://github.com/modelcontextprotocol/conformance). These tests validate that mcp4s correctly implements the MCP specification.

#### Prerequisites

- Node.js 18+
- Install conformance test dependencies (first time only):
  ```bash
  cd conformance && npm install && cd ..
  ```

#### Running Conformance Tests

First, start the conformance test server (implements all required test tools, resources, and prompts):

```bash
mill examples.runMain mcp4s.examples.ConformanceServer
```

Then run the conformance tests:

```bash
# Run active test scenarios (recommended for CI)
mill conformance

# Run a specific scenario
mill conformance --scenario server-initialize
mill conformance --scenario tools-list
mill conformance --scenario tools-call-simple-text

# Run all scenarios (including pending/experimental)
mill conformance --suite all

# Test against a different server URL
mill conformance --url http://localhost:8080/mcp

# Show detailed output
mill conformance --verbose
```

#### Available Scenarios

List all available test scenarios:

```bash
mill conformanceList
```

Key scenarios include:

| Category | Scenarios |
|----------|-----------|
| Lifecycle | `server-initialize`, `ping` |
| Tools | `tools-list`, `tools-call-simple-text`, `tools-call-image`, `tools-call-error` |
| Resources | `resources-list`, `resources-read-text`, `resources-read-binary` |
| Prompts | `prompts-list`, `prompts-get-simple`, `prompts-get-with-args` |
| Logging | `logging-set-level` |

## Running Examples

```bash
# Start the calculator server
mill examples.runMain mcp4s.examples.CalculatorServer

# In another terminal, run the client
mill examples.runMain mcp4s.examples.CalculatorClient
```

## License

Apache 2.0 - see [LICENSE](LICENSE) for details.

## Links

- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [MCP Documentation](https://modelcontextprotocol.io/)
