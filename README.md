# MCP4S

A Scala implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) for the Typelevel ecosystem.

## Overview

MCP4S provides a type-safe, functional implementation of MCP for Scala 3 using cats-effect, fs2, http4s, and circe. It enables Scala applications to act as MCP servers (providing tools, resources, and prompts) or clients (consuming MCP servers).

## Modules

- **core** - Protocol types, JSON-RPC messages, and codec definitions
- **server** - MCP server implementation with Streamable HTTP and stdio transports
- **client** - MCP client for connecting to MCP servers
- **postgres** - PostgreSQL MCP server for database access

## Installation

Add to your `build.mill`:

```scala
def ivyDeps = Agg(
  ivy"io.github.mcp4s::mcp4s-core::0.1.1",
  ivy"io.github.mcp4s::mcp4s-server::0.1.1",  // for servers
  ivy"io.github.mcp4s::mcp4s-client::0.1.1"   // for clients
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

  val server: McpServer[IO] = McpServer.builder[IO]
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
  requiredScopes = Set("mcp:read")
)

HttpTransport.serve[IO](server, auth = Some(authConfig)).useForever
```

Available validators:
- `TokenValidator.jwt` - Parse JWT tokens (dev mode, no signature verification)
- `TokenValidator.apiKey(keys)` - Validate against a set of API keys
- `TokenValidator.allowAll` - Accept any token (dev only)

### PostgreSQL Server

Run an MCP server that provides SQL query access to PostgreSQL:

```bash
# Set connection environment variables
export POSTGRES_HOST=localhost
export POSTGRES_PORT=5432
export POSTGRES_USER=myuser
export POSTGRES_PASSWORD=mypassword
export POSTGRES_DATABASE=mydb

# Run via stdio
mill postgres.runMain mcp4s.postgres.PostgresStdio
```

Provides:
- `query` tool - Execute read-only SQL queries
- `schema://public` resource - Database schema information

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
mill unitTests        # Run core, server, client, postgres tests
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

#### Expected Failures

The `conformance-baseline.yml` file tracks scenarios that don't yet pass. This allows CI to pass while still catching regressions. When you fix a scenario, remove it from the baseline.

```bash
# CI will fail if:
# - A scenario fails that's NOT in the baseline (regression)
# - A scenario passes that IS in the baseline (stale entry)
```

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
