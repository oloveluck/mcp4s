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
mill __.compile     # Compile all modules
mill __.test        # Run all tests
mill __.publishLocal  # Publish locally
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
