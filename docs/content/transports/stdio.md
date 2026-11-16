# Stdio Transport

The Stdio transport communicates over standard input/output — the client spawns the server as a subprocess. This is the transport used by **Claude Desktop** and other desktop AI clients that manage server processes locally.

> For the full protocol details, see [Stdio Transport](https://modelcontextprotocol.io/specification/2025-11-25/basic/transports#stdio) in the MCP specification.

No networking is involved. The client writes JSON-RPC to the server's stdin and reads responses from stdout.

## Server

```scala
import cats.effect.*
import mcp4s.protocol.ServerInfo
import mcp4s.server.*
import mcp4s.server.dsl.*

case class Args(query: String) derives Schema

object MyServer extends IOApp.Simple:
  val tools = Tool("search").withDescription("Search files").input[Args]
    .handle[IO](args => IO.pure(ok(s"Results for: ${args.query}")))

  val server = McpServer[IO](ServerInfo("my-server", "1.0.0")).withTools(tools)

  def run = server.stdio.run
```

`stdio.run` runs until stdin closes. The transport is fully bidirectional: context-aware tools
(`handleWith`) can report progress, log, sample the client's model and elicit input over the same
pipe, and the client's answers are correlated back to the waiting tool. Requests are dispatched
concurrently so a `notifications/cancelled` line can interrupt a running call.

To run the server over something other than the process's stdin/stdout — a socket, or in-memory
pipes in a test — use `StdioTransport.runWith` with your own byte streams:

```scala
import mcp4s.server.transport.{StdioConfig, StdioTransport}
import scala.concurrent.duration.*

def input: Stream[IO, Byte]              = ???   // newline-delimited JSON-RPC from the client
def output: fs2.Pipe[IO, Byte, Unit]     = ???   // where the server's lines go
given Tracer[IO]                         = Tracer.noop[IO]

StdioTransport.runWith[IO](server, input, output, StdioConfig(requestTimeout = 1.minute))
```

## Client

To drive a server you spawn yourself as a subprocess (cross-platform, no import needed):

```scala
import io.circe.Json
import mcp4s.client.McpClientBuilder
import mcp4s.protocol.ClientInfo

val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))

client.stdio("java", "-jar", "/path/to/server.jar").use: conn =>
  conn.callTool("search", Json.obj("query" -> Json.fromString("readme")))
```

For working directory, environment, or timeouts, pass a full config:

```scala
import mcp4s.client.transport.StdioTransportConfig
import mcp4s.transport.Timeouts
import scala.concurrent.duration.*

client.stdio(StdioTransportConfig(
  command          = "node",
  args             = List("server.js"),
  workingDirectory = Some("/srv/mcp"),
  env              = Map("LOG_LEVEL" -> "debug"),
  timeouts         = Timeouts(request = 1.minute, init = 15.seconds)
)).use(conn => conn.listAllTools)
```

## Claude Desktop Config

**macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`

```json
{
  "mcpServers": {
    "my-server": {
      "command": "java",
      "args": ["-jar", "/path/to/server.jar"]
    }
  }
}
```

## Limitations

- Plain request/response only — no server-initiated requests back to the client (no sampling/elicitation); use HTTP (SSE) or WebSocket for bidirectional features
- Single client only — one process, one connection
- Process exits when stdin closes
