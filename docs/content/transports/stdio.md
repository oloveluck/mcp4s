# Stdio Transport

The Stdio transport communicates over standard input/output — the client spawns the server as a subprocess. This is the transport used by **Claude Desktop** and other desktop AI clients that manage server processes locally.

> For the full protocol details, see [Stdio Transport](https://spec.modelcontextprotocol.io/specification/2025-03-26/transport/stdio/) in the MCP specification.

No networking is involved. The client writes JSON-RPC to the server's stdin and reads responses from stdout.

## Server

```scala
import cats.effect.*
import mcp4s.server.*
import mcp4s.server.dsl.*
import mcp4s.protocol.*

object MyServer extends IOApp.Simple:
  val tools = Tool("search").withDescription("Search files").input[Args]
    .handle[IO](args => ...)

  val server = McpServer[IO](ServerInfo("my-server", "1.0.0")).withTools(tools)

  def run = server.stdio.run
```

`stdio.run` runs until stdin closes.

## Client

To drive a server you spawn yourself as a subprocess (cross-platform, no import needed):

```scala
client.stdio("java", "-jar", "/path/to/server.jar").use: conn =>
  conn.callTool("search", args)
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
)).use(conn => ...)
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
