# Stdio Transport

The Stdio transport communicates over standard input/output — the client spawns the server as a subprocess. This is the transport used by **Claude Desktop** and other desktop AI clients that manage server processes locally.

> For the full protocol details, see [Stdio Transport](https://spec.modelcontextprotocol.io/specification/2025-03-26/transport/stdio/) in the MCP specification.

No networking is involved. The client writes JSON-RPC to the server's stdin and reads responses from stdout.

## Server

```scala
object MyServer extends IOApp.Simple:
  val server = Server.builder[IO]
    .withInfo(ServerInfo("my-server", "1.0.0"))
    .tool[Args]("search", "Search files") { args => ... }
    .build

  def run: IO[Unit] = server.runStdio
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

With Mill:
```json
{
  "mcpServers": {
    "my-server": {
      "command": "/path/to/project/mill",
      "args": ["myserver.run"],
      "cwd": "/path/to/project"
    }
  }
}
```

## Limitations

- No bidirectional communication (no sampling/elicitation) — the server can't initiate requests back to the client
- Single client only — one process, one connection
- Process exits when stdin closes
