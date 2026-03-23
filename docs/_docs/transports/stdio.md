# Stdio Transport

For Claude Desktop integration.

## Server

```scala
object MyServer extends IOApp.Simple:
  val server = McpServer.builder[IO]
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

- No bidirectional communication (no sampling/elicitation)
- Single client
- Process exits when stdin closes
