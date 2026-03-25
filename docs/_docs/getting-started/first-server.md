# Build Your First Server

## Define Tools

```scala
import cats.effect.*
import mcp4s.server.mcp.*
import mcp4s.protocol.ToolInput

case class CalcArgs(a: Double, b: Double) derives ToolInput

val tools =
  Tool.text[IO, CalcArgs]("add", "Add two numbers") { args =>
    s"${args.a + args.b}"
  } |+|
  Tool.text[IO, CalcArgs]("multiply", "Multiply two numbers") { args =>
    s"${args.a * args.b}"
  }
```

## Add Resources

```scala
val resources =
  Resource.text[IO]("file:///readme", "README") {
    "Calculator Server v1.0"
  } |+|
  Resource.template[IO]("api://users/{id}", "User", "Get user by ID") { uri =>
    val id = uri.split("/").last
    IO.pure(mcp.text(uri, s"""{"id":"$id"}"""))
  }
```

## Define Prompts

```scala
import mcp4s.protocol.PromptInput

case class GreetArgs(name: String) derives PromptInput

val prompts =
  Prompt[IO]("help", "Get help")(user("How do I use this?")) |+|
  Prompt[IO, GreetArgs]("greet", "Greet someone") { args =>
    IO.pure(messages(user(s"Hello, ${args.name}!")))
  }
```

## Build and Run

```scala
val server = Server.from[IO](
  info = ServerInfo("calculator", "1.0.0"),
  tools = tools,
  resources = resources,
  prompts = prompts
)

// HTTP (production)
server.serveHttp(3000)

// Stdio (Claude Desktop)
server.runStdio

// WebSocket
WebSocketTransport.serve[IO](server, WebSocketConfig(port = 3000))
```

## Builder Alternative

```scala
val server = Server.builder[IO]
  .withInfo(ServerInfo("calculator", "1.0.0"))
  .tool[CalcArgs]("add", "Add") { args => IO.pure(ok(s"${args.a + args.b}")) }
  .resource("file:///readme", "README") { "Calculator v1.0" }
  .prompt("help", "Help")(user("How can I help?"))
  .build
```

## Test with MCP Inspector

```bash
npx @modelcontextprotocol/inspector --transport http --server-url http://localhost:3000/mcp
```
