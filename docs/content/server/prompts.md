# Prompts

MCP **prompts** are reusable message templates that clients can discover and use. While tools let AI *do* things and resources let AI *read* things, prompts give AI pre-built *workflows* — a code review template, an analysis checklist, a structured debugging flow.

> For the full protocol details, see [Prompts](https://spec.modelcontextprotocol.io/specification/2025-03-26/server/prompts/) in the MCP specification.

Prompts return a list of messages (user/assistant turns) that the client inserts into the AI's conversation. They can be static or take arguments for customization.

## Constructors

```scala
import mcp4s.server.mcp.*
import mcp4s.protocol.PromptInput

// Static — always returns the same messages
Prompt[IO]("help", "Get help")(
  user("How can I help?"),
  assistant("I can assist with calculations.")
)

// With arguments — customizable via parameters
case class GreetArgs(name: String) derives PromptInput

Prompt[IO, GreetArgs]("greet", "Greet someone")(args =>
  IO.pure(messages(user(s"Hello, ${args.name}!")))
)
```

Clients call `getPrompt("greet", {"name": "Alice"})` and receive the rendered messages to inject into the conversation.

## Message Builders

```scala
user("text")
assistant("text")
messages(user("Hello"), assistant("Hi"))
messages("Description")(user("Hello"))
```

## Composition

```scala
val prompts = helpPrompt |+| greetPrompt |+| tutorialPrompt
```

## Register with a Server

```scala
Server.from[IO](
  ServerInfo("my-server", "1.0.0"),
  Tools.empty[IO],
  Resources.empty[IO],
  prompts
)
```

---
**Next:** [HTTP Security](auth.md)
