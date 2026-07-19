# Prompts

MCP **prompts** are reusable message templates that clients can discover and use. While tools let AI *do* things and resources let AI *read* things, prompts give AI pre-built *workflows* — a code review template, an analysis checklist, a structured debugging flow.

> For the full protocol details, see [Prompts](https://spec.modelcontextprotocol.io/specification/2025-03-26/server/prompts/) in the MCP specification.

Prompts return a list of messages (user/assistant turns) that the client inserts into the AI's conversation. They can be static or take arguments for customization.

## Defining Prompts

Like tools, a prompt is an **endpoint definition** (`PromptEndpoint`) plus a handler:

```scala
import mcp4s.server.dsl.*

// Static — always returns the same messages
Prompt("help")
  .withDescription("Get help")
  .messages[IO](
    user("How can I help?"),
    assistant("I can assist with calculations.")
  )

// Static with a described result
Prompt("intro")
  .withDescription("Introduction")
  .static[IO](messages("A short intro")(user("Hello!")))

// With arguments — customizable via parameters
case class GreetArgs(name: String, excitement: Int = 1) derives Schema

Prompt("greet")
  .withDescription("Greet someone")
  .input[GreetArgs]
  .handle[IO](args => IO.pure(messages(user(s"Hello, ${args.name}${"!" * args.excitement}"))))
```

`Prompt.from[GreetArgs]` derives the name and description from the input type, exactly like `Tool.from`:

```scala
@description("Greet someone")
case class GreetArgs(name: String) derives Schema

Prompt.from[GreetArgs].handle[IO](args => IO.pure(messages(user(s"Hello, ${args.name}!"))))
```

Clients call `getPrompt("greet", {"name": "Alice"})` and receive the rendered messages to inject into the conversation.

## Argument Types

Prompt arguments arrive as strings on the wire, but the schema parses them into your field types: `String` passes through; `Int`, `Double`, and `Boolean` parse from their literal form; Scala 3 enums parse from the case name; `Option` and constructor defaults make fields optional. See [Type Derivation](../reference/type-derivation.md).

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
val server = McpServer[IO](ServerInfo("my-server", "1.0.0")).withPrompts(prompts)
```

A server with prompts registered advertises the `prompts` capability automatically.

---
**Next:** [Services](services.md)
