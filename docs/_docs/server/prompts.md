# Prompts

## Constructors

```scala
import mcp4s.server.mcp.*
import mcp4s.protocol.PromptInput

// Static
Prompt[IO]("help", "Get help")(
  user("How can I help?"),
  assistant("I can assist with calculations.")
)

// With arguments
case class GreetArgs(name: String) derives PromptInput

Prompt[IO, GreetArgs]("greet", "Greet someone") { args =>
  IO.pure(messages(user(s"Hello, ${args.name}!")))
}
```

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

## With Builder

```scala
Server.builder[IO]
  .prompt("help", "Help")(user("How can I help?"))
  .prompt[GreetArgs]("greet", "Greet") { args =>
    IO.pure(messages(user(s"Hello, ${args.name}!")))
  }
  .build
```
