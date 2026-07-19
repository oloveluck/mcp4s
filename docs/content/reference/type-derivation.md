# Type Derivation

MCP4S derives JSON schemas, decoders, and argument lists from case classes at compile time. Annotate fields with `@description` to generate documentation.

## ToolInput

Derive tool argument schemas and decoders:

```scala
import mcp4s.protocol.*

@description("Search documents")
case class SearchArgs(
  @description("Search query") query: String,
  @description("Max results to return") limit: Option[Int]
) derives ToolInput
```

This generates:
- A JSON schema with `query` (required string) and `limit` (optional integer)
- A Circe `Decoder[SearchArgs]` for parsing tool call arguments
- Field descriptions in the schema's `properties`

Use with the server DSL — the name and description are derived automatically:

```scala
Tool[IO, SearchArgs](args => search(args.query, args.limit.getOrElse(10)).map(ok(_)))
```

The tool name is derived from the class name (`SearchArgs` → `"search"`) and the description from the class-level `@description` annotation.

## ToolOutput

Derive structured output encoding for tool results:

```scala
import mcp4s.protocol.ToolOutput
import io.circe.Encoder

case class CalcResult(
  @description("The computed value") result: Double,
  @description("The operation") operation: String
) derives ToolOutput, Encoder.AsObject
```

This generates:
- A JSON schema for the output type
- An encoder that produces `ToolResult` with `structuredContent`

Primitive instances are provided for `String`, `Int`, `Long`, `Double`, `Boolean`, and `Json`.

## PromptInput

Derive prompt arguments and decoders:

```scala
import mcp4s.protocol.*

@description("Greet someone")
case class GreetArgs(
  @description("Name to greet") name: String,
  @description("Greeting style") style: Option[String]
) derives PromptInput
```

This generates:
- A `List[PromptArgument]` with required/optional flags
- A decoder from `Map[String, String]` to the case class

`Option[String]` fields are marked as `required: false`. All other fields are `required: true`.

Use with the server DSL — name and description derived from the class:

```scala
Prompt[IO, GreetArgs](args => IO.pure(messages(user(s"Hello, ${args.name}!"))))
```

## Supported Types

| Scala Type | JSON Schema Type | Notes |
|------------|-----------------|-------|
| `String` | `"string"` | |
| `Int` | `"integer"` | |
| `Long` | `"integer"` | |
| `Double` | `"number"` | |
| `Float` | `"number"` | |
| `Boolean` | `"boolean"` | |
| `Option[T]` | Type of `T` | Field becomes optional |
| `List[T]` / `Seq[T]` | `"array"` | With `items` schema |
| `Map[K, V]` | `"object"` | |

## @description Annotation

The `@description` annotation adds human-readable documentation to schema properties and to tool/prompt definitions:

```scala
import mcp4s.protocol.*

// Class-level — becomes the tool/prompt description
@description("Add two numbers")
case class AddArgs(
  @description("First number") a: Double,
  @description("Second number") b: Double
) derives ToolInput
```

The annotation is extracted at compile time via macros. Fields without `@description` get no description in the schema. Classes without `@description` get an empty description.

## Name Derivation

When using the derived-name API (e.g. `Tool[IO, AddArgs] { ... }`), the tool name is derived from the class name:

1. Common suffixes are stripped: `Args`, `Input`, `Params`, `Request`
2. CamelCase is converted to snake_case: `BatchAdd` → `batch_add`

Examples: `AddArgs` → `"add"`, `SmartCalcArgs` → `"smart_calc"`, `SearchInput` → `"search"`

Use the name-override variant when the derived name doesn't match: `Tool[IO, Args]("custom-name") { ... }`

## Tips

- Always add `@description` to fields — it helps LLMs understand your tool's interface
- Add class-level `@description` for automatic tool/prompt descriptions
- Use `Option[T]` for fields that should be optional in the schema
- `ToolOutput` requires a Circe `Encoder.AsObject` instance (derive it alongside)
- `PromptInput` only supports `String` and `Option[String]` fields since MCP prompts use string arguments
