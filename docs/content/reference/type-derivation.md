# Type Derivation

MCP4S describes a Scala type **once** with `Schema[A]`, then derives everything from it: the JSON Schema advertised to clients, the circe encoder/decoder used on the wire, and prompt-argument metadata. Because all of these are interpreters over the same value, they can never disagree with each other.

## Schema

Derive a schema with `derives Schema` (available via `import mcp4s.server.dsl.*` or directly from `mcp4s.schema`):

```scala
import mcp4s.server.dsl.*

@description("Search documents")
case class SearchArgs(
  @description("Search query") query: String,
  @description("Max results to return") limit: Option[Int]
) derives Schema
```

This single derivation gives you:
- A JSON schema with `query` (required string) and `limit` (optional integer)
- A circe `Encoder[SearchArgs]` and `Decoder[SearchArgs]`
- Prompt-argument metadata (when used as a prompt input)
- Field and class descriptions in the schema

Use it with the endpoint DSL — with `Tool.from`, the name and description derive automatically:

```scala
Tool.from[SearchArgs].handle[IO](args => search(args.query, args.limit.getOrElse(10)).map(ok(_)))
```

> `ToolInput` / `ToolOutput` / `PromptInput` still exist as thin views over `Schema`, but you should never need them directly — teach your types `derives Schema` and everything else follows.

## What Derives

Derivation recurses automatically — nested types do **not** need their own `derives Schema` clause:

```scala
enum Unit derives Schema:
  case Celsius, Fahrenheit

case class Coordinates(lat: Double, lon: Double)   // nested: derived automatically

@description("Get a weather forecast")
case class Forecast(
  @description("City to look up") city: String,
  where: Coordinates,
  days: Int = 3,
  unit: Unit = Unit.Celsius,
  tags: Map[String, String] = Map.empty
) derives Schema
```

| Scala construct | JSON Schema rendering |
|-----------------|----------------------|
| Case class | `object` with typed `properties` (nested classes render as full nested schemas) |
| Scala 3 enum (no payloads) | string `enum` |
| Sealed trait with payloads | `oneOf` with a `"type"` discriminator field naming the case |
| `Option[T]` | type of `T`, field not required; `None` omitted when encoding |
| Constructor default | field not required, `default` in the schema; missing fields decode to the default |
| `List` / `Vector` / `Seq` / `Set` | `array` with `items` schema |
| `Map[String, V]` | `object` with `additionalProperties` |
| `String` | `"string"` |
| `Int` / `Long` | `"integer"` |
| `Double` / `Float` | `"number"` |
| `Boolean` | `"boolean"` |
| `io.circe.Json` | opaque `"object"` |

### Sealed traits

Payload-carrying hierarchies render as `oneOf`, discriminated by a `"type"` field:

```scala
sealed trait Shape derives Schema
case class Circle(radius: Double) extends Shape
case class Rect(w: Double, h: Double) extends Shape

// on the wire: {"type": "Circle", "radius": 2.0}
```

### Recursive types

Break cycles with `Schema.defer`:

```scala
case class Tree(value: Int, children: List[Tree])

given Schema[Tree] = Schema.defer(Schema.derived[Tree])
```

Recursive occurrences render as an untyped `object` in the JSON Schema but encode/decode fully.

### Wrapper types

For newtypes and other isomorphic wrappers, use `Schema.bijection`:

```scala
case class UserId(value: String)
given Schema[UserId] = Schema.bijection[String, UserId](UserId(_), _.value)
```

## Typed Tool Output

Declare a tool's output with `.output[B]`; the schema is advertised as `outputSchema` and results are encoded as `structuredContent`:

```scala
case class CalcResult(
  @description("The computed value") result: Double,
  @description("The operation") operation: String
) derives Schema

Tool("calculate").input[CalcArgs].output[CalcResult]
  .handle[IO](args => IO.pure(CalcResult(args.a + args.b, "add")))
```

Primitive outputs (`String`, `Double`, ...) are wrapped as `{"result": ...}` on the wire; the typed client unwraps them again.

## Prompt Inputs

The same schema drives prompt arguments. Prompt arguments arrive as `Map[String, String]` on the wire, so each field is parsed from its string form according to its schema:

- `String` passes through
- `Int`, `Long`, `Double`, `Float`, `Boolean` parse from their literal form (`"3"`, `"true"`)
- Scala 3 enums parse from the case name (`"Celsius"`)
- `Option[T]` and fields with defaults are not required
- Structured fields (lists, nested objects) parse from a JSON string

```scala
@description("Greet someone")
case class GreetArgs(
  @description("Name to greet") name: String,
  @description("How excited") excitement: Int = 1
) derives Schema

Prompt.from[GreetArgs].handle[IO] { args =>
  IO.pure(messages(user(s"Hello, ${args.name}${"!" * args.excitement}")))
}
```

## @description Annotation

The `@description` annotation (from `mcp4s.protocol`, exported by the dsl) adds human-readable documentation to schema properties and to tool/prompt definitions:

```scala
// Class-level — becomes the tool/prompt description
@description("Add two numbers")
case class AddArgs(
  @description("First number") a: Double,
  @description("Second number") b: Double
) derives Schema
```

The annotation is extracted at compile time via macros. Fields without `@description` get no description in the schema.

## Name Derivation

`Tool.from[A]` / `Prompt.from[A]` derive the endpoint name from the class name:

1. Common suffixes are stripped: `Args`, `Input`, `Params`, `Request`
2. CamelCase is converted to snake_case: `BatchAdd` → `batch_add`

Examples: `AddArgs` → `"add"`, `SmartCalcArgs` → `"smart_calc"`, `SearchInput` → `"search"`

Use an explicit endpoint when the derived name doesn't match: `Tool("custom-name").input[Args]`.

## Tips

- Always add `@description` to fields — it helps LLMs understand your tool's interface
- Add class-level `@description` for automatic tool/prompt descriptions with `Tool.from` / `Prompt.from`
- Use `Option[T]` or a constructor default for fields that should be optional in the schema
- If a field's type has no `Schema` and no `Mirror`, you'll get a compile error — add `derives Schema` to it or provide a `given Schema[...]`
