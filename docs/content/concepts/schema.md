# Schema

Every argument a model passes to a tool crosses three boundaries: the JSON Schema that tells the
model what to send, the decoder that validates what arrived, and (for prompts) the argument list
in `prompts/list`. Most libraries describe these separately and let them drift. mcp4s describes
the type once, as a `Schema[A]`, and derives the rest.

```
Schema[A] ─┬─→ JSON Schema       (what tools/list advertises)
           ├─→ Encoder / Decoder (what moves on the wire)
           └─→ prompt arguments  (names, descriptions, required flags)
```

`Schema[A]` is a small reified ADT (struct, enumeration, union, collection, map, optional,
primitive). Each output above is an interpreter over that value, so the advertised schema can
never disagree with the decoder that enforces it. The idea is borrowed from smithy4s.

## Deriving one

```scala
import mcp4s.server.dsl.*

enum TempUnit derives Schema:
  case Celsius, Fahrenheit

case class Coordinates(lat: Double, lon: Double)   // nested types derive automatically

@description("Get a weather forecast")
case class ForecastArgs(
  @description("City to look up") city: String,
  where: Option[Coordinates],
  days: Int = 3,
  unit: TempUnit = TempUnit.Celsius
) derives Schema
```

`derives Schema` handles case classes, Scala 3 enums (as string enums), sealed hierarchies with
payloads (as `oneOf` with a `type` discriminator), `Option`, constructor defaults, the standard
collections and `Map[String, V]`. Nested types do not need their own `derives` clause. The
`@description` annotation is read at compile time: on the class it becomes the tool or prompt
description, on a field it documents the property. Write them; they are what the model reads.

Two rules decide whether a field is required: `Option` fields and fields with a default are not.
A default is also rendered into the schema and applied when the field is missing.

Recursive types break the cycle with `Schema.defer`; wrapper types map through an existing
schema with `Schema.bijection`. A field whose type has neither a `Schema` nor a `Mirror` is a
compile error, not a runtime surprise.

## Where it is used

- **Tool input**: `Tool.from[ForecastArgs]` or `Tool("forecast").input[ForecastArgs]`. The
  schema is advertised as `inputSchema`; the handler receives a decoded `ForecastArgs`, and a
  malformed call fails with `InvalidToolArguments` before your code runs.
- **Tool output**: `.output[B]` advertises `outputSchema` and encodes the handler's `B` as
  `structuredContent`. Primitive outputs are wrapped as `{"result": …}` because the spec requires
  an object. The typed client decodes it back.
- **Prompt input**: prompt arguments arrive as strings on the wire, so the same schema parses
  each field from its string form (numbers, booleans and enums from their literals). Enum fields
  also drive argument completion: `completion/complete` suggests their values.

The older `ToolInput`, `ToolOutput` and `PromptInput` type classes still exist as thin views over
`Schema` for compatibility; new code should not need them.

## Names

`Tool.from[A]` and `Prompt.from[A]` derive the endpoint name from the class name: the suffixes
`Args`, `Input`, `Params` and `Request` are dropped and the rest becomes snake_case, so
`SmartCalcArgs` is `smart_calc`. Use `Tool("name").input[A]` when the derived name is not the one
you want.
