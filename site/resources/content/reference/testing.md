# Testing

MCP4S provides testing utilities for verifying servers and tools without running a transport.

## ServerTest

Test a full server with an in-memory client:

```scala
import cats.effect.IO
import munit.CatsEffectSuite
import mcp4s.server.*
import mcp4s.server.testing.*

class MyServerSuite extends CatsEffectSuite:

  val server = Server.builder[IO]
    .withInfo(ServerInfo("test", "1.0.0"))
    .tool[AddArgs]("add", "Add") { args => IO.pure(ok(s"${args.a + args.b}")) }
    .build

  test("add tool returns correct result") {
    ServerTest(server).use { client =>
      for
        result <- client.callTool("add", AddArgs(2.0, 3.0))
      yield assertEquals(result.textContent, "5.0")
    }
  }
```

### Synchronous Variant

For simpler tests that don't need Resource lifecycle:

```scala
test("sync test") {
  val client = ServerTest.sync(server)
  for
    result <- client.callTool("add", AddArgs(1.0, 2.0))
  yield assertEquals(result.textContent, "3.0")
}
```

### Available Operations

```scala
client.listTools                           // IO[List[Tool]]
client.callTool("name", args)             // IO[ToolResult]
client.callToolJson("name", json)         // IO[ToolResult]
client.listResources                       // IO[List[Resource]]
client.readResource("file:///path")       // IO[ResourceContent]
client.listPrompts                         // IO[List[Prompt]]
client.getPrompt("name", args)            // IO[GetPromptResult]
client.getPromptMap("name", Map("k" -> "v"))  // IO[GetPromptResult]
```

## ToolsTest

Test tools directly without building a full server:

```scala
import mcp4s.server.testing.*

import mcp4s.server.mcp.*

case class AddArgs(a: Double, b: Double) derives ToolInput

val tools = Tool[IO, AddArgs]("add", "Add") { args =>
  IO.pure(ToolResult.text(s"${args.a + args.b}"))
}

test("call tool directly") {
  for
    result <- tools.testCall("add", args("a" -> 3.0, "b" -> 2.0))
  yield assertEquals(result.textContent, "5.0")
}

test("tool exists") {
  for
    exists <- tools.hasTool("add")
  yield assert(exists)
}

test("get tool definition") {
  for
    tool <- tools.assertTool("add")
  yield assertEquals(tool.name, "add")
}
```

### Extension Methods

```scala
tools.testCall("name", arguments)   // Call tool, raises McpError.ToolNotFound if missing
tools.testCallJson("name", json)    // Call with raw JSON
tools.hasTool("name")               // IO[Boolean]
tools.getTool("name")               // IO[Option[Tool]]
tools.assertTool("name")            // IO[Tool], raises AssertionError if missing
```

## args Helper

Build JSON arguments concisely:

```scala
import mcp4s.server.testing.args

args("a" -> 2.0, "b" -> 3.0)              // Json.obj("a" -> 2.0, "b" -> 3.0)
args("query" -> "hello", "limit" -> 10)    // Json.obj("query" -> "hello", "limit" -> 10)
args("name" -> "Alice")                    // Json.obj("name" -> "Alice")
args.empty                                 // Json.obj()
```

Supports `String`, `Int`, `Double`, `Boolean`, and up to 4 key-value pairs.

## Patterns

### Testing Error Cases

```scala
test("unknown tool raises error") {
  tools.testCall("nonexistent", args.empty).intercept[McpError]
}
```

### Testing Resources

```scala
test("read resource") {
  ServerTest(server).use { client =>
    for
      content <- client.readResource("file:///readme")
    yield assert(content.text.exists(_.contains("Hello")))
  }
}
```

### Testing Prompts

```scala
test("prompt generates messages") {
  ServerTest(server).use { client =>
    for
      result <- client.getPromptMap("greet", Map("name" -> "Alice"))
    yield assert(result.messages.nonEmpty)
  }
}
```
