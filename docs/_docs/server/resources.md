# Resources

## Constructors

```scala
import mcp4s.server.mcp.*

// Static text
Resource.text[IO]("file:///readme", "README") { "Hello world" }

// Dynamic
Resource[IO]("file:///status", "Status") {
  getStatus().map(s => mcp.text("file:///status", s))
}

// Template (pattern matching)
Resource.template[IO]("api://users/{id}", "User", "Get user by ID") { uri =>
  val id = uri.split("/").last
  fetchUser(id).map(u => mcp.text(uri, u.toJson))
}
```

## Content Types

```scala
mcp.text("uri", "text content")
mcp.blob("uri", base64Data, "image/png")
```

## Composition

```scala
val resources = readme |+| config |+| userTemplate
```

## With Builder

```scala
McpServer.builder[IO]
  .resource("file:///readme", "README") { "Hello" }
  .withResource("file:///status", "Status") { _ =>
    getStatus().map(s => ResourceContent.text("file:///status", s))
  }
  .build
```
