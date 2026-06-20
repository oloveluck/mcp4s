# Resources

MCP **resources** expose data to AI clients via URIs. While tools let AI *do* things, resources let AI *read* things — files, database records, API responses, configuration, or any content that can be addressed by a URI.

> For the full protocol details, see [Resources](https://spec.modelcontextprotocol.io/specification/2025-03-26/server/resources/) in the MCP specification.

Clients discover available resources with `listResources`, then fetch them with `readResource("uri")`. Resources can be **static** (fixed URI) or **templates** (URI pattern with parameters like `api://users/{id}`).

## Constructors

```scala
import mcp4s.server.mcp.*

// Static text
Resource.text[IO]("file:///readme", "README") { "Hello world" }

// Dynamic (effectful)
Resource[IO]("file:///status", "Status") {
  getStatus().map(s => mcp.text("file:///status", s))
}

// Template (pattern matching)
Resource.template[IO]("api://users/{id}", "User", "Get user by ID") { uri =>
  val id = uri.split("/").last
  fetchUser(id).map(u => mcp.text(uri, u.toJson))
}
```

Templates use URI patterns with `{param}` placeholders. Clients discover templates via `listResourceTemplates` and substitute parameters to form concrete URIs.

## Content Types

```scala
// Plain text
mcp.text("uri", "text content")

// Binary (base64-encoded)
mcp.blob("uri", base64Data, "image/png")
```

Set MIME types when constructing resources (e.g. `text/plain`, `application/json`, `image/png`). Text resources default to `text/plain`.

## Composition

```scala
val resources = readme |+| config |+| userTemplate
```

## Register with a Server

```scala
val resources =
  Resource.text[IO]("file:///readme", "README")("Hello") |+|
    Resource.handler[IO]("file:///status", "Status") { _ =>
      getStatus().map(s => ResourceContent.text("file:///status", s))
    }

Server.from[IO](ServerInfo("my-server", "1.0.0"), Tools.empty[IO], resources, Prompts.empty[IO])
```

## Subscribable Resources

Resources can notify clients when their content changes. Use `subscribable` with an fs2 change stream, or `polling` for periodic checks:

```scala
// Change-stream driven — notifies when the stream emits
Resource.subscribable[IO]("db://status", "DB Status", dbChangeStream) { uri =>
  getDbStatus().map(s => mcp.text(uri, s))
}

// Polling — checks a condition on an interval
Resource.polling[IO]("file:///config", "Config", 10.seconds, configChanged) { uri =>
  readConfig().map(c => mcp.text(uri, c.toString))
}
```

Clients subscribe via `subscribeResource` and receive `notifications/resources/updated` when changes occur. This works with all persistent transports (HTTP with SSE, WebSocket, Stdio).

## Template Example

A more realistic template exposing database records:

```scala
Resource.template[IO](
  "db://orders/{orderId}",
  "Order",
  "Fetch order by ID"
) { uri =>
  val orderId = uri.split("/").last
  orderRepo.findById(orderId).flatMap {
    case Some(order) => IO.pure(mcp.text(uri, order.toJson))
    case None        => IO.raiseError(McpError.ResourceNotFound(uri))
  }
}
```

---
**Next:** [Prompts](prompts.md)
