# Resources

MCP **resources** expose data to AI clients via URIs. While tools let AI *do* things, resources let AI *read* things — files, database records, API responses, configuration, or any content that can be addressed by a URI.

> For the full protocol details, see [Resources](https://spec.modelcontextprotocol.io/specification/2025-03-26/server/resources/) in the MCP specification.

Clients discover available resources with `listResources`, then fetch them with `readResource("uri")`. Resources can be **static** (fixed URI) or **templates** (URI pattern with parameters like `api://users/{id}`).

## Constructors

```scala
import mcp4s.server.dsl.*

// Static text
Resource.text[IO]("file:///readme", "README")("Hello world")

// Dynamic (effectful)
Resource[IO]("file:///status", "Status")(getStatus().map(s => text("file:///status", s)))

// Template (pattern matching)
Resource.template[IO]("api://users/{id}", "User", "Get user by ID"): uri =>
  fetchUser(uri.split("/").last).map(u => text(uri, u.toJson))
```

Templates use URI patterns with `{param}` placeholders. Clients discover templates via `listResourceTemplates` and substitute parameters to form concrete URIs.

## Content Types

```scala
// Plain text
text("uri", "text content")

// Binary (base64-encoded)
blob("uri", base64Data, "image/png")
```

`text` and `blob` come from `mcp4s.server.dsl`. Set MIME types when constructing resources (e.g. `text/plain`, `application/json`, `image/png`). Text resources default to `text/plain`.

## Composition

```scala
val resources = readme |+| config |+| userTemplate
```

## Register with a Server

```scala
val resources =
  Resource.text[IO]("file:///readme", "README")("Hello") |+|
    Resource.handler[IO]("file:///status", "Status")(_ =>
      getStatus().map(s => ResourceContent.text("file:///status", s))
    )

val server = McpServer[IO](ServerInfo("my-server", "1.0.0")).withResources(resources)
```

A server with resources registered advertises the `resources` capability automatically.

## Subscribable Resources

Resources can notify clients when their content changes. Use `subscribable` with an fs2 change stream, or `polling` for periodic checks:

```scala
// Change-stream driven — notifies when the stream emits
Resource.subscribable[IO]("db://status", "DB Status", dbChangeStream)(uri =>
  getDbStatus().map(s => text(uri, s))
)

// Polling — checks a condition on an interval
Resource.polling[IO]("file:///config", "Config", 10.seconds, configChanged)(uri =>
  readConfig().map(c => text(uri, c.toString))
)
```

Clients subscribe via `subscribeResource` and receive `notifications/resources/updated` when changes occur. This works with all persistent transports (HTTP with SSE, WebSocket, Stdio).

The server advertises `resources.subscribe = true` only when at least one subscribable resource is registered — capabilities always reflect what's actually there.

## Template Example

A more realistic template exposing database records:

```scala
Resource.template[IO]("db://orders/{orderId}", "Order", "Fetch order by ID"): uri =>
  orderRepo.findById(uri.split("/").last).flatMap:
    case Some(order) => IO.pure(text(uri, order.toJson))
    case None        => IO.raiseError(McpError.ResourceNotFound(uri))
```

---
**Next:** [Prompts](prompts.md)
