# Architecture

MCP is JSON-RPC 2.0 with a vocabulary: `initialize`, `tools/call`, `resources/read`, a handful of
notifications, and a few requests that flow the *other* way, from server to client. mcp4s is built
as a stack of small layers so that the vocabulary is implemented once, in one place, and every
transport and every handler sits on top of it.

## The layers

```
             ┌──────────────────────────────────────────────────────────┐
  your code  │  handlers (tools / resources / prompts)                  │
             ├──────────────────────────────────────────────────────────┤
  mcp4s-core │  Schema[A]  →  ToolEndpoint / PromptEndpoint             │  describe
             ├──────────────────────────────────────────────────────────┤
  mcp4s-     │  Tools[F] |+| Resources[F] |+| Prompts[F]  →  Server[F]  │  route
  server     ├──────────────────────────────────────────────────────────┤
             │  Dispatcher  (one per session: state, tasks, subs)       │  speak MCP
             ├──────────────────────────────────────────────────────────┤
             │  ServerSession  (correlation, tool context, outbound)    │  talk back
             ├──────────────────────────────────────────────────────────┤
             │  stdio  ·  Streamable HTTP  ·  WebSocket                 │  frame + carry
             └──────────────────────────────────────────────────────────┘
```

Each layer only knows the one below it:

- **Schema** turns a Scala type into everything the protocol needs to know about it. It lives in
  `mcp4s-core` because both sides use it.
- **Endpoints** (`ToolEndpoint[I, O]`, `PromptEndpoint[I]`) are *definitions*: a name, a
  description, typed input and output. They carry no behaviour, which is what lets a client and a
  server share them.
- **Routes** (`Tools`, `Resources`, `Prompts`) are endpoints with handlers attached. They are
  values with a `Semigroup`, so they combine with `|+|` and the first match wins.
- **`Server[F]`** is the plain interface the protocol layer calls: list tools, call a tool, read
  a resource, get a prompt. `McpServer` assembles one from routes and derives its capabilities
  from what was registered; you can also implement the trait by hand.
- **`Dispatcher`** is the protocol state machine for *one session*: it performs the
  `initialize` handshake and version negotiation, tracks in-flight requests so
  `notifications/cancelled` can interrupt them, remembers the client's log level and resource
  subscriptions, and runs task-augmented calls. It maps method names to `Server` calls and turns
  failures into JSON-RPC errors.
- **`ServerSession`** adds the half of the protocol that goes *from* the server: it correlates
  server-initiated requests (sampling, elicitation) with the client's answers, builds the
  `ToolContext` a handler sees, and forwards subscribed resource changes. It is shared by all
  three transports.
- **Transports** only frame and carry bytes. Stdio reads lines; Streamable HTTP maps requests
  onto POSTs and SSE streams; WebSocket uses frames. None of them know what a tool is.

## A request, end to end

A `tools/call` over Streamable HTTP:

1. The client POSTs one JSON-RPC request with `Accept: text/event-stream`.
2. The transport resolves the session from the `Mcp-Session-Id` header, opens an SSE stream
   dedicated to this request, and hands the message to the session.
3. `ServerSession` routes it to the `Dispatcher`, which checks the session is initialized,
   builds a `ToolContext` bound to *this request's* stream, and calls `Server.callToolWithContext`.
4. `Server` finds the route by name and the route decodes the arguments with the endpoint's
   schema, then runs your handler.
5. Anything the handler emits through the context (progress, log lines, a sampling request)
   is written to the request's stream as it happens. A sampling request parks on a `Deferred`
   until the client POSTs the answer, which the session correlates by id.
6. The handler's result is encoded, wrapped in a JSON-RPC response, written as the last event,
   and the stream ends.

Over WebSocket and stdio, steps 2 and 5 use the single duplex connection instead of a
per-request stream; everything in between is the same code.

## The client, mirrored

```
  McpClientBuilder  →  McpClient (handlers for roots / sampling / elicitation / notifications)
                              │
                       ConnectionRunner   handshake · correlation · timeouts · routing
                              │
                          McpChannel      stdio · HTTP · WebSocket  (send + incoming stream)
                              │
                       McpConnection      what your code holds
```

Every client transport implements `McpChannel`, a two-method contract: send a message, expose
the stream of incoming messages. One `ConnectionRunner` layers MCP on top: it performs the
handshake, matches responses to requests with a uniform timeout, routes progress to the
per-request callback, answers server-initiated requests with your handlers, and fails everything
in flight when the connection closes. This is why sampling works identically over HTTP, WebSocket
and stdio, and why adding a transport is a framing exercise.

## Design decisions

**Capabilities are derived, never declared.** A `tools` capability appears when you register a
tool, `resources.subscribe` when a resource is subscribable, `tasks` when a tool declares task
support; a client advertises `sampling` when it has a sampling handler. There is no way to claim
something you cannot do.

**One session, one dispatcher.** All protocol state is per session, so concurrent clients never
share initialization state, subscriptions or tasks. A stateless HTTP deployment is the same code
with a single shared session.

**Server-to-client messages belong to a request.** The spec says messages a server sends while
handling a request should relate to it and must never be broadcast across streams. The HTTP
transport gives every streamed request its own queue; messages that relate to no request (a
subscribed resource changing) go to the session's standalone stream. The shared-queue design this
replaced lost notifications under concurrency.

**Composition over configuration.** Routes and servers are semigroups. A server for a new domain
is a value you `|+|` onto an existing one, and middleware is ordinary http4s middleware applied to
`server.http(config).routes`. There are no plugin hooks.

**Effect-polymorphic, resource-safe.** Everything is `F[_]` with cats-effect constraints.
Connections, subprocesses, sessions and SSE streams are `Resource`s or fibers scoped to one, so
a client that disconnects cancels the handler serving it and nothing leaks.

**The spec, and only the spec.** mcp4s implements MCP 2025-11-25 and negotiates down to
2024-11-05 with older peers. Methods not in the spec are not offered, and the in-progress draft
revision is not implemented until it is released.

## Modules

| Module | Holds | Platforms |
|---|---|---|
| `mcp4s-core` | protocol types and codecs, `Schema`, endpoints, `McpChannel` | JVM · JS · Native |
| `mcp4s-server` | DSL, `McpServer`, `Dispatcher`, sessions, server transports | JVM · JS · Native |
| `mcp4s-client` | `McpClientBuilder`, `ConnectionRunner`, `McpConnection`, client transports | JVM · JS · Native (WebSocket client: JVM) |
| `mcp4s-testkit` | test servers and clients, compliance and performance suites | JVM · JS · Native (suites: JVM) |
