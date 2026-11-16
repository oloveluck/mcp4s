# Server-to-client traffic

Most of MCP is the client asking and the server answering. The interesting part is the other
direction. While a tool runs, the server can report **progress**, write **log** lines, ask the
client's model for a completion (**sampling**) and ask the user a question (**elicitation**); it
can also push **resource updates** a client subscribed to and run **tasks** that outlive the
call that started them. mcp4s exposes all of this through one object a context-aware handler
receives, and delivers it the same way on every transport.

## The tool context

Attach a handler with `handleWith` (or `streamWith`) instead of `handle` and it receives a
`ToolContext` alongside the decoded arguments:

```scala
import mcp4s.server.dsl.*

case class ReviewArgs(path: String) derives Schema
case class Confirm(proceed: Boolean) derives Schema
def readFile(path: String): IO[String]   = ???
def rewrite(path: String, text: String): IO[Unit] = ???

val review = Tool.from[ReviewArgs].withDescription("Review a file and apply suggestions")
  .handleWith[IO] { (args, ctx) =>
    for
      _      <- ctx.log(LogLevel.Info, s"Reviewing ${args.path}")
      source <- readFile(args.path)
      _      <- ctx.progress(1, Some(3))
      reply  <- ctx.sampling.createMessage(
        CreateMessageParams(List(SamplingMessage(Role.User, SamplingTextContent(source))), maxTokens = 800)
      )
      _      <- ctx.progress(2, Some(3))
      answer <- ctx.elicitation.elicit(ElicitFormParams("Apply the suggestions?", Schema[Confirm].jsonSchema))
      result <- answer.action match
        case ElicitAction.Accept => rewrite(args.path, reply.content.toString).as(ok("Applied"))
        case _                   => IO.pure(ok("Left unchanged"))
      _      <- ctx.progress(3, Some(3))
    yield result
  }
```

Each capability is gated on what the *client* declared during the handshake. `ctx.sampling`
against a client with no sampling handler fails with `McpError.SamplingNotSupported` instead of
hanging; `ctx.sampling.supportsSampling` lets you branch first. Log lines below the level the
client chose with `logging/setLevel` are dropped on the server.

On the client, the same features are handlers you add to the builder, and adding one is what
advertises the capability:

```scala
import mcp4s.client.{McpClientBuilder, Notifications}
import mcp4s.client.mcp.*
import mcp4s.protocol.*

def myLlm(p: CreateMessageParams): IO[CreateMessageResult] = ???
def askUser(p: ElicitParams): IO[ElicitResult]            = ???

val client = McpClientBuilder[IO](ClientInfo("my-client", "1.0.0"))
  .withSampling(Sampling[IO](myLlm))
  .withElicitation(Elicitation[IO](askUser))
  .withNotifications(Notifications[IO] {
    case ServerNotification.ResourceUpdated(uri) => IO.println(s"$uri changed")
    case ServerNotification.Log(m)               => IO.println(m.data.noSpaces)
  })
```

Progress is per request rather than a global handler: pass a callback to `callTool` and only
that call's notifications reach it.

## How it reaches the client

The spec distinguishes messages that *relate to a request* (progress, logs, sampling and
elicitation made while handling it) from messages that do not (a subscribed resource changing).
mcp4s keeps that distinction explicit in the server session, and each transport maps it:

| | related to a request | unrelated |
|---|---|---|
| stdio, WebSocket | the connection | the connection |
| Streamable HTTP | the request's own SSE stream | the session's standalone GET stream |

Over HTTP, a sampling request is written to the tool call's stream, the client answers it with
a POST, and the session matches the answer to the waiting handler by id. Nothing is ever
broadcast to more than one stream, and a message emitted after its request's stream has closed
(a task still running) falls back to the standalone stream.

## Resource subscriptions

A resource built with `Resource.subscribable` (or `Resource.polling`) carries a change stream.
The server advertises `resources.subscribe`, each session remembers the URIs its client
subscribed to with `conn.subscribeResource(uri)`, and matching changes become
`notifications/resources/updated` on that session's unrelated channel. The change stream is
consumed once per connected session, so build it from something that can be subscribed to more
than once, such as an fs2 `Topic`.

## Tasks

A task-augmented `tools/call` (SEP-1686, experimental in 2025-11-25) returns immediately with a
task id; the client polls `tasks/get` and fetches the outcome with `tasks/result`. Tools opt in
with `withTaskSupport(TaskSupport.Optional)` (or `Required`), which also makes the server
advertise the `tasks` capability. The handler is unchanged: the dispatcher runs it in a background
fiber, tracks `working → completed | failed | cancelled`, retains the result for the task's TTL,
and reports a tool error result as a `failed` task whose result is still retrievable, as the spec
requires.

```scala
conn.callToolTask("review", Json.obj("path" -> Json.fromString("src/Main.scala"))).flatMap { task =>
  conn.taskResult(task.taskId)   // answered once the task is terminal
}
```

Progress and logs a task emits after its original call returned go to the standalone channel,
so a client that keeps the GET stream open (the mcp4s client does by default) still sees them.
