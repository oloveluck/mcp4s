# Errors

There are three kinds of failure, and they travel in different channels on purpose.

**Tool errors** are results. A tool that cannot do what was asked returns `error("…")`, an
`isError` result the model can read and react to (retry with better arguments, ask the user).
The spec asks that input validation failures be reported this way rather than as protocol
errors, so the model can self-correct.

**Protocol errors** are `McpError` values: an unknown tool, invalid arguments that failed schema
decoding, a method the server does not support, a request cancelled or timed out. On the server
they become JSON-RPC error responses with the standard codes; on the client they are raised.

| Code | Meaning | Typical `McpError` |
|---|---|---|
| -32700 | parse error | malformed JSON |
| -32600 | invalid request | `NotInitialized`, `ProtocolVersionMismatch`, `SamplingNotSupported` |
| -32601 | method not found | `MethodNotFound`, `ToolNotFound`, task support violations |
| -32602 | invalid params | `InvalidToolArguments`, `ResourceNotFound`, `PromptNotFound`, unknown task |
| -32603 | internal error | `InternalError`, uncaught exceptions from handlers |
| -32800 | request cancelled | `RequestCancelled` |

**Transport errors** are whatever the transport raised: a refused connection, a closed socket,
an HTTP status with no JSON-RPC body. They are not `McpError`s.

On the client this gives a natural pattern:

```scala
import io.circe.Json
import mcp4s.protocol.McpError

conn.callTool("tool", Json.obj()).attempt.flatMap:
  case Right(r) if r.isError.contains(true) => IO.println(s"tool said no: ${r.textContent}")
  case Right(r)                             => IO.println(s"ok: ${r.textContent}")
  case Left(e: McpError)                    => IO.println(s"protocol: ${e.message}")
  case Left(e)                              => IO.println(s"transport: ${e.getMessage}")
```

Typed calls (`conn.call(endpoint)(input)`) collapse the first two cases: an `isError` result is
raised as `McpError.ToolExecutionError`, so a typed call either returns the decoded value or
fails.

Timeouts are configured per transport with `Timeouts(request, init)`; retry is left to http4s
middleware on the client you supply, or to re-acquiring the connection `Resource`.
