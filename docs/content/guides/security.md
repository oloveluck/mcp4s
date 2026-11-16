# Security

mcp4s does not ship an authentication scheme. It exposes the MCP endpoint as ordinary http4s
routes, so the standard middleware ecosystem applies, and it implements the protections the spec
requires of the transport itself.

## What the transport does for you

- **DNS rebinding.** A server bound to localhost refuses requests whose `Host` or `Origin` is not
  a localhost origin (`403`). Browsers on the same machine cannot be tricked into driving it.
  `HttpConfig.allowedOrigins` whitelists additional origins.
- **Sessions.** Session ids are random UUIDs and are required on every request after
  `initialize`; unknown ids get `404`. Sessions expire after `SessionConfig.timeout` of inactivity
  and are capped at `maxSessions`.
- **Protocol version.** An unsupported `MCP-Protocol-Version` header is refused with `400`.

## Adding authentication and CORS

Take the raw routes and wrap them. A bearer-token check and a CORS policy:

```scala
import cats.data.{Kleisli, OptionT}
import org.http4s.{AuthScheme, AuthedRoutes, Credentials, Request}
import org.http4s.headers.Authorization
import org.http4s.server.{AuthMiddleware, Router}
import org.http4s.server.middleware.CORS
import org.http4s.ember.server.EmberServerBuilder
import mcp4s.server.transport.HttpConfig

def validToken(token: String): IO[Boolean] = ???   // JWT validation, a lookup, …

val authUser: Kleisli[OptionT[IO, *], Request[IO], String] = Kleisli { req =>
  OptionT(
    req.headers.get[Authorization].collect {
      case Authorization(Credentials.Token(AuthScheme.Bearer, token)) => token
    } match
      case Some(token) => validToken(token).map(ok => Option.when(ok)(token))
      case None        => IO.pure(None)
  )
}

val bearerAuth: AuthMiddleware[IO, String] = AuthMiddleware(authUser)

server.http(HttpConfig()).routes.flatMap: mcpRoutes =>
  val authed   = bearerAuth(AuthedRoutes(ctx => mcpRoutes.run(ctx.req)))
  val withCors = CORS.policy.withAllowOriginAll.withAllowCredentials(false).apply(authed)
  EmberServerBuilder.default[IO].withHttpApp(Router("/" -> withCors).orNotFound).build
```

On the client, `HttpTransportConfig(auth = Some(McpAuth.Bearer("…")))` sends the matching
header; `McpAuth.TokenProvider(io)` fetches a fresh token before each request. The same `McpAuth`
authenticates WebSocket upgrades.

Rate limiting, request logging and metrics are more http4s middleware applied the same way.
