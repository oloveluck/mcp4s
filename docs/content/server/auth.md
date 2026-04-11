# HTTP Security

MCP servers exposed over HTTP should be secured. mcp4s exposes raw `HttpRoutes[F]` via `HttpTransport.routes`, so you can compose standard http4s middleware — CORS, authentication, rate limiting — directly.

## Composable Routes

Use `HttpTransport.routes` to get the raw MCP routes, then wrap them with any http4s middleware:

```scala
import cats.data.{Kleisli, OptionT}
import cats.effect.IO
import org.http4s.*
import org.http4s.headers.Authorization
import org.http4s.server.{AuthMiddleware, Router}
import org.http4s.server.middleware.CORS
import org.http4s.ember.server.EmberServerBuilder
import mcp4s.server.transport.*

// 1. Define a bearer-token auth check
val authUser: Kleisli[OptionT[IO, *], Request[IO], String] =
  Kleisli { req =>
    OptionT.fromOption[IO](
      req.headers.get[Authorization].collect {
        case Authorization(Credentials.Token(AuthScheme.Bearer, token))
            if token == "my-secret" => token
      }
    )
  }

val bearerAuth: AuthMiddleware[IO, String] = AuthMiddleware(authUser)

HttpTransport.routes[IO](server).flatMap { mcpRoutes =>
  // 2. Wrap MCP routes with bearer-token auth
  val authed = bearerAuth(AuthedRoutes { req =>
    mcpRoutes.run(req.req)
  })

  // 3. Apply CORS
  val withCors = CORS.policy
    .withAllowOriginAll
    .withAllowCredentials(false)
    .apply(authed)

  EmberServerBuilder.default[IO]
    .withHttpApp(Router("/" -> withCors).orNotFound)
    .build
}
```

## Middleware

Since `HttpTransport.routes` returns standard `HttpRoutes[F]`, any http4s middleware works out of the box.

### Authentication

Use `org.http4s.server.AuthMiddleware` with a `Kleisli` that extracts credentials from the request. The example above shows a simple bearer token check — for production use, swap the token comparison for JWT validation using a library like [http4s-jwt-auth](https://github.com/profunktor/http4s-jwt-auth).

### CORS

Use `org.http4s.server.middleware.CORS` with a `CORSPolicy`:

```scala
import org.http4s.server.middleware.CORS
import org.http4s.Method
import scala.concurrent.duration.*

val corsRoutes = CORS.policy
  .withAllowOriginAll
  .withAllowCredentials(false)
  .withAllowMethodsIn(Set(Method.GET, Method.POST, Method.DELETE))
  .withMaxAge(1.day)
  .apply(mcpRoutes)
```

### Rate Limiting and Logging

The http4s ecosystem includes middleware for [request logging](https://http4s.org/v0.23/docs/server-middleware.html), metrics, and more. These compose the same way — wrap `mcpRoutes` before passing to the server builder.

## Session Management

Sessions track individual client connections and manage their state. The server issues a session ID via the `Mcp-Session-Id` header:

```scala
val sessionConfig = SessionConfig(
  timeout = 30.minutes,
  maxQueueSize = 1000,
  requestTimeout = 5.minutes
)

HttpTransport.serve[IO](server, HttpConfig(
  port = port"3000",
  enableSessions = true,
  sessionConfig = sessionConfig
))
```

## DNS Rebinding Protection

When the server is bound to localhost, it automatically validates the `Host` header and rejects requests with non-localhost origins. This prevents DNS rebinding attacks without any configuration.

---
**Next:** [Client Guide](../client/README.md)
