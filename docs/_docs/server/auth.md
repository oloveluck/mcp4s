# Authentication

## HTTP Transport

```scala
import mcp4s.server.transport.*

val authConfig = AuthConfig(
  validateToken = token => IO.pure(token == "secret")
)

HttpTransport.serve[IO](server, HttpConfig(
  port = 3000,
  auth = Some(authConfig)
))
```

## Session Management

```scala
val sessionConfig = SessionConfig(
  timeout = 30.minutes,
  maxQueueSize = 1000,
  requestTimeout = 5.minutes
)

HttpTransport.serve[IO](server, HttpConfig(
  port = 3000,
  enableSessions = true,
  sessionConfig = sessionConfig
))
```

## CORS

```scala
val corsConfig = CorsConfig(
  allowOriginAll = false,
  allowedOrigins = Set("https://app.example.com"),
  exposeSessionHeader = true
)

HttpTransport.serve[IO](server, HttpConfig(
  port = 3000,
  enableCors = true,
  cors = Some(corsConfig)
))
```

## DNS Rebinding Protection

```scala
val dnsConfig = DnsRebindingConfig(
  enabled = true,
  allowedHosts = Set("localhost", "127.0.0.1")
)

HttpTransport.serve[IO](server, HttpConfig(
  port = 3000,
  dnsRebinding = Some(dnsConfig)
))
```
