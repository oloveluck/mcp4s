// GENERATED from transports/http.md — do not edit; regenerate with snippet_harness.py
package docsnippets.transports_http

import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import io.circe.Json
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.server.{McpServer, Prompts, Resources, Server, ServiceRoutes, ToolContext, Tools}
import mcp4s.server.transport.{HttpConfig, SessionConfig, WebSocketConfig}
import mcp4s.client.{McpClient, McpClientBuilder, McpConnection}
import mcp4s.client.transport.{HttpTransportConfig, McpAuth, StdioTransportConfig, WebSocketTransportConfig}
import mcp4s.transport.Timeouts

object stubs:
  def conn: McpConnection[IO]                  = ???
  def connection: McpConnection[IO]            = ???
  def httpClient: org.http4s.client.Client[IO] = ???
  def server: Server[IO]                       = ???
  def client: McpClient[IO]                    = ???

object scope_1:
  import stubs.{*, given}
  // ---- snippet at line 12
  // Simple — starts an Ember server on port 3000, path /mcp
  server.http().resource.useForever

  // With custom config
  import mcp4s.server.transport.*
  import com.comcast.ip4s.*

  server.http(HttpConfig(
    host = host"0.0.0.0",
    port = port"3000",
    path = "mcp",
    enableSessions = true
  )).resource.useForever

  // ---- snippet at line 44
  import org.http4s.HttpRoutes
  import org.http4s.server.middleware.CORS
  import org.http4s.server.Router
  import org.http4s.ember.server.EmberServerBuilder

  def myAppRoutes: HttpRoutes[IO] = ???   // your existing routes

  server.http(HttpConfig[IO]()).routes.flatMap: mcpRoutes =>
    // Wrap with CORS, combine with your own routes
    val withCors  = CORS.policy.withAllowOriginAll.withAllowCredentials(false).apply(mcpRoutes)
    val allRoutes = withCors <+> myAppRoutes
    EmberServerBuilder.default[IO].withHttpApp(Router("/" -> allRoutes).orNotFound).build

  // ---- snippet at line 63
  import mcp4s.client.syntax.*      // JVM-only convenience overloads
  import mcp4s.client.transport.*

  // JVM one-liner — builds and manages an Ember client for you
  client.http("http://localhost:3000/mcp").use(conn => conn.listAllTools)

  // Cross-platform — bring your own http4s Client[F]
  client.http(HttpTransportConfig[IO]("http://localhost:3000/mcp"), httpClient).use(conn => conn.listAllTools)

  // ---- snippet at line 87
  def fetchToken: IO[String] = ???   // your token refresh flow

  val config = HttpTransportConfig[IO](
    uri  = "https://api.example.com/mcp",
    auth = Some(McpAuth.Bearer("my-token"))              // static token
  )

  // or resolve a fresh token before each request:
  val refreshing = config.copy(auth = Some(McpAuth.TokenProvider(fetchToken)))

  // ---- snippet at line 104
  import org.http4s.client.middleware.{Retry, RetryPolicy}
  import scala.concurrent.duration.*

  val retryPolicy = RetryPolicy[IO](RetryPolicy.exponentialBackoff(maxWait = 10.seconds, maxRetry = 3))
  val resilientClient = Retry(retryPolicy)(httpClient)

  client.http(HttpTransportConfig[IO]("http://localhost:3000/mcp"), resilientClient).use(conn => conn.listAllTools)

