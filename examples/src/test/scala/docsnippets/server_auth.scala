// GENERATED from server/auth.md — do not edit; regenerate with snippet_harness.py
package docsnippets.server_auth

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
  // ---- snippet at line 10
  import cats.data.{Kleisli, OptionT}
  import cats.effect.IO
  import org.http4s.{AuthScheme, AuthedRoutes, Credentials, Request}
  import org.http4s.headers.Authorization
  import org.http4s.implicits.*
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

  server.http(HttpConfig[IO]()).routes.flatMap: mcpRoutes =>
    // 2. Wrap MCP routes with bearer-token auth
    val authed = bearerAuth(AuthedRoutes(req => mcpRoutes.run(req.req)))
    // 3. Apply CORS
    val withCors = CORS.policy.withAllowOriginAll.withAllowCredentials(false).apply(authed)
    EmberServerBuilder.default[IO].withHttpApp(Router("/" -> withCors).orNotFound).build

  // ---- snippet at line 56
  import org.http4s.{HttpRoutes, Method}
  import scala.concurrent.duration.*

  def corsRoutes(mcpRoutes: HttpRoutes[IO]): HttpRoutes[IO] = CORS.policy
    .withAllowOriginAll
    .withAllowCredentials(false)
    .withAllowMethodsIn(Set(Method.GET, Method.POST, Method.DELETE))
    .withMaxAge(1.day)
    .apply(mcpRoutes)

  // ---- snippet at line 76
  val sessionConfig = SessionConfig(
    timeout = 30.minutes,
    maxQueueSize = 1000,
    requestTimeout = 5.minutes
  )

  server.http(HttpConfig(
    port = port"3000",
    enableSessions = true,
    sessionConfig = sessionConfig
  )).resource.useForever

