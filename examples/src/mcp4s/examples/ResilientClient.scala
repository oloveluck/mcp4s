package mcp4s.examples

import cats.effect.{IO, IOApp}
import io.circe.Json
import org.typelevel.otel4s.trace.Tracer
import mcp4s.client.*
import mcp4s.client.retry.*
import mcp4s.client.transport.*
import mcp4s.protocol.*
import org.http4s.ember.client.EmberClientBuilder

import scala.concurrent.duration.*

/** Example MCP client with resilience configuration.
  *
  * Demonstrates using `ResilienceConfig` fluent builders to configure
  * retry and timeout at transport connect time.
  *
  * Usage:
  *   1. Start the calculator server: mill examples.runMain mcp4s.examples.CalculatorServer
  *   2. In another terminal: mill examples.runMain mcp4s.examples.ResilientClient
  */
object ResilientClient extends IOApp.Simple:

  val client: McpClient[IO] = McpClient.builder[IO]
    .withInfo(ClientInfo("resilient-client", "1.0.0"))
    .build

  val resilience: ResilienceConfig = ResilienceConfig.default
    .withRetry(RetryPolicy.exponentialBackoff(maxRetries = 5, baseDelay = 200.millis))
    .withTimeout(10.seconds)

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    IO.println("Connecting with resilience (retry + timeout)...") *>
      EmberClientBuilder.default[IO].build.use { httpClient =>
        HttpClientTransport.connect[IO](
          client,
          HttpClientConfig[IO]("http://localhost:3000"),
          httpClient,
          resilience = Some(resilience)
        ).use { conn =>
          for
            _ <- IO.println(s"Connected to: ${conn.serverInfo.name} v${conn.serverInfo.version}")
            tools <- conn.listTools
            _ <- IO.println(s"Available tools: ${tools.map(_.name).mkString(", ")}")
            result <- conn.callTool("add", Json.obj(
              "a" -> Json.fromDouble(5.0).get,
              "b" -> Json.fromDouble(3.0).get
            ))
            _ <- IO.println(s"5 + 3 = ${result.content.headOption.collect { case TextContent(t, _, _) => t }.getOrElse("?")}")
            _ <- conn.shutdown
          yield ()
        }
      }
