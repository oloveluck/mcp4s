package mcp4s.server

import cats.effect.IO
import io.circe.Json
import io.circe.syntax.*
import mcp4s.protocol.*
import munit.CatsEffectSuite

class McpToolAttemptSpec extends CatsEffectSuite:

  case class FetchArgs(url: String) derives ToolInput

  test("McpTool.attempt converts success to text result") {
    val fetch = McpTool.attempt[IO, FetchArgs]("fetch", "Fetch URL") { args =>
      IO.pure(s"Response from ${args.url}")
    }

    val json = Json.obj("url" -> "https://example.com".asJson)
    for
      result <- fetch.call("fetch", json).value
      _ = assert(result.isDefined)
      _ = assertEquals(result.get.isError.getOrElse(false), false)
      _ = assertEquals(result.get.textContent, "Response from https://example.com")
    yield ()
  }

  test("McpTool.attempt converts exceptions to error result") {
    val fetch = McpTool.attempt[IO, FetchArgs]("fetch", "Fetch URL") { args =>
      IO.raiseError(new RuntimeException("Connection refused"))
    }

    val json = Json.obj("url" -> "https://example.com".asJson)
    for
      result <- fetch.call("fetch", json).value
      _ = assert(result.isDefined)
      _ = assertEquals(result.get.isError.getOrElse(false), true)
      _ = assertEquals(result.get.textContent, "Connection refused")
    yield ()
  }

  test("McpTool.attemptNoArgs converts success to text result") {
    val version = McpTool.attemptNoArgs[IO]("version", "Get version") {
      IO.pure("1.0.0")
    }

    for
      result <- version.call("version", Json.obj()).value
      _ = assert(result.isDefined)
      _ = assertEquals(result.get.isError.getOrElse(false), false)
      _ = assertEquals(result.get.textContent, "1.0.0")
    yield ()
  }

  test("McpTool.attemptNoArgs converts exceptions to error result") {
    val fail = McpTool.attemptNoArgs[IO]("fail", "Always fails") {
      IO.raiseError(new RuntimeException("Service unavailable"))
    }

    for
      result <- fail.call("fail", Json.obj()).value
      _ = assert(result.isDefined)
      _ = assertEquals(result.get.isError.getOrElse(false), true)
      _ = assertEquals(result.get.textContent, "Service unavailable")
    yield ()
  }

  test("McpTool.attemptWith uses custom error formatter") {
    case class DbError(code: Int, msg: String) extends Exception(msg)

    val query = McpTool.attemptWith[IO, FetchArgs]("query", "Run query") { args =>
      IO.raiseError(DbError(1045, "Access denied"))
    } { e =>
      e match
        case DbError(code, msg) => s"Database error $code: $msg"
        case other => s"Unexpected: ${other.getMessage}"
    }

    val json = Json.obj("url" -> "db://query".asJson)
    for
      result <- query.call("query", json).value
      _ = assert(result.isDefined)
      _ = assertEquals(result.get.isError.getOrElse(false), true)
      _ = assertEquals(result.get.textContent, "Database error 1045: Access denied")
    yield ()
  }

  test("McpTool.attemptWith handles generic exceptions") {
    val query = McpTool.attemptWith[IO, FetchArgs]("query", "Run query") { _ =>
      IO.raiseError(new IllegalArgumentException("bad input"))
    } {
      case _: IllegalArgumentException => "Invalid arguments provided"
      case e => s"Error: ${e.getMessage}"
    }

    val json = Json.obj("url" -> "x".asJson)
    for
      result <- query.call("query", json).value
      _ = assertEquals(result.get.textContent, "Invalid arguments provided")
    yield ()
  }

  test("McpTool.attempt does not catch decoding errors") {
    val fetch = McpTool.attempt[IO, FetchArgs]("fetch", "Fetch URL") { args =>
      IO.pure(s"Response from ${args.url}")
    }

    // Missing required 'url' field - this should still be an exception
    val json = Json.obj("wrong" -> "field".asJson)
    for
      result <- fetch.call("fetch", json).value.attempt
      _ = assert(result.isLeft, "Decoding errors should still propagate")
    yield ()
  }
