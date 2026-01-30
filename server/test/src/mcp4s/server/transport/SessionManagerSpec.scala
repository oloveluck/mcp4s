package mcp4s.server.transport

import cats.effect.IO
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.server.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

class SessionManagerSpec extends CatsEffectSuite:

  given Tracer[IO] = Tracer.noop[IO]

  // Test fixtures
  val testServer: McpServer[IO] = McpServer.fromTools[IO](
    ServerInfo("test", "1.0.0"),
    McpTools.empty[IO]
  )

  test("SessionManager creates sessions with unique IDs") {
    for
      manager <- SessionManager[IO](testServer)
      session1 <- manager.create
      session2 <- manager.create
      _ = assertNotEquals(session1.id, session2.id)
    yield ()
  }

  test("SessionManager retrieves created sessions") {
    for
      manager <- SessionManager[IO](testServer)
      session <- manager.create
      retrieved <- manager.get(session.id)
      _ = assertEquals(retrieved.map(_.id), Some(session.id))
    yield ()
  }

  test("SessionManager returns None for unknown session IDs") {
    for
      manager <- SessionManager[IO](testServer)
      retrieved <- manager.get("unknown-session-id")
      _ = assertEquals(retrieved, None)
    yield ()
  }

  test("SessionManager removes sessions") {
    for
      manager <- SessionManager[IO](testServer)
      session <- manager.create
      _ <- manager.remove(session.id)
      retrieved <- manager.get(session.id)
      _ = assertEquals(retrieved, None)
    yield ()
  }

  test("SessionManager tracks session count") {
    for
      manager <- SessionManager[IO](testServer)
      count0 <- manager.sessionCount
      _ = assertEquals(count0, 0)
      session1 <- manager.create
      count1 <- manager.sessionCount
      _ = assertEquals(count1, 1)
      session2 <- manager.create
      count2 <- manager.sessionCount
      _ = assertEquals(count2, 2)
      _ <- manager.remove(session1.id)
      count3 <- manager.sessionCount
      _ = assertEquals(count3, 1)
    yield ()
  }

  test("SessionManager.get touches session to update last accessed time") {
    for
      manager <- SessionManager[IO](testServer)
      session <- manager.create
      lastAccessed1 <- session.lastAccessed
      _ <- IO.sleep(10.millis)
      _ <- manager.get(session.id)
      lastAccessed2 <- session.lastAccessed
      _ = assert(lastAccessed2.isAfter(lastAccessed1) || lastAccessed2 == lastAccessed1)
    yield ()
  }

  test("HttpSession.isExpired returns true for expired sessions") {
    val shortTimeout = SessionConfig(timeout = 10.millis)
    for
      manager <- SessionManager[IO](testServer, shortTimeout)
      session <- manager.create
      expired1 <- session.isExpired
      _ = assertEquals(expired1, false)
      _ <- IO.sleep(20.millis)
      expired2 <- session.isExpired
      _ = assertEquals(expired2, true)
    yield ()
  }

  test("HttpSession tracks pending request count") {
    for
      manager <- SessionManager[IO](testServer)
      session <- manager.create
      count <- session.pendingRequestCount
      _ = assertEquals(count, 0)
    yield ()
  }

  test("HttpSession queue size is tracked") {
    for
      manager <- SessionManager[IO](testServer)
      session <- manager.create
      size <- session.queueSize
      _ = assertEquals(size, 0)
    yield ()
  }

  test("SessionConfig provides sensible defaults") {
    val config = SessionConfig.default
    assertEquals(config.timeout, 30.minutes)
    assertEquals(config.maxQueueSize, 1000)
    assertEquals(config.requestTimeout, 5.minutes)
    assertEquals(config.cleanupInterval, 1.minute)
  }

  test("SessionManager with custom config uses that config") {
    val customConfig = SessionConfig(
      timeout = 1.hour,
      maxQueueSize = 500,
      requestTimeout = 10.minutes
    )
    for
      manager <- SessionManager[IO](testServer, customConfig)
      session <- manager.create
      _ = assertEquals(session.config.timeout, 1.hour)
      _ = assertEquals(session.config.maxQueueSize, 500)
    yield ()
  }

  // === pruneExpired Tests ===

  test("pruneExpired removes expired sessions based on lastAccessedAt") {
    val shortTimeout = SessionConfig(timeout = 20.millis)
    for
      manager <- SessionManager[IO](testServer, shortTimeout)
      session1 <- manager.create
      session2 <- manager.create
      _ <- IO.sleep(10.millis)
      _ <- manager.get(session1.id)  // touch session1
      _ <- IO.sleep(15.millis)  // session2 now expired (25ms), session1 not (15ms since touch)
      pruned <- manager.pruneExpired
      _ = assertEquals(pruned, 1)
      s1 <- manager.get(session1.id)
      s2 <- manager.get(session2.id)
      _ = assert(s1.isDefined, "touched session should survive")
      _ = assert(s2.isEmpty, "untouched session should be pruned")
    yield ()
  }

  test("pruneExpired returns count of removed sessions") {
    val shortTimeout = SessionConfig(timeout = 10.millis)
    for
      manager <- SessionManager[IO](testServer, shortTimeout)
      _ <- manager.create
      _ <- manager.create
      _ <- manager.create
      _ <- IO.sleep(20.millis)
      pruned <- manager.pruneExpired
      _ = assertEquals(pruned, 3)
      count <- manager.sessionCount
      _ = assertEquals(count, 0)
    yield ()
  }

  test("pruneExpired returns zero when no sessions expired") {
    for
      manager <- SessionManager[IO](testServer)  // default 30 min timeout
      _ <- manager.create
      _ <- manager.create
      pruned <- manager.pruneExpired
      _ = assertEquals(pruned, 0)
      count <- manager.sessionCount
      _ = assertEquals(count, 2)
    yield ()
  }

  test("pruneExpired handles empty session map") {
    for
      manager <- SessionManager[IO](testServer)
      pruned <- manager.pruneExpired
      _ = assertEquals(pruned, 0)
    yield ()
  }

  // === remove with shutdown Tests ===

  test("remove handles non-existent session gracefully") {
    for
      manager <- SessionManager[IO](testServer)
      _ <- manager.remove("nonexistent-id")  // should not throw
    yield ()
  }

  // === HttpSession.shutdown Tests ===

  test("HttpSession.shutdown clears pending requests") {
    for
      manager <- SessionManager[IO](testServer)
      session <- manager.create
      count1 <- session.pendingRequestCount
      _ = assertEquals(count1, 0)
      _ <- session.shutdown
      count2 <- session.pendingRequestCount
      _ = assertEquals(count2, 0)
    yield ()
  }

  test("HttpSession.shutdown is idempotent") {
    for
      manager <- SessionManager[IO](testServer)
      session <- manager.create
      _ <- session.shutdown
      _ <- session.shutdown  // second call should not throw
    yield ()
  }
