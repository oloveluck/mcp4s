package mcp4s.client.retry

import cats.effect.IO
import cats.syntax.all.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

class CircuitBreakerSpec extends CatsEffectSuite:

  // === Basic Circuit Breaker Tests ===

  test("CircuitBreaker starts in Closed state") {
    val config = CircuitBreakerConfig(failureThreshold = 3)
    for
      cb <- CircuitBreaker[IO](config)
      state <- cb.state
      _ = assertEquals(state, CircuitState.Closed)
    yield ()
  }

  test("CircuitBreaker allows successful operations") {
    val config = CircuitBreakerConfig(failureThreshold = 3)
    for
      cb <- CircuitBreaker[IO](config)
      result <- cb.protect(IO.pure("success"))
      _ = assertEquals(result, "success")
      state <- cb.state
      _ = assertEquals(state, CircuitState.Closed)
    yield ()
  }

  test("CircuitBreaker opens after threshold failures") {
    val config = CircuitBreakerConfig(failureThreshold = 3)
    for
      cb <- CircuitBreaker[IO](config)
      // Cause 3 failures
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException("fail1"))).attempt
      state1 <- cb.state
      _ = assertEquals(state1, CircuitState.Closed)

      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException("fail2"))).attempt
      state2 <- cb.state
      _ = assertEquals(state2, CircuitState.Closed)

      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException("fail3"))).attempt
      state3 <- cb.state
      _ = assertEquals(state3, CircuitState.Open)
    yield ()
  }

  test("CircuitBreaker fails fast when Open") {
    val config = CircuitBreakerConfig(failureThreshold = 1)
    for
      cb <- CircuitBreaker[IO](config)
      // Trigger open
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException("fail"))).attempt
      state <- cb.state
      _ = assertEquals(state, CircuitState.Open)

      // Should fail fast without executing the operation
      executed <- IO.ref(false)
      result <- cb.protect(executed.set(true) *> IO.pure("success")).attempt
      wasExecuted <- executed.get
      _ = assert(!wasExecuted, "Operation should not have been executed")
      _ = assert(result.isLeft)
      _ = assert(result.left.exists(_.isInstanceOf[CircuitBreakerOpenException]))
    yield ()
  }

  test("CircuitBreaker transitions to HalfOpen after resetTimeout") {
    val config = CircuitBreakerConfig(
      failureThreshold = 1,
      resetTimeout = 50.millis
    )
    for
      cb <- CircuitBreaker[IO](config)
      // Trigger open
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException("fail"))).attempt
      state1 <- cb.state
      _ = assertEquals(state1, CircuitState.Open)

      // Wait for reset timeout
      _ <- IO.sleep(100.millis)

      // Next operation should transition to HalfOpen and be allowed
      result <- cb.protect(IO.pure("success"))
      _ = assertEquals(result, "success")
    yield ()
  }

  test("CircuitBreaker closes after success threshold in HalfOpen") {
    val config = CircuitBreakerConfig(
      failureThreshold = 1,
      resetTimeout = 50.millis,
      successThreshold = 2
    )
    for
      cb <- CircuitBreaker[IO](config)
      // Trigger open
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException("fail"))).attempt
      _ <- IO.sleep(100.millis)

      // First success - still half-open
      _ <- cb.protect(IO.pure("s1"))
      state1 <- cb.state
      _ = assertEquals(state1, CircuitState.HalfOpen)

      // Second success - should close
      _ <- cb.protect(IO.pure("s2"))
      state2 <- cb.state
      _ = assertEquals(state2, CircuitState.Closed)
    yield ()
  }

  test("CircuitBreaker returns to Open on HalfOpen failure") {
    val config = CircuitBreakerConfig(
      failureThreshold = 1,
      resetTimeout = 50.millis,
      successThreshold = 2
    )
    for
      cb <- CircuitBreaker[IO](config)
      // Trigger open
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException("fail1"))).attempt
      _ <- IO.sleep(100.millis)

      // Fail in half-open state
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException("fail2"))).attempt
      state <- cb.state
      _ = assertEquals(state, CircuitState.Open)
    yield ()
  }

  test("CircuitBreaker resets failure count on success") {
    val config = CircuitBreakerConfig(failureThreshold = 3)
    for
      cb <- CircuitBreaker[IO](config)
      // 2 failures
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException("f1"))).attempt
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException("f2"))).attempt
      count1 <- cb.failureCount
      _ = assertEquals(count1, 2)

      // Success resets count
      _ <- cb.protect(IO.pure("success"))
      count2 <- cb.failureCount
      _ = assertEquals(count2, 0)

      // State should still be closed
      state <- cb.state
      _ = assertEquals(state, CircuitState.Closed)
    yield ()
  }

  // === Force Open/Close Tests ===

  test("forceOpen opens the circuit") {
    val config = CircuitBreakerConfig(failureThreshold = 5)
    for
      cb <- CircuitBreaker[IO](config)
      _ <- cb.forceOpen
      state <- cb.state
      _ = assertEquals(state, CircuitState.Open)
    yield ()
  }

  test("forceClose closes the circuit and resets state") {
    val config = CircuitBreakerConfig(failureThreshold = 1)
    for
      cb <- CircuitBreaker[IO](config)
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException())).attempt
      state1 <- cb.state
      _ = assertEquals(state1, CircuitState.Open)

      _ <- cb.forceClose
      state2 <- cb.state
      count <- cb.failureCount
      _ = assertEquals(state2, CircuitState.Closed)
      _ = assertEquals(count, 0)
    yield ()
  }

  // === Custom isFailure Tests ===

  test("CircuitBreaker respects custom isFailure predicate") {
    val config = CircuitBreakerConfig(
      failureThreshold = 1,
      isFailure = {
        case _: IllegalArgumentException => false // Don't count as failure
        case _                           => true
      }
    )
    for
      cb <- CircuitBreaker[IO](config)
      // IllegalArgumentException should not trip the circuit
      _ <- cb.protect(IO.raiseError[Unit](new IllegalArgumentException())).attempt
      state1 <- cb.state
      _ = assertEquals(state1, CircuitState.Closed)

      // Other exceptions should
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException())).attempt
      state2 <- cb.state
      _ = assertEquals(state2, CircuitState.Open)
    yield ()
  }

  // === CircuitBreakerStats Tests ===

  test("stats returns initial state with zero counters") {
    for
      cb <- CircuitBreaker[IO]()
      s <- cb.stats
      _ = assertEquals(s.state, CircuitState.Closed)
      _ = assertEquals(s.failures, 0)
      _ = assertEquals(s.successes, 0)
      _ = assertEquals(s.totalRequests, 0L)
      _ = assertEquals(s.rejectedRequests, 0L)
    yield ()
  }

  test("stats.totalRequests increments on each protected call") {
    for
      cb <- CircuitBreaker[IO]()
      _ <- cb.protect(IO.pure(1))
      _ <- cb.protect(IO.pure(2))
      _ <- cb.protect(IO.pure(3))
      s <- cb.stats
      _ = assertEquals(s.totalRequests, 3L)
    yield ()
  }

  test("stats.totalRequests increments on failures too") {
    for
      cb <- CircuitBreaker[IO](CircuitBreakerConfig(failureThreshold = 5))
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException())).attempt
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException())).attempt
      s <- cb.stats
      _ = assertEquals(s.totalRequests, 2L)
      _ = assertEquals(s.failures, 2)
    yield ()
  }

  test("stats.rejectedRequests increments when circuit is Open") {
    for
      cb <- CircuitBreaker[IO](CircuitBreakerConfig(failureThreshold = 1))
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException())).attempt
      _ <- cb.protect(IO.pure(1)).attempt  // rejected
      _ <- cb.protect(IO.pure(2)).attempt  // rejected
      _ <- cb.protect(IO.pure(3)).attempt  // rejected
      s <- cb.stats
      _ = assertEquals(s.rejectedRequests, 3L)
    yield ()
  }

  test("stats.totalRequests does not increment for rejected requests") {
    for
      cb <- CircuitBreaker[IO](CircuitBreakerConfig(failureThreshold = 1))
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException())).attempt
      s1 <- cb.stats
      _ <- cb.protect(IO.pure(1)).attempt  // rejected
      s2 <- cb.stats
      _ = assertEquals(s1.totalRequests, s2.totalRequests)  // unchanged
    yield ()
  }

  test("stats preserves counters after forceClose") {
    for
      cb <- CircuitBreaker[IO](CircuitBreakerConfig(failureThreshold = 1))
      _ <- cb.protect(IO.pure(1))
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException())).attempt
      _ <- cb.protect(IO.pure(2)).attempt  // rejected
      _ <- cb.forceClose
      s <- cb.stats
      _ = assertEquals(s.state, CircuitState.Closed)
      _ = assertEquals(s.totalRequests, 2L)  // preserved
      _ = assertEquals(s.rejectedRequests, 1L)  // preserved
    yield ()
  }

  test("stats.successes tracks HalfOpen successes") {
    for
      cb <- CircuitBreaker[IO](CircuitBreakerConfig(
        failureThreshold = 1,
        resetTimeout = 10.millis,
        successThreshold = 3
      ))
      _ <- cb.protect(IO.raiseError[Unit](new RuntimeException())).attempt
      _ <- IO.sleep(20.millis)
      _ <- cb.protect(IO.pure(1))  // HalfOpen, success 1
      s <- cb.stats
      _ = assertEquals(s.state, CircuitState.HalfOpen)
      _ = assertEquals(s.successes, 1)
    yield ()
  }

  test("stats under concurrent access") {
    for
      cb <- CircuitBreaker[IO]()
      _ <- (1 to 100).toList.parTraverse(_ => cb.protect(IO.pure(1)))
      s <- cb.stats
      _ = assertEquals(s.totalRequests, 100L)
    yield ()
  }
