package mcp4s.client.retry

import cats.effect.IO
import munit.CatsEffectSuite

import scala.concurrent.duration.*

class RetryPolicySpec extends CatsEffectSuite:

  // === RetryPolicy.exponentialBackoff Tests ===

  test("exponentialBackoff should retry up to maxRetries") {
    val policy = RetryPolicy.exponentialBackoff(maxRetries = 3)

    val r1 = policy.shouldRetry(new java.net.ConnectException(), 1)
    val r2 = policy.shouldRetry(new java.net.ConnectException(), 2)
    val r3 = policy.shouldRetry(new java.net.ConnectException(), 3)
    val r4 = policy.shouldRetry(new java.net.ConnectException(), 4)

    assert(r1, "Should retry on attempt 1")
    assert(r2, "Should retry on attempt 2")
    assert(r3, "Should retry on attempt 3")
    assert(!r4, "Should not retry on attempt 4")
  }

  test("exponentialBackoff delay should double with each attempt") {
    val policy = RetryPolicy.exponentialBackoff(
      baseDelay = 100.millis,
      maxDelay = 10.seconds,
      jitterFactor = 0.0 // Disable jitter for predictable tests
    )

    assertEquals(policy.delay(1), 100.millis)
    assertEquals(policy.delay(2), 200.millis)
    assertEquals(policy.delay(3), 400.millis)
    assertEquals(policy.delay(4), 800.millis)
  }

  test("exponentialBackoff delay should be capped at maxDelay") {
    val policy = RetryPolicy.exponentialBackoff(
      baseDelay = 100.millis,
      maxDelay = 500.millis,
      jitterFactor = 0.0
    )

    assertEquals(policy.delay(1), 100.millis)
    assertEquals(policy.delay(10), 500.millis) // Should be capped
  }

  test("exponentialBackoff should only retry on retryable errors") {
    val policy = RetryPolicy.exponentialBackoff(maxRetries = 3)

    // Retryable errors
    val r1 = policy.shouldRetry(new java.net.ConnectException(), 1)
    val r2 = policy.shouldRetry(new java.net.SocketTimeoutException(), 1)
    val r3 = policy.shouldRetry(new java.io.IOException("Connection reset"), 1)
    assert(r1, "ConnectException should be retryable")
    assert(r2, "SocketTimeoutException should be retryable")
    assert(r3, "IOException should be retryable")

    // Non-retryable errors
    val r4 = policy.shouldRetry(new IllegalArgumentException("invalid"), 1)
    val r5 = policy.shouldRetry(new RuntimeException("unknown"), 1)
    assert(!r4, "IllegalArgumentException should not be retryable")
    assert(!r5, "Generic RuntimeException should not be retryable")
  }

  // === RetryPolicy.noRetry Tests ===

  test("noRetry should never retry") {
    val policy = RetryPolicy.noRetry

    val r1 = policy.shouldRetry(new java.net.ConnectException(), 1)
    val r2 = policy.shouldRetry(new RuntimeException(), 1)
    assert(!r1)
    assert(!r2)
  }

  test("noRetry delay should be zero") {
    val policy = RetryPolicy.noRetry
    assertEquals(policy.delay(1), Duration.Zero)
  }

  // === RetryPolicy.fixedDelay Tests ===

  test("fixedDelay should use constant delay") {
    val policy = RetryPolicy.fixedDelay(delay = 500.millis)

    assertEquals(policy.delay(1), 500.millis)
    assertEquals(policy.delay(2), 500.millis)
    assertEquals(policy.delay(10), 500.millis)
  }

  // === RetryPolicy.linearBackoff Tests ===

  test("linearBackoff should increase delay linearly") {
    val policy = RetryPolicy.linearBackoff(
      initialDelay = 100.millis,
      increment = 50.millis,
      maxDelay = 1.second
    )

    assertEquals(policy.delay(1), 100.millis)
    assertEquals(policy.delay(2), 150.millis)
    assertEquals(policy.delay(3), 200.millis)
    assertEquals(policy.delay(4), 250.millis)
  }

  test("linearBackoff should be capped at maxDelay") {
    val policy = RetryPolicy.linearBackoff(
      initialDelay = 100.millis,
      increment = 100.millis,
      maxDelay = 250.millis
    )

    assertEquals(policy.delay(1), 100.millis)
    assertEquals(policy.delay(2), 200.millis)
    assertEquals(policy.delay(3), 250.millis) // Capped
    assertEquals(policy.delay(10), 250.millis) // Still capped
  }

  // === Retry Helper Tests ===

  test("Retry.apply should retry on failure") {
    val policy = RetryPolicy.exponentialBackoff(
      maxRetries = 3,
      baseDelay = 10.millis,
      jitterFactor = 0.0
    )

    for
      attemptsRef <- IO.ref(0)
      result <- Retry(policy) {
        attemptsRef.updateAndGet(_ + 1).flatMap { attempts =>
          if attempts < 3 then IO.raiseError(new java.net.ConnectException())
          else IO.pure("success")
        }
      }
      attempts <- attemptsRef.get
      _ = assertEquals(result, "success")
      _ = assertEquals(attempts, 3)
    yield ()
  }

  test("Retry.apply should fail after max retries") {
    val policy = RetryPolicy.exponentialBackoff(
      maxRetries = 2,
      baseDelay = 10.millis,
      jitterFactor = 0.0
    )

    for
      attemptsRef <- IO.ref(0)
      result <- Retry(policy) {
        attemptsRef.update(_ + 1) *>
          IO.raiseError[String](new java.net.ConnectException("always fail"))
      }.attempt
      attempts <- attemptsRef.get
      _ = assert(result.isLeft)
      _ = assertEquals(attempts, 3) // 1 initial + 2 retries
    yield ()
  }

  test("Retry.apply should not retry non-retryable errors") {
    val policy = RetryPolicy.exponentialBackoff(maxRetries = 3)

    for
      attemptsRef <- IO.ref(0)
      result <- Retry(policy) {
        attemptsRef.update(_ + 1) *>
          IO.raiseError[String](new IllegalArgumentException("invalid"))
      }.attempt
      attempts <- attemptsRef.get
      _ = assert(result.isLeft)
      _ = assertEquals(attempts, 1) // Should not retry
    yield ()
  }
