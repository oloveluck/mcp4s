package mcp4s.client.resilient

import mcp4s.client.retry.{CircuitBreakerConfig, RetryPolicy}
import munit.CatsEffectSuite

import scala.concurrent.duration.*

class ResilientConnectionSpec extends CatsEffectSuite:

  // === ResilienceConfigBuilder Tests ===

  test("builder creates default configuration") {
    val config = ResilienceConfig.builder.build
    assertEquals(config.retryPolicy.maxRetries, 3)
    assert(config.circuitBreaker.isDefined)
    assertEquals(config.timeout, Some(30.seconds))
  }

  test("builder.withRetry sets retry policy") {
    val policy = RetryPolicy.fixedDelay(maxRetries = 10, delay = 500.millis)
    val config = ResilienceConfig.builder.withRetry(policy).build
    assertEquals(config.retryPolicy.maxRetries, 10)
  }

  test("builder.withoutRetry disables retry") {
    val config = ResilienceConfig.builder.withoutRetry.build
    assertEquals(config.retryPolicy.maxRetries, 0)
  }

  test("builder.withCircuitBreaker sets circuit breaker config") {
    val cbConfig = CircuitBreakerConfig(failureThreshold = 10, resetTimeout = 1.minute)
    val config = ResilienceConfig.builder.withCircuitBreaker(cbConfig).build
    assertEquals(config.circuitBreaker.map(_.failureThreshold), Some(10))
    assertEquals(config.circuitBreaker.map(_.resetTimeout), Some(1.minute))
  }

  test("builder.withoutCircuitBreaker disables circuit breaker") {
    val config = ResilienceConfig.builder.withoutCircuitBreaker.build
    assertEquals(config.circuitBreaker, None)
  }

  test("builder.withTimeout sets timeout") {
    val config = ResilienceConfig.builder.withTimeout(45.seconds).build
    assertEquals(config.timeout, Some(45.seconds))
  }

  test("builder.withoutTimeout disables timeout") {
    val config = ResilienceConfig.builder.withoutTimeout.build
    assertEquals(config.timeout, None)
  }

  test("builder supports chained configuration") {
    val config = ResilienceConfig.builder
      .withRetry(RetryPolicy.linearBackoff(maxRetries = 5))
      .withCircuitBreaker(CircuitBreakerConfig(failureThreshold = 3))
      .withTimeout(20.seconds)
      .build

    assertEquals(config.retryPolicy.maxRetries, 5)
    assertEquals(config.circuitBreaker.map(_.failureThreshold), Some(3))
    assertEquals(config.timeout, Some(20.seconds))
  }

  test("builder.from preserves existing configuration") {
    val original = ResilienceConfig(
      retryPolicy = RetryPolicy.fixedDelay(maxRetries = 7),
      circuitBreaker = Some(CircuitBreakerConfig(failureThreshold = 2)),
      timeout = Some(15.seconds)
    )
    val config = ResilienceConfigBuilder.from(original).build

    assertEquals(config.retryPolicy.maxRetries, 7)
    assertEquals(config.circuitBreaker.map(_.failureThreshold), Some(2))
    assertEquals(config.timeout, Some(15.seconds))
  }

  test("builder.from allows modification of existing configuration") {
    val original = ResilienceConfig.default
    val config = ResilienceConfigBuilder.from(original)
      .withTimeout(60.seconds)
      .withoutCircuitBreaker
      .build

    // Retry preserved from default
    assertEquals(config.retryPolicy.maxRetries, 3)
    // Modified values
    assertEquals(config.timeout, Some(60.seconds))
    assertEquals(config.circuitBreaker, None)
  }

  test("builder is immutable") {
    val builder1 = ResilienceConfig.builder
    val builder2 = builder1.withTimeout(10.seconds)
    val builder3 = builder1.withTimeout(20.seconds)

    assertEquals(builder2.build.timeout, Some(10.seconds))
    assertEquals(builder3.build.timeout, Some(20.seconds))
    // Original unchanged
    assertEquals(builder1.build.timeout, Some(30.seconds))
  }

  // === ResilienceConfig Factory Method Tests ===

  test("ResilienceConfig.default has all features enabled") {
    val config = ResilienceConfig.default
    assert(config.retryPolicy.maxRetries > 0)
    assert(config.circuitBreaker.isDefined)
    assert(config.timeout.isDefined)
  }

  test("ResilienceConfig.retryOnly disables other features") {
    val policy = RetryPolicy.exponentialBackoff(maxRetries = 5)
    val config = ResilienceConfig.retryOnly(policy)

    assertEquals(config.retryPolicy.maxRetries, 5)
    assertEquals(config.circuitBreaker, None)
    assertEquals(config.timeout, None)
  }

  test("ResilienceConfig.timeoutOnly disables other features") {
    val config = ResilienceConfig.timeoutOnly(10.seconds)

    assertEquals(config.retryPolicy.maxRetries, 0)
    assertEquals(config.circuitBreaker, None)
    assertEquals(config.timeout, Some(10.seconds))
  }
