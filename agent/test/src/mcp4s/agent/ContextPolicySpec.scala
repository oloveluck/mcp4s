package mcp4s.agent

import cats.effect.{IO, Ref}
import io.circe.Json
import munit.CatsEffectSuite

class ContextPolicySpec extends CatsEffectSuite:

  // Fixed-cost estimator: each message costs exactly 10 tokens
  private val fixedEstimator = TokenEstimator[IO](_ => IO.pure(Tokens(10L)))

  test("slidingWindow keeps messages from end within budget") {
    val msgs = (1 to 5).map(i => Message.User(s"msg$i")).toList
    // Budget allows 25 tokens, each msg costs 10 -> fits 2
    val budget = TokenBudget(maxTokens = 25, reservedForResponse = 0)
    ContextPolicy.slidingWindow[IO].compact(msgs, budget, fixedEstimator).map { result =>
      assertEquals(result.size, 2)
      assertEquals(result, List(Message.User("msg4"), Message.User("msg5")))
    }
  }

  test("slidingWindow keeps all messages when within budget") {
    val msgs = List(Message.User("a"), Message.User("b"))
    val budget = TokenBudget(maxTokens = 100, reservedForResponse = 0)
    ContextPolicy.slidingWindow[IO].compact(msgs, budget, fixedEstimator).map { result =>
      assertEquals(result, msgs)
    }
  }

  test("keepSystemAndRecent preserves first message plus recent") {
    val msgs = (1 to 5).map(i => Message.User(s"msg$i")).toList
    // Budget 35 tokens, each msg 10 -> first msg (10) + room for 2 more (20) = 3 total
    val budget = TokenBudget(maxTokens = 35, reservedForResponse = 0)
    ContextPolicy.keepSystemAndRecent[IO].compact(msgs, budget, fixedEstimator).map { result =>
      assertEquals(result.size, 3)
      assertEquals(result.head, Message.User("msg1"))
      assertEquals(result(1), Message.User("msg4"))
      assertEquals(result(2), Message.User("msg5"))
    }
  }

  test("keepSystemAndRecent handles empty messages") {
    val budget = TokenBudget(maxTokens = 100, reservedForResponse = 0)
    ContextPolicy.keepSystemAndRecent[IO].compact(Nil, budget, fixedEstimator).map { result =>
      assertEquals(result, Nil)
    }
  }

  test("dropToolResults replaces older tool result content with [truncated]") {
    val msgs = List(
      Message.User("start"),
      Message.ToolResult("1", "tool1", Json.fromString("big result data")),
      Message.ToolResult("2", "tool2", Json.fromString("more data")),
      Message.User("recent1"),
      Message.User("recent2")
    )
    val budget = TokenBudget(maxTokens = 100, reservedForResponse = 0)
    ContextPolicy.dropToolResults[IO](keepRecent = 2).compact(msgs, budget, fixedEstimator).map { result =>
      assertEquals(result.size, 5)
      // First 3 (older) — tool results should be truncated
      assertEquals(result(1), Message.ToolResult("1", "tool1", Json.fromString("[truncated]")))
      assertEquals(result(2), Message.ToolResult("2", "tool2", Json.fromString("[truncated]")))
      // Last 2 (recent) — kept intact
      assertEquals(result(3), Message.User("recent1"))
      assertEquals(result(4), Message.User("recent2"))
    }
  }

  test("summarize calls LLM and replaces older messages with summary") {
    Ref.of[IO, Int](0).flatMap { callCount =>
      val mockLlm = LlmClient[IO] { _ =>
        callCount.update(_ + 1).as(LlmResponse.Text("This is a summary"))
      }
      val msgs = (1 to 8).map(i => Message.User(s"msg$i")).toList
      val budget = TokenBudget(maxTokens = 100, reservedForResponse = 0)
      val policy = ContextPolicy.summarize[IO](mockLlm, LlmConfig.default, keepRecent = 3)

      policy.compact(msgs, budget, fixedEstimator).flatMap { result =>
        callCount.get.map { count =>
          assertEquals(count, 1) // LLM was called once
          // Result should be 1 summary + 3 recent = 4 messages
          assertEquals(result.size, 4)
          assert(result.head.isInstanceOf[Message.Assistant])
          val summary = result.head.asInstanceOf[Message.Assistant]
          assert(summary.content.contains("summary"), s"Expected summary content, got: ${summary.content}")
          // Last 3 messages preserved
          assertEquals(result(1), Message.User("msg6"))
          assertEquals(result(2), Message.User("msg7"))
          assertEquals(result(3), Message.User("msg8"))
        }
      }
    }
  }

  test("summarize returns original messages when no older messages exist") {
    val mockLlm = LlmClient[IO](_ => IO.pure(LlmResponse.Text("summary")))
    val msgs = List(Message.User("msg1"), Message.User("msg2"))
    val budget = TokenBudget(maxTokens = 100, reservedForResponse = 0)
    val policy = ContextPolicy.summarize[IO](mockLlm, LlmConfig.default, keepRecent = 5)

    policy.compact(msgs, budget, fixedEstimator).map { result =>
      assertEquals(result, msgs)
    }
  }

  test("pipeline applies second policy only when first is insufficient") {
    Ref.of[IO, List[String]](Nil).flatMap { log =>
      val first: ContextPolicy[IO] = new ContextPolicy[IO]:
        def compact(messages: List[Message], budget: TokenBudget, estimator: TokenEstimator[IO]): IO[List[Message]] =
          log.update(_ :+ "first").as(messages) // does nothing

      val second: ContextPolicy[IO] = new ContextPolicy[IO]:
        def compact(messages: List[Message], budget: TokenBudget, estimator: TokenEstimator[IO]): IO[List[Message]] =
          log.update(_ :+ "second").as(messages.takeRight(2))

      val msgs = (1 to 5).map(i => Message.User(s"msg$i")).toList
      // Budget 25 tokens, 5 msgs at 10 each = 50, over budget
      val budget = TokenBudget(maxTokens = 25, reservedForResponse = 0)
      val policy = ContextPolicy.pipeline(first, second)

      policy.compact(msgs, budget, fixedEstimator).flatMap { result =>
        log.get.map { entries =>
          // Both policies should have been called
          assertEquals(entries, List("first", "second"))
          assertEquals(result.size, 2)
        }
      }
    }
  }

  test("pipeline skips second policy when first is sufficient") {
    Ref.of[IO, List[String]](Nil).flatMap { log =>
      val first: ContextPolicy[IO] = new ContextPolicy[IO]:
        def compact(messages: List[Message], budget: TokenBudget, estimator: TokenEstimator[IO]): IO[List[Message]] =
          log.update(_ :+ "first").as(messages.takeRight(2)) // reduces to 2 msgs = 20 tokens

      val second: ContextPolicy[IO] = new ContextPolicy[IO]:
        def compact(messages: List[Message], budget: TokenBudget, estimator: TokenEstimator[IO]): IO[List[Message]] =
          log.update(_ :+ "second").as(messages.takeRight(1))

      val msgs = (1 to 5).map(i => Message.User(s"msg$i")).toList
      // Budget 25 tokens — after first, 2 msgs * 10 = 20 <= 25
      val budget = TokenBudget(maxTokens = 25, reservedForResponse = 0)
      val policy = ContextPolicy.pipeline(first, second)

      policy.compact(msgs, budget, fixedEstimator).flatMap { result =>
        log.get.map { entries =>
          // Only first should have been called
          assertEquals(entries, List("first"))
          assertEquals(result.size, 2)
        }
      }
    }
  }
