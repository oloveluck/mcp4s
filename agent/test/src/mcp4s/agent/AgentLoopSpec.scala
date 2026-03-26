package mcp4s.agent

import cats.effect.IO
import cats.syntax.all.*
import munit.CatsEffectSuite

class AgentLoopSpec extends CatsEffectSuite:

  test("AgentLoop.apply creates loop from function") {
    val loop = AgentLoop[IO] { (msgs, _) =>
      IO.pure(msgs :+ Message.Assistant("done"))
    }
    loop.run(List(Message.User("hi")), _ => IO.unit).map { result =>
      assertEquals(result.size, 2)
      assertEquals(result.last, Message.Assistant("done"))
    }
  }

  test("Semigroup combines loops sequentially via |+|") {
    val first = AgentLoop[IO] { (msgs, _) =>
      IO.pure(msgs :+ Message.Assistant("first"))
    }
    val second = AgentLoop[IO] { (msgs, _) =>
      IO.pure(msgs :+ Message.Assistant("second"))
    }
    val combined = first |+| second
    combined.run(List(Message.User("start")), _ => IO.unit).map { result =>
      assertEquals(result.size, 3)
      assertEquals(result(1), Message.Assistant("first"))
      assertEquals(result(2), Message.Assistant("second"))
    }
  }

  test("Semigroup is associative") {
    val a = AgentLoop[IO] { (msgs, _) => IO.pure(msgs :+ Message.Assistant("a")) }
    val b = AgentLoop[IO] { (msgs, _) => IO.pure(msgs :+ Message.Assistant("b")) }
    val c = AgentLoop[IO] { (msgs, _) => IO.pure(msgs :+ Message.Assistant("c")) }
    val init = List(Message.User("start"))
    val emit: AgentEvent => IO[Unit] = _ => IO.unit
    for
      leftAssoc  <- ((a |+| b) |+| c).run(init, emit)
      rightAssoc <- (a |+| (b |+| c)).run(init, emit)
    yield assertEquals(leftAssoc, rightAssoc)
  }
