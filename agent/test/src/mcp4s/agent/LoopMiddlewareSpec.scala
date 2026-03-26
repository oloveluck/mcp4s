package mcp4s.agent

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import munit.CatsEffectSuite

class LoopMiddlewareSpec extends CatsEffectSuite:

  private val noopEmit: AgentEvent => IO[Unit] = _ => IO.unit

  test("identity passes through") {
    val mw = LoopMiddleware.identity[IO]
    val msgs = List(Message.User("hello"))
    mw(msgs, noopEmit)(IO.pure(msgs :+ Message.Assistant("done"))).map { result =>
      assertEquals(result.size, 2)
      assertEquals(result.last, Message.Assistant("done"))
    }
  }

  test("logging middleware logs start and end") {
    Ref.of[IO, List[String]](Nil).flatMap { ref =>
      val mw = LoopMiddleware.logging[IO](msg => ref.update(_ :+ msg))
      val msgs = List(Message.User("hello"))
      val resultMsgs = msgs :+ Message.Assistant("done")
      mw(msgs, noopEmit)(IO.pure(resultMsgs)).flatMap { result =>
        ref.get.map { log =>
          assertEquals(result, resultMsgs)
          assertEquals(log.size, 2)
          assert(log.head.contains("starting"))
          assert(log.last.contains("finished"))
        }
      }
    }
  }

  test("catchErrors converts exceptions to Finished event") {
    Ref.of[IO, List[String]](Nil).flatMap { logRef =>
      Ref.of[IO, List[AgentEvent]](Nil).flatMap { eventRef =>
        val mw = LoopMiddleware.catchErrors[IO](msg => logRef.update(_ :+ msg))
        val msgs = List(Message.User("hello"))
        val emit: AgentEvent => IO[Unit] = event => eventRef.update(_ :+ event)
        mw(msgs, emit)(IO.raiseError(new RuntimeException("boom"))).flatMap { result =>
          for
            log <- logRef.get
            events <- eventRef.get
          yield
            // Should return original messages (error recovery)
            assertEquals(result, msgs)
            // Should have logged the error
            assert(log.exists(_.contains("boom")))
            // Should have emitted a Finished event
            val finished = events.collect { case f: AgentEvent.Finished => f }
            assertEquals(finished.size, 1)
            assert(finished.head.content.contains("boom"))
        }
      }
    }
  }

  test("Semigroup composes middlewares (ordering)") {
    Ref.of[IO, List[String]](Nil).flatMap { ref =>
      val mw1 = LoopMiddleware[IO] { (_, _, next) =>
        ref.update(_ :+ "outer-before") *> next.flatTap(_ => ref.update(_ :+ "outer-after"))
      }
      val mw2 = LoopMiddleware[IO] { (_, _, next) =>
        ref.update(_ :+ "inner-before") *> next.flatTap(_ => ref.update(_ :+ "inner-after"))
      }
      val combined = mw1 |+| mw2
      val msgs = List(Message.User("hello"))
      combined(msgs, noopEmit)(IO.pure(msgs)).flatMap { _ =>
        ref.get.map { log =>
          assertEquals(log, List("outer-before", "inner-before", "inner-after", "outer-after"))
        }
      }
    }
  }

  test("withMiddleware extension wraps AgentLoop") {
    Ref.of[IO, List[String]](Nil).flatMap { ref =>
      val loop = AgentLoop[IO] { (msgs, _) =>
        ref.update(_ :+ "loop").as(msgs :+ Message.Assistant("done"))
      }
      val mw = LoopMiddleware[IO] { (_, _, next) =>
        ref.update(_ :+ "before") *> next.flatTap(_ => ref.update(_ :+ "after"))
      }
      val wrapped = loop.withMiddleware(mw)
      wrapped.run(List(Message.User("hi")), noopEmit).flatMap { result =>
        ref.get.map { log =>
          assertEquals(log, List("before", "loop", "after"))
          assertEquals(result.last, Message.Assistant("done"))
        }
      }
    }
  }

  test("mapMessages post-processes result") {
    val loop = AgentLoop[IO] { (msgs, _) =>
      IO.pure(msgs :+ Message.Assistant("original"))
    }
    val mapped = loop.mapMessages { msgs =>
      IO.pure(msgs :+ Message.Assistant("appended"))
    }
    mapped.run(List(Message.User("hi")), noopEmit).map { result =>
      assertEquals(result.size, 3)
      assertEquals(result(1), Message.Assistant("original"))
      assertEquals(result(2), Message.Assistant("appended"))
    }
  }

  test("mapEvents transforms emitted events") {
    Ref.of[IO, List[AgentEvent]](Nil).flatMap { ref =>
      val loop = AgentLoop[IO] { (msgs, emit) =>
        emit(AgentEvent.Finished("done")).as(msgs)
      }
      val mapped = loop.mapEvents {
        case AgentEvent.Finished(content) => AgentEvent.Finished(content.toUpperCase)
        case other => other
      }
      val emit: AgentEvent => IO[Unit] = event => ref.update(_ :+ event)
      mapped.run(List(Message.User("hi")), emit).flatMap { _ =>
        ref.get.map { events =>
          assertEquals(events, List(AgentEvent.Finished("DONE")))
        }
      }
    }
  }

  test("observable middleware updates Ref before and after loop") {
    Ref.of[IO, Option[TurnView]](None).flatMap { viewRef =>
      Ref.of[IO, Option[TurnView]](None).flatMap { capturedBeforeRef =>
        val mw = LoopMiddleware.observable[IO](viewRef)
        val msgs = List(Message.User("hello"))
        val resultMsgs = msgs :+ Message.Assistant("done")
        val loop: IO[List[Message]] = viewRef.get.flatMap { snapshot =>
          // Capture the Ref value during loop execution
          capturedBeforeRef.set(snapshot).as(resultMsgs)
        }
        mw(msgs, noopEmit)(loop).flatMap { result =>
          for
            capturedBefore <- capturedBeforeRef.get
            afterView <- viewRef.get
          yield
            assertEquals(result, resultMsgs)
            // Before loop: Ref should have been set with input messages
            assertEquals(capturedBefore, Some(TurnView(msgs, 0)))
            // After loop: Ref should have result messages
            assertEquals(afterView, Some(TurnView(resultMsgs, -1)))
        }
      }
    }
  }
