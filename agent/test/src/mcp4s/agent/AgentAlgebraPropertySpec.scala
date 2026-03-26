package mcp4s.agent

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import io.circe.Json
import mcp4s.protocol.*
import mcp4s.server.{McpPrompt, McpResource, Prompts, Resources, Tools}
import munit.CatsEffectSuite
import org.scalacheck.Gen

class AgentAlgebraPropertySpec extends CatsEffectSuite:

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  /** Run a property test `iterations` times using random values from `gen`. */
  private def checkIO[A](gen: Gen[A], iterations: Int = 50)(test: A => IO[Unit]): IO[Unit] =
    val params = Gen.Parameters.default
    IO.defer {
      (0 until iterations).toList.traverse_ { i =>
        val seed = org.scalacheck.rng.Seed(i.toLong)
        gen(params, seed) match
          case Some(a) => test(a)
          case None    => IO.unit // skip if gen fails
      }
    }

  /** Non-empty alpha-lower strings, small. */
  private val genTag: Gen[String] = Gen.alphaLowerStr.filter(_.nonEmpty).map(_.take(8))

  /** Small list of tags (1–4 elements). */
  private val genTags: Gen[List[String]] = Gen.choose(1, 4).flatMap(n => Gen.listOfN(n, genTag))

  // ---------------------------------------------------------------------------
  // TurnHook properties
  // ---------------------------------------------------------------------------

  private def taggedHook(tag: String, ref: Ref[IO, List[String]]): TurnHook[IO] =
    new TurnHook[IO]:
      def beforeTurn(view: TurnView, emit: AgentEvent => IO[Unit]): IO[List[Message]] =
        ref.update(_ :+ s"before-$tag").as(view.messages)
      def afterTurn(view: TurnView, emit: AgentEvent => IO[Unit]): IO[List[Message]] =
        ref.update(_ :+ s"after-$tag").as(view.messages)

  private val noopEmit: AgentEvent => IO[Unit] = _ => IO.unit
  private val emptyView: TurnView = TurnView(Nil, 0)

  test("TurnHook: semigroup associativity") {
    checkIO(genTags.filter(_.size >= 3)) { tags =>
      val List(t1, t2, t3) = tags.take(3): @unchecked
      for
        ref1 <- Ref.of[IO, List[String]](Nil)
        ref2 <- Ref.of[IO, List[String]](Nil)
        a = taggedHook(t1, ref1)
        b = taggedHook(t2, ref1)
        c = taggedHook(t3, ref1)
        _ <- ((a |+| b) |+| c).beforeTurn(emptyView, noopEmit)
        left <- ref1.get
        a2 = taggedHook(t1, ref2)
        b2 = taggedHook(t2, ref2)
        c2 = taggedHook(t3, ref2)
        _ <- (a2 |+| (b2 |+| c2)).beforeTurn(emptyView, noopEmit)
        right <- ref2.get
      yield assertEquals(left, right)
    }
  }

  test("TurnHook: left identity") {
    checkIO(genTag) { tag =>
      for
        ref1 <- Ref.of[IO, List[String]](Nil)
        ref2 <- Ref.of[IO, List[String]](Nil)
        a = taggedHook(tag, ref1)
        _ <- (TurnHook.identity[IO] |+| a).beforeTurn(emptyView, noopEmit)
        left <- ref1.get
        a2 = taggedHook(tag, ref2)
        _ <- a2.beforeTurn(emptyView, noopEmit)
        right <- ref2.get
      yield assertEquals(left, right)
    }
  }

  test("TurnHook: right identity") {
    checkIO(genTag) { tag =>
      for
        ref1 <- Ref.of[IO, List[String]](Nil)
        ref2 <- Ref.of[IO, List[String]](Nil)
        a = taggedHook(tag, ref1)
        _ <- (a |+| TurnHook.identity[IO]).beforeTurn(emptyView, noopEmit)
        left <- ref1.get
        a2 = taggedHook(tag, ref2)
        _ <- a2.beforeTurn(emptyView, noopEmit)
        right <- ref2.get
      yield assertEquals(left, right)
    }
  }

  test("TurnHook: composition order (left-to-right)") {
    for
      ref <- Ref.of[IO, List[String]](Nil)
      a = taggedHook("a", ref)
      b = taggedHook("b", ref)
      _ <- (a |+| b).beforeTurn(emptyView, noopEmit)
      markers <- ref.get
    yield assertEquals(markers, List("before-a", "before-b"))
  }

  test("TurnHook: afterTurn also chains left-to-right") {
    for
      ref <- Ref.of[IO, List[String]](Nil)
      a = taggedHook("a", ref)
      b = taggedHook("b", ref)
      _ <- (a |+| b).afterTurn(emptyView, noopEmit)
      markers <- ref.get
    yield assertEquals(markers, List("after-a", "after-b"))
  }

  // ---------------------------------------------------------------------------
  // LoopMiddleware properties
  // ---------------------------------------------------------------------------

  private def taggedMiddleware(tag: String, ref: Ref[IO, List[String]]): LoopMiddleware[IO] =
    new LoopMiddleware[IO]:
      def apply(messages: List[Message], emit: AgentEvent => IO[Unit])(next: => IO[List[Message]]): IO[List[Message]] =
        ref.update(_ :+ s"before-$tag") *> next <* ref.update(_ :+ s"after-$tag")

  test("LoopMiddleware: semigroup associativity") {
    checkIO(genTags.filter(_.size >= 3)) { tags =>
      val List(t1, t2, t3) = tags.take(3): @unchecked
      for
        ref1 <- Ref.of[IO, List[String]](Nil)
        ref2 <- Ref.of[IO, List[String]](Nil)
        a = taggedMiddleware(t1, ref1)
        b = taggedMiddleware(t2, ref1)
        c = taggedMiddleware(t3, ref1)
        _ <- ((a |+| b) |+| c)(Nil, noopEmit)(IO.pure(Nil))
        left <- ref1.get
        a2 = taggedMiddleware(t1, ref2)
        b2 = taggedMiddleware(t2, ref2)
        c2 = taggedMiddleware(t3, ref2)
        _ <- (a2 |+| (b2 |+| c2))(Nil, noopEmit)(IO.pure(Nil))
        right <- ref2.get
      yield assertEquals(left, right)
    }
  }

  test("LoopMiddleware: left identity") {
    checkIO(genTag) { tag =>
      for
        ref1 <- Ref.of[IO, List[String]](Nil)
        ref2 <- Ref.of[IO, List[String]](Nil)
        a = taggedMiddleware(tag, ref1)
        _ <- (LoopMiddleware.identity[IO] |+| a)(Nil, noopEmit)(IO.pure(Nil))
        left <- ref1.get
        a2 = taggedMiddleware(tag, ref2)
        _ <- a2(Nil, noopEmit)(IO.pure(Nil))
        right <- ref2.get
      yield assertEquals(left, right)
    }
  }

  test("LoopMiddleware: right identity") {
    checkIO(genTag) { tag =>
      for
        ref1 <- Ref.of[IO, List[String]](Nil)
        ref2 <- Ref.of[IO, List[String]](Nil)
        a = taggedMiddleware(tag, ref1)
        _ <- (a |+| LoopMiddleware.identity[IO])(Nil, noopEmit)(IO.pure(Nil))
        left <- ref1.get
        a2 = taggedMiddleware(tag, ref2)
        _ <- a2(Nil, noopEmit)(IO.pure(Nil))
        right <- ref2.get
      yield assertEquals(left, right)
    }
  }

  test("LoopMiddleware: nesting order (outer wraps inner)") {
    for
      ref <- Ref.of[IO, List[String]](Nil)
      a = taggedMiddleware("a", ref)
      b = taggedMiddleware("b", ref)
      _ <- (a |+| b)(Nil, noopEmit)(ref.update(_ :+ "inner").as(Nil))
      markers <- ref.get
    yield assertEquals(markers, List("before-a", "before-b", "inner", "after-b", "after-a"))
  }

  test("LoopMiddleware: identity passes through result unchanged") {
    val msgs = List(Message.User("hello"))
    for result <- LoopMiddleware.identity[IO](msgs, noopEmit)(IO.pure(msgs))
    yield assertEquals(result, msgs)
  }

  // ---------------------------------------------------------------------------
  // AgentLoop properties
  // ---------------------------------------------------------------------------

  private def taggedLoop(tag: String): AgentLoop[IO] =
    AgentLoop[IO] { (messages, _) =>
      IO.pure(messages :+ Message.Assistant(tag))
    }

  test("AgentLoop: semigroup associativity") {
    checkIO(genTags.filter(_.size >= 3)) { tags =>
      val List(t1, t2, t3) = tags.take(3): @unchecked
      val a = taggedLoop(t1)
      val b = taggedLoop(t2)
      val c = taggedLoop(t3)
      for
        left <- ((a |+| b) |+| c).run(Nil, noopEmit)
        right <- (a |+| (b |+| c)).run(Nil, noopEmit)
      yield assertEquals(left, right)
    }
  }

  test("AgentLoop: left-to-right ordering") {
    val a = taggedLoop("first")
    val b = taggedLoop("second")
    for result <- (a |+| b).run(Nil, noopEmit)
    yield
      assertEquals(result, List(
        Message.Assistant("first"),
        Message.Assistant("second")
      ))
  }

  test("AgentLoop: sequential output accumulates") {
    checkIO(genTags) { tags =>
      val loops = tags.map(taggedLoop)
      val combined = loops.reduceLeft(_ |+| _)
      for result <- combined.run(Nil, noopEmit)
      yield assert(result.nonEmpty)
    }
  }

  // ---------------------------------------------------------------------------
  // Tools properties
  // ---------------------------------------------------------------------------

  private def taggedTools(name: String): Tools[IO] =
    Tools.single[IO](Tool(name, Some(name), JsonSchema.empty)) { _ =>
      IO.pure(ToolResult.text(name))
    }

  test("Tools: semigroup associativity") {
    checkIO(genTags.filter(_.size >= 3)) { tags =>
      val List(t1, t2, t3) = tags.take(3): @unchecked
      val a = taggedTools(t1)
      val b = taggedTools(t2)
      val c = taggedTools(t3)
      for
        leftList <- ((a |+| b) |+| c).list
        rightList <- (a |+| (b |+| c)).list
        leftNames = leftList.map(_.name).toSet
        rightNames = rightList.map(_.name).toSet
        // All names from both associations should route the same
        allNames = leftNames ++ rightNames
        leftResults <- allNames.toList.traverse(n => ((a |+| b) |+| c).call(n, Json.obj()).value)
        rightResults <- allNames.toList.traverse(n => (a |+| (b |+| c)).call(n, Json.obj()).value)
      yield
        assertEquals(leftNames, rightNames)
        assertEquals(leftResults, rightResults)
    }
  }

  test("Tools: left identity with empty") {
    checkIO(genTag) { tag =>
      val a = taggedTools(tag)
      for
        leftList <- (Tools.empty[IO] |+| a).list
        rightList <- a.list
        leftResult <- (Tools.empty[IO] |+| a).call(tag, Json.obj()).value
        rightResult <- a.call(tag, Json.obj()).value
      yield
        assertEquals(leftList.map(_.name), rightList.map(_.name))
        assertEquals(leftResult, rightResult)
    }
  }

  test("Tools: right identity with empty") {
    checkIO(genTag) { tag =>
      val a = taggedTools(tag)
      for
        leftList <- (a |+| Tools.empty[IO]).list
        rightList <- a.list
        leftResult <- (a |+| Tools.empty[IO]).call(tag, Json.obj()).value
        rightResult <- a.call(tag, Json.obj()).value
      yield
        assertEquals(leftList.map(_.name), rightList.map(_.name))
        assertEquals(leftResult, rightResult)
    }
  }

  test("Tools: first-match-wins on duplicate names") {
    for
      result <- (taggedTools("dup") |+| taggedTools("dup")).call("dup", Json.obj()).value
      list <- (taggedTools("dup") |+| taggedTools("dup")).list
    yield
      // Both return same content since handler is identical, but list deduplicates
      assertEquals(list.count(_.name == "dup"), 1)
      assert(result.isDefined)
  }

  // ---------------------------------------------------------------------------
  // Resources properties
  // ---------------------------------------------------------------------------

  private def taggedResource(uri: String): Resources[IO] =
    McpResource[IO](uri, uri)("content-" + uri)

  test("Resources: semigroup associativity") {
    checkIO(genTags.filter(_.size >= 3)) { tags =>
      val List(t1, t2, t3) = tags.take(3): @unchecked
      val a = taggedResource(t1)
      val b = taggedResource(t2)
      val c = taggedResource(t3)
      for
        leftList <- ((a |+| b) |+| c).list
        rightList <- (a |+| (b |+| c)).list
        leftUris = leftList.map(_.uri).toSet
        rightUris = rightList.map(_.uri).toSet
        leftResults <- leftUris.toList.traverse(u => ((a |+| b) |+| c).read(u).value)
        rightResults <- rightUris.toList.sorted.traverse(u => (a |+| (b |+| c)).read(u).value)
        leftSorted <- leftUris.toList.sorted.traverse(u => ((a |+| b) |+| c).read(u).value)
      yield
        assertEquals(leftUris, rightUris)
        // Compare results for same URIs in same order
        assertEquals(leftSorted, rightResults)
    }
  }

  test("Resources: left identity with empty") {
    checkIO(genTag) { tag =>
      val a = taggedResource(tag)
      for
        leftList <- (Resources.empty[IO] |+| a).list
        rightList <- a.list
        leftResult <- (Resources.empty[IO] |+| a).read(tag).value
        rightResult <- a.read(tag).value
      yield
        assertEquals(leftList.map(_.uri), rightList.map(_.uri))
        assertEquals(leftResult, rightResult)
    }
  }

  test("Resources: right identity with empty") {
    checkIO(genTag) { tag =>
      val a = taggedResource(tag)
      for
        leftList <- (a |+| Resources.empty[IO]).list
        rightList <- a.list
        leftResult <- (a |+| Resources.empty[IO]).read(tag).value
        rightResult <- a.read(tag).value
      yield
        assertEquals(leftList.map(_.uri), rightList.map(_.uri))
        assertEquals(leftResult, rightResult)
    }
  }

  test("Resources: first-match-wins on duplicate URIs") {
    val r1 = McpResource[IO]("dup", "dup")("first")
    val r2 = McpResource[IO]("dup", "dup")("second")
    for
      result <- (r1 |+| r2).read("dup").value
      list <- (r1 |+| r2).list
    yield
      assertEquals(list.count(_.uri == "dup"), 1)
      assert(result.isDefined)
      assertEquals(result.get.text, Some("first"))
  }

  // ---------------------------------------------------------------------------
  // Prompts properties
  // ---------------------------------------------------------------------------

  private def taggedPrompt(name: String): Prompts[IO] =
    McpPrompt.noArgs[IO](name, name) {
      IO.pure(GetPromptResult(None, List(PromptMessage(Role.User, TextContent(name)))))
    }

  test("Prompts: semigroup associativity") {
    checkIO(genTags.filter(_.size >= 3)) { tags =>
      val List(t1, t2, t3) = tags.take(3): @unchecked
      val a = taggedPrompt(t1)
      val b = taggedPrompt(t2)
      val c = taggedPrompt(t3)
      for
        leftList <- ((a |+| b) |+| c).list
        rightList <- (a |+| (b |+| c)).list
        leftNames = leftList.map(_.name).toSet
        rightNames = rightList.map(_.name).toSet
        leftResults <- leftNames.toList.sorted.traverse(n => ((a |+| b) |+| c).get(n, Map.empty).value)
        rightResults <- rightNames.toList.sorted.traverse(n => (a |+| (b |+| c)).get(n, Map.empty).value)
      yield
        assertEquals(leftNames, rightNames)
        assertEquals(leftResults, rightResults)
    }
  }

  test("Prompts: left identity with empty") {
    checkIO(genTag) { tag =>
      val a = taggedPrompt(tag)
      for
        leftList <- (Prompts.empty[IO] |+| a).list
        rightList <- a.list
        leftResult <- (Prompts.empty[IO] |+| a).get(tag, Map.empty).value
        rightResult <- a.get(tag, Map.empty).value
      yield
        assertEquals(leftList.map(_.name), rightList.map(_.name))
        assertEquals(leftResult, rightResult)
    }
  }

  test("Prompts: right identity with empty") {
    checkIO(genTag) { tag =>
      val a = taggedPrompt(tag)
      for
        leftList <- (a |+| Prompts.empty[IO]).list
        rightList <- a.list
        leftResult <- (a |+| Prompts.empty[IO]).get(tag, Map.empty).value
        rightResult <- a.get(tag, Map.empty).value
      yield
        assertEquals(leftList.map(_.name), rightList.map(_.name))
        assertEquals(leftResult, rightResult)
    }
  }

  test("Prompts: first-match-wins on duplicate names") {
    val p1 = McpPrompt.noArgs[IO]("dup", "first") {
      IO.pure(GetPromptResult(Some("first"), List(PromptMessage(Role.User, TextContent("first")))))
    }
    val p2 = McpPrompt.noArgs[IO]("dup", "second") {
      IO.pure(GetPromptResult(Some("second"), List(PromptMessage(Role.User, TextContent("second")))))
    }
    for
      result <- (p1 |+| p2).get("dup", Map.empty).value
      list <- (p1 |+| p2).list
    yield
      assertEquals(list.count(_.name == "dup"), 1)
      assert(result.isDefined)
      assertEquals(result.get.description, Some("first"))
  }
