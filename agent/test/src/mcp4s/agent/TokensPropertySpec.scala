package mcp4s.agent

import cats.kernel.{Monoid, Order}
import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class TokensPropertySpec extends ScalaCheckSuite:

  private val genTokens: Gen[Tokens] = Gen.choose(0L, 100000L).map(Tokens(_))

  // === Monoid laws ===

  property("Monoid associativity: (a |+| b) |+| c == a |+| (b |+| c)") {
    forAll(genTokens, genTokens, genTokens) { (a, b, c) =>
      assertEquals(
        (a |+| b) |+| c,
        a |+| (b |+| c)
      )
    }
  }

  property("Monoid left identity: empty |+| a == a") {
    forAll(genTokens) { a =>
      assertEquals(Monoid[Tokens].empty |+| a, a)
    }
  }

  property("Monoid right identity: a |+| empty == a") {
    forAll(genTokens) { a =>
      assertEquals(a |+| Monoid[Tokens].empty, a)
    }
  }

  property("Monoid commutativity: a |+| b == b |+| a") {
    forAll(genTokens, genTokens) { (a, b) =>
      assertEquals(a |+| b, b |+| a)
    }
  }

  // === Order laws ===

  property("Order reflexivity: a <= a") {
    forAll(genTokens) { a =>
      assert(Order[Tokens].lteqv(a, a))
    }
  }

  property("Order transitivity: if a <= b and b <= c then a <= c") {
    forAll(genTokens, genTokens, genTokens) { (x, y, z) =>
      // Sort to guarantee a <= b <= c
      val sorted = List(x, y, z).sorted(Order[Tokens].toOrdering)
      val a = sorted(0)
      val b = sorted(1)
      val c = sorted(2)
      assert(Order[Tokens].lteqv(a, b))
      assert(Order[Tokens].lteqv(b, c))
      assert(Order[Tokens].lteqv(a, c))
    }
  }

  property("Order antisymmetry: if a <= b and b <= a then a == b") {
    forAll(genTokens, genTokens) { (a, b) =>
      if Order[Tokens].lteqv(a, b) && Order[Tokens].lteqv(b, a) then
        assertEquals(a, b)
    }
  }

  property("Order totality: a <= b or b <= a") {
    forAll(genTokens, genTokens) { (a, b) =>
      assert(Order[Tokens].lteqv(a, b) || Order[Tokens].lteqv(b, a))
    }
  }

  // === Domain properties ===

  property("Tokens.zero is minimum for non-negative tokens") {
    forAll(genTokens) { a =>
      assert(Order[Tokens].lteqv(Tokens.zero, a))
    }
  }

  property("combine monotonicity: if a <= b then a |+| c <= b |+| c") {
    forAll(genTokens, genTokens, genTokens) { (a, b, c) =>
      if Order[Tokens].lteqv(a, b) then
        assert(Order[Tokens].lteqv(a |+| c, b |+| c))
    }
  }
