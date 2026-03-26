package mcp4s.agent

import cats.kernel.{Monoid, Order}
import munit.CatsEffectSuite

class TokensSpec extends CatsEffectSuite:

  test("apply and value round-trip") {
    val t = Tokens(42L)
    assertEquals(t.value, 42L)
  }

  test("zero has value 0") {
    assertEquals(Tokens.zero.value, 0L)
  }

  test("addition") {
    val a = Tokens(10L)
    val b = Tokens(20L)
    assertEquals((a + b).value, 30L)
  }

  test("subtraction") {
    val a = Tokens(30L)
    val b = Tokens(10L)
    assertEquals((a - b).value, 20L)
  }

  test("greater than") {
    assert(Tokens(10L) > Tokens(5L))
    assert(!(Tokens(5L) > Tokens(10L)))
    assert(!(Tokens(5L) > Tokens(5L)))
  }

  test("less than or equal") {
    assert(Tokens(5L) <= Tokens(10L))
    assert(Tokens(5L) <= Tokens(5L))
    assert(!(Tokens(10L) <= Tokens(5L)))
  }

  test("Monoid.empty is zero") {
    val m = Monoid[Tokens]
    assertEquals(m.empty.value, 0L)
  }

  test("Monoid identity: zero + x == x") {
    val m = Monoid[Tokens]
    val x = Tokens(42L)
    assertEquals(m.combine(m.empty, x).value, x.value)
    assertEquals(m.combine(x, m.empty).value, x.value)
  }

  test("Monoid associativity") {
    val m = Monoid[Tokens]
    val a = Tokens(1L)
    val b = Tokens(2L)
    val c = Tokens(3L)
    assertEquals(
      m.combine(m.combine(a, b), c).value,
      m.combine(a, m.combine(b, c)).value
    )
  }

  test("Order is consistent with Long ordering") {
    val o = Order[Tokens]
    assert(o.compare(Tokens(1L), Tokens(2L)) < 0)
    assert(o.compare(Tokens(2L), Tokens(1L)) > 0)
    assertEquals(o.compare(Tokens(5L), Tokens(5L)), 0)
  }
