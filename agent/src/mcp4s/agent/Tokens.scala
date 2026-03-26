package mcp4s.agent

import cats.kernel.{Monoid, Order}

/** Type-safe wrapper for token counts.
  *
  * Follows the project's opaque type convention from `core/src/mcp4s/protocol/Mcp.scala`.
  */
opaque type Tokens = Long

object Tokens:
  def apply(value: Long): Tokens = value
  val zero: Tokens = 0L

  extension (tokens: Tokens)
    def value: Long = tokens
    def +(other: Tokens): Tokens = tokens + other
    def -(other: Tokens): Tokens = tokens - other
    def >(other: Tokens): Boolean = tokens > other
    def <=(other: Tokens): Boolean = tokens <= other

  given Monoid[Tokens] with
    def empty: Tokens = zero
    def combine(x: Tokens, y: Tokens): Tokens = x + y

  given Order[Tokens] with
    def compare(x: Tokens, y: Tokens): Int = java.lang.Long.compare(x.value, y.value)
