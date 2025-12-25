package assignment3

/**
 * Tiny immutable wrapper for generated SaM code. Designed for safe composition
 * without sprinkling raw Strings everywhere. Use `+` to concatenate and
 * call `.toString` to materialize at boundaries.
 */
final case class Code private (private val s: String) {
  inline def +(other: Code): Code = Code(s + other.s)
  inline def +(other: String): Code = Code(s + other)
  override def toString: String = s
}

object Code {
  val empty: Code = Code("")
  inline def fromString(s: String): Code = Code(s)
  inline def from(sb: SamBuilder): Code = Code(sb.toString)
  // Convenience factories for common single-value patterns
  def pushInt(n: Int): Code = from(new SamBuilder().pushImmInt(n))
  def pushBool(b: Boolean): Code = from(new SamBuilder().pushBool(b))
  def pushNull: Code = from(new SamBuilder().pushNull())
  def returnSlot: Code = from(new SamBuilder().returnSlot())
}
