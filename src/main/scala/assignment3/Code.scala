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
}
