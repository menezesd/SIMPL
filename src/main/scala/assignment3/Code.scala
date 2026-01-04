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

object Code:
  val empty: Code = Code("")
  inline def fromString(s: String): Code = Code(s)
  inline def from(sb: SamBuilder): Code = Code(sb.toString)

  // Convenience factories for common single-value patterns
  def pushInt(n: Int): Code = from(new SamBuilder().pushImmInt(n))
  def pushBool(b: Boolean): Code = from(new SamBuilder().pushBool(b))
  def pushNull: Code = from(new SamBuilder().pushNull())
  def returnSlot: Code = from(new SamBuilder().returnSlot())

  /** Efficiently concatenate multiple Code fragments (avoids O(n²) string concatenation). */
  def concat(codes: Iterable[Code]): Code =
    val sb = new StringBuilder
    codes.foreach(c => sb.append(c.s))
    Code(sb.toString)

  /** Efficiently concatenate Code fragments with a prefix. */
  def concat(prefix: Code, codes: Iterable[Code]): Code =
    val sb = new StringBuilder
    sb.append(prefix.s)
    codes.foreach(c => sb.append(c.s))
    Code(sb.toString)

  /** Escape a string literal for SaM string instructions. */
  def escapeStringLiteral(s: String): String =
    val escaped = s.flatMap {
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case '\\' => "\\\\"
      case '\"' => "\\\""
      case c if c.isControl => f"\\u$c%04x"
      case c => c.toString
    }
    s"\"$escaped\""

  /** Extension methods for fluent Code composition. */
  extension (code: Code)
    /** Check if this code fragment is empty. */
    inline def isEmpty: Boolean = code.s.isEmpty

    /** Check if this code fragment is non-empty. */
    inline def nonEmpty: Boolean = code.s.nonEmpty

    /** Append a SamBuilder's output directly. */
    inline def ++(sb: SamBuilder): Code = Code(code.s + sb.toString)

    /** Prepend code to this fragment. */
    inline def prepend(other: Code): Code = Code(other.s + code.s)

    /** Apply a transformation if condition is true. */
    inline def when(cond: Boolean)(f: Code => Code): Code =
      if cond then f(code) else code

    /** Surround this code with before and after fragments. */
    inline def surroundWith(before: Code, after: Code): Code =
      Code(before.s + code.s + after.s)

    /** Repeat this code fragment n times. */
    def repeatN(n: Int): Code =
      if n <= 0 then Code.empty
      else Code.concat(List.fill(n)(code))
