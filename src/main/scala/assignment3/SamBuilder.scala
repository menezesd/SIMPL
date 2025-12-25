package assignment3

import assignment3.Offsets.{FieldOffset, StackOffset}

final class SamBuilder {
  private val sb = new StringBuilder

  // Helper to extract label name from union type
  private inline def labelName(lbl: String | Label): String = lbl match
    case s: String => s
    case l: Label  => l.name

  def append(s: String): SamBuilder = { sb.append(Option(s).getOrElse("")); this }
  def append(c: Code): SamBuilder = append(Option(c).map(_.toString).getOrElse(""))
  def line(s: String): SamBuilder = { sb.append(Option(s).getOrElse("")).append('\n'); this }

  // Label definition (union type: accepts String or Label)
  def label(lbl: String | Label): SamBuilder =
    sb.append(labelName(lbl)).append(':').append('\n'); this

  def instr(op: String): SamBuilder = line(op)
  def instr(op: String, arg: Any): SamBuilder = { sb.append(op).append(' ').append(String.valueOf(arg)).append('\n'); this }

  // Jump instructions (union type: accepts String or Label)
  def jsr(lbl: String | Label): SamBuilder =
    sb.append("JSR ").append(labelName(lbl)).append('\n'); this

  def jump(lbl: String | Label): SamBuilder =
    sb.append("JUMP ").append(labelName(lbl)).append('\n'); this

  def jumpc(lbl: String | Label): SamBuilder =
    sb.append("JUMPC ").append(labelName(lbl)).append('\n'); this

  // Alias: clearer intent when TOS is a boolean (1 = true, 0 = false)
  def branchIfTruthy(lbl: String | Label): SamBuilder = jumpc(lbl)

  // Micro-helpers for common patterns
  def jumpIfNil(lbl: String | Label): SamBuilder = { instr("ISNIL"); jumpc(lbl); this }

  def linkCall(lbl: String | Label): SamBuilder = { instr("LINK"); jsr(lbl); instr("UNLINK"); this }

  // Higher-level call helper: handles LINK/JSR/UNLINK + arg cleanup and optional return-slot drop
  def call(lbl: String | Label, argCount: Int, returns: Boolean): SamBuilder = {
    linkCall(lbl)
    addSp(-argCount)
    if (!returns) addSp(-1)
    this
  }
  // Common instruction helpers
  // Typed immediates (keep generic for compatibility)
  def pushImm(v: Any): SamBuilder = instr("PUSHIMM", v)
  def pushImmInt(n: Int): SamBuilder = instr("PUSHIMM", n)
  def pushImmStr(s: String): SamBuilder = instr("PUSHIMMSTR", s)
  // Semantic helpers for common patterns
  def pushNull(): SamBuilder = pushImmInt(0)
  def pushBool(b: Boolean): SamBuilder = pushImmInt(if b then 1 else 0)
  def returnSlot(): SamBuilder = pushImmInt(0) // Placeholder for return value
  def pushImmCh(c: Char): SamBuilder = {
    // Emit single-quoted char with escapes understood by SaM (e.g., '\\0', '\\n', '\\t', '\\'', '\\\\')
    val esc = c match {
      case '\\' => "\\\\"
      case '\''  => "\\'"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case '\u0000' => "\\0"
      case other => other.toString
    }
    instr("PUSHIMMCH", s"'${esc}'")
  }
  def pushOff(off: Int): SamBuilder = instr("PUSHOFF", off)
  def storeOff(off: Int): SamBuilder = instr("STOREOFF", off)
  // Offset-typed variants (opaque types)
  def pushOffS(off: StackOffset): SamBuilder = instr("PUSHOFF", off.value)
  def storeOffS(off: StackOffset): SamBuilder = instr("STOREOFF", off.value)
  def pushFieldOff(off: FieldOffset): SamBuilder = pushImm(off.value)
  def addFieldOff(off: FieldOffset): SamBuilder = { pushFieldOff(off); add(); this }
  def pushInd(): SamBuilder = instr("PUSHIND")
  def storeInd(): SamBuilder = instr("STOREIND")
  def addSp(n: Int): SamBuilder = instr("ADDSP", n)
  def dup(): SamBuilder = instr("DUP")
  def swap(): SamBuilder = instr("SWAP")
  def malloc(): SamBuilder = instr("MALLOC")
  // Arithmetic and logic helpers
  def add(): SamBuilder = instr("ADD")
  def sub(): SamBuilder = instr("SUB")
  def mul(): SamBuilder = instr("TIMES")
  def div(): SamBuilder = instr("DIV")
  def mod(): SamBuilder = instr("MOD")
  def and(): SamBuilder = instr("AND")
  def or(): SamBuilder = instr("OR")
  // Aliases for clarity (bitwise context)
  inline def andB(): SamBuilder = and()
  inline def orB(): SamBuilder = or()
  def isNil(): SamBuilder = instr("ISNIL")
  def isNeg(): SamBuilder = instr("ISNEG")
  def cmp(): SamBuilder = instr("CMP")
  def greater(): SamBuilder = instr("GREATER")
  def less(): SamBuilder = instr("LESS")
  def equal(): SamBuilder = instr("EQUAL")
  def link(): SamBuilder = instr("LINK")
  def unlink(): SamBuilder = instr("UNLINK")
  def rst(): SamBuilder = instr("RST")
  def stop(): SamBuilder = instr("STOP")
  override def toString: String = sb.toString
}
