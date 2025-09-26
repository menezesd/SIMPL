
package assignment3

import assignment3.Offsets.{FieldOffset, StackOffset}

final class SamBuilder {
  private val sb = new StringBuilder
  def append(s: String): SamBuilder = { sb.append(Option(s).getOrElse("")); this }
  def append(c: Code): SamBuilder = append(Option(c).map(_.toString).getOrElse(""))
  def line(s: String): SamBuilder = { sb.append(Option(s).getOrElse("")).append('\n'); this }
  def label(name: String): SamBuilder = { sb.append(Option(name).getOrElse("")).append(':').append('\n'); this }
  // Convenience helpers
  def label(lbl: Label): SamBuilder = label(lbl.name)
  def instr(op: String): SamBuilder = line(op)
  def instr(op: String, arg: Any): SamBuilder = { sb.append(op).append(' ').append(String.valueOf(arg)).append('\n'); this }
  def jsr(label: String): SamBuilder = { sb.append("JSR ").append(Option(label).getOrElse("")).append('\n'); this }
  def jump(label: String): SamBuilder = { sb.append("JUMP ").append(Option(label).getOrElse("")).append('\n'); this }
  def jump(lbl: Label): SamBuilder = jump(lbl.name)
  def jumpc(label: String): SamBuilder = { sb.append("JUMPC ").append(Option(label).getOrElse("")).append('\n'); this }
  def jumpc(lbl: Label): SamBuilder = jumpc(lbl.name)
  // Alias: clearer intent when TOS is a boolean (1 = true, 0 = false)
  def branchIfTruthy(label: String): SamBuilder = jumpc(label)
  def branchIfTruthy(lbl: Label): SamBuilder = jumpc(lbl)
  // Micro-helpers for common patterns
  def jumpIfNil(label: String): SamBuilder = { instr("ISNIL"); jumpc(label); this }
  def jumpIfNil(lbl: Label): SamBuilder = jumpIfNil(lbl.name)
  def linkCall(label: String): SamBuilder = { instr("LINK"); jsr(label); instr("UNLINK"); this }
  def linkCall(lbl: Label): SamBuilder = linkCall(lbl.name)
  // Higher-level call helper: handles LINK/JSR/UNLINK + arg cleanup and optional return-slot drop
  def call(label: String, argCount: Int, returns: Boolean): SamBuilder = {
    linkCall(label)
    addSp(-argCount)
    if (!returns) addSp(-1)
    this
  }
  def call(lbl: Label, argCount: Int, returns: Boolean): SamBuilder = call(lbl.name, argCount, returns)
  // Common instruction helpers
  // Typed immediates (keep generic for compatibility)
  def pushImm(v: Any): SamBuilder = instr("PUSHIMM", v)
  def pushImmInt(n: Int): SamBuilder = instr("PUSHIMM", n)
  def pushImmStr(s: String): SamBuilder = instr("PUSHIMMSTR", s)
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
  def andB(): SamBuilder = instr("AND")
  def orB(): SamBuilder = instr("OR")
  def and(): SamBuilder = instr("AND")
  def or(): SamBuilder = instr("OR")
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
