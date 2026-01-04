package assignment3

import assignment3.Offsets.{FieldOffset, StackOffset}

/** Constants for SaM calling convention and stack management. */
object CallConvention:
  /** Cleanup for return slot when callee doesn't return a value. */
  val ReturnSlotCleanup = -1

  /** Calculate stack cleanup after call (pops arguments). */
  inline def argCleanup(count: Int): Int = -count

  /** Combined cleanup: arguments + return slot. */
  inline def voidCallCleanup(argCount: Int): Int = argCleanup(argCount) + ReturnSlotCleanup

final class SamBuilder {
  private val sb = new StringBuilder

  // Helper to extract label name from union type
  private inline def labelName(lbl: String | Label): String = lbl match
    case s: String => s
    case l: Label  => l.name

  // Null-safe append methods for Java interoperability.
  // Scala callers should use Code/String directly (never null).
  def append(s: String): SamBuilder = { if (s != null) sb.append(s); this }
  def append(c: Code): SamBuilder = { if (c != null) sb.append(c.toString); this }
  def line(s: String): SamBuilder = { if (s != null) sb.append(s); sb.append('\n'); this }

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
    addSp(CallConvention.argCleanup(argCount))
    if (!returns) addSp(CallConvention.ReturnSlotCleanup)
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
    instr("PUSHIMMCH", s"'${c.escapeForSam}'")
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

  /** Extension methods for common code generation patterns. */
  def emitNullCheck(onNull: Label | String): SamBuilder =
    instr("ISNIL").jumpc(onNull)

  def emitBooleanNot(): SamBuilder =
    pushImmInt(1).add().pushImmInt(2).mod()

  def emitZeroCheck(onZero: Label | String): SamBuilder =
    dup().jumpIfNil(onZero)

  override def toString: String = sb.toString
}

/** Extension method for character escaping in SaM assembly format. */
extension (c: Char)
  def escapeForSam: String = c match
    case '\\' => "\\\\"
    case '\'' => "\\'"
    case '\n' => "\\n"
    case '\t' => "\\t"
    case '\r' => "\\r"
    case '\u0000' => "\\0"
    case other => other.toString

/** Fluent API for building SAM code with functional style. */
object CodeBuilder:
  /** Build SAM code using a builder function. */
  inline def build(f: SamBuilder => Unit): Code =
    val sb = new SamBuilder()
    f(sb)
    Code.from(sb)

  /** Build SAM code and return as String. */
  inline def buildString(f: SamBuilder => Unit): String =
    val sb = new SamBuilder()
    f(sb)
    sb.toString

  /** Build SAM code from a sequence of operations. */
  inline def sequence(ops: (SamBuilder => Unit)*): Code =
    val sb = new SamBuilder()
    ops.foreach(_(sb))
    Code.from(sb)
