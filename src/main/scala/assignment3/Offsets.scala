package assignment3

/**
 * Type-safe offset wrappers for stack and field addressing.
 *
 * ## Stack Frame Layout (SaM calling convention)
 *
 * When a method is called, the stack grows as follows:
 * {{{
 * Higher addresses (earlier pushes)
 * ┌───────────────────────────────┐
 * │  Return value slot (RV)      │  ← Pushed by caller before args
 * ├───────────────────────────────┤
 * │  'this' (implicit arg 0)     │  ← Receiver object reference
 * ├───────────────────────────────┤
 * │  Argument 1                  │
 * │  ...                         │
 * │  Argument N                  │
 * ├───────────────────────────────┤
 * │  Return address (via LINK)   │  ← FBR points here after LINK
 * ├───────────────────────────────┤
 * │  Local 0                     │  ← SP points here after ADDSP for locals
 * │  ...                         │
 * │  Local M-1                   │
 * └───────────────────────────────┘
 * Lower addresses (later pushes, top of stack)
 * }}}
 *
 * ## Offset Semantics
 * - Stack offsets are relative to FBR (Frame Base Register)
 * - Negative offsets: parameters ('this' at -1, arg1 at -2, etc.)
 * - Positive offsets: locals (local0 at 1, local1 at 2, etc.)
 * - Field offsets are relative to object base pointer (0-indexed)
 *
 * ## Usage
 * Use StackOffset for PUSHOFF/STOREOFF instructions (stack access).
 * Use FieldOffset for object field access (base + offset, then PUSHIND/STOREIND).
 */
object Offsets {
  opaque type StackOffset = Int
  object StackOffset {
    def apply(n: Int): StackOffset = n
    extension (s: StackOffset) inline def value: Int = s

    // Conventional slots for readability; these are relative to FBR within a frame
    inline def returnSlot: StackOffset = 0      // Return value slot (above 'this')
    inline def thisSlot: StackOffset = -1       // Implicit receiver ('this')

    // Stack frame layout constants
    /** Base offset for local variables (after return address slot at 0). */
    inline def FirstLocalBase: Int = 2

    // Common argument and local slot helpers
    /** Get argument slot (1-indexed: arg1 = -2, arg2 = -3, etc.) */
    inline def arg(n: Int): StackOffset = -(n + 1)

    /** Get local variable slot (0-indexed: local0 = 1, local1 = 2, etc.) */
    inline def local(n: Int): StackOffset = n + 1

    /** Compute parameter offset for SaM calling convention. */
    inline def parameterOffset(index: Int, totalParams: Int): StackOffset = -(totalParams - index)

    /** Compute local variable offset for SaM calling convention. */
    inline def localOffset(index: Int): StackOffset = FirstLocalBase + index

    // Specific slot aliases for common patterns
    inline def firstArg: StackOffset = -2       // First explicit argument (after 'this')
    inline def secondArg: StackOffset = -3      // Second argument
    inline def thirdArg: StackOffset = -4       // Third argument
    inline def firstLocal: StackOffset = 1      // First local variable
    inline def secondLocal: StackOffset = 2     // Second local variable
    inline def thirdLocal: StackOffset = 3      // Third local variable
  }

  opaque type FieldOffset = Int
  object FieldOffset {
    def apply(n: Int): FieldOffset = n
    extension (f: FieldOffset) inline def value: Int = f
  }

  /**
   * Source location (line, column pair) for error reporting.
   * Replaces scattered (line: Int, column: Int) parameters.
   */
  final case class SourceLocation(line: Int, column: Int = -1):
    def format: String = if column >= 0 then s"$line:$column" else s"$line"

  object SourceLocation:
    /** Constant for unknown/unavailable source positions. */
    val UnknownLine = -1
    val UnknownColumn = -1

    val unknown: SourceLocation = SourceLocation(UnknownLine, UnknownColumn)
    def fromLine(line: Int): SourceLocation = SourceLocation(line, UnknownColumn)

    /** Create from tokenizer position */
    def from(tz: edu.utexas.cs.sam.io.SamTokenizer): SourceLocation =
      SourceLocation(tz.lineNo(), TokenizerOps.column(tz))
}
