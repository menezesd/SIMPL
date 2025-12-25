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
    val unknown: SourceLocation = SourceLocation(-1, -1)
    def fromLine(line: Int): SourceLocation = SourceLocation(line, -1)

    /** Create from tokenizer position */
    def from(tz: edu.utexas.cs.sam.io.SamTokenizer): SourceLocation =
      SourceLocation(tz.lineNo(), TokenizerOps.column(tz))
}
