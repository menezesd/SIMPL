
package assignment3

import assignment3.symbol.ProgramSymbols

/**
 * Per-compilation context (Scala port).
 *
 * Usage pattern:
 * 1. Create context at start of compilation
 * 2. Build symbols (first pass) - get new context with symbols
 * 3. Parse and emit using symbols
 * 4. Cleanup recorder at end
 *
 * Note: TokenRecorder is inherently mutable for token accumulation.
 * Labeler is now an object (stateless utility).
 */
final class CompilerContext private (
  private val _symbols: Option[ProgramSymbols],
  val recorder: TokenRecorder
) {
  def symbols: Option[ProgramSymbols] = _symbols

  def withSymbols(s: ProgramSymbols): CompilerContext =
    new CompilerContext(Some(s), recorder)
}

object CompilerContext {
  def apply(): CompilerContext =
    new CompilerContext(None, new ListTokenRecorder())
}
