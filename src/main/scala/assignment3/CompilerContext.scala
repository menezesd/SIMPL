
package assignment3

import assignment3.symbol.ProgramSymbols

/**
 * Per-compilation context (Scala port).
 *
 * Usage pattern:
 * 1. Create context at start of compilation
 * 2. Build symbols (first pass) - symbols are then set once
 * 3. Parse and emit using symbols
 * 4. Cleanup recorder at end
 *
 * Note: TokenRecorder is inherently mutable for token accumulation.
 * Labeler is now an object (stateless utility).
 */
final class CompilerContext {
  private var _symbols: Option[ProgramSymbols] = None
  val recorder: TokenRecorder = new ListTokenRecorder()

  def symbols: Option[ProgramSymbols] = _symbols
  def symbols_=(s: ProgramSymbols): Unit = _symbols = Some(s)
}
