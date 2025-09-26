
package assignment3

import assignment3.symbol.ProgramSymbols

/** Per-compilation context (Scala port, idiomatic). */
final class CompilerContext {
  private var _symbols: Option[ProgramSymbols] = None
  val labeler = new Labeler()
  val recorder: TokenRecorder = new ListTokenRecorder()

  def symbols: Option[ProgramSymbols] = _symbols
  def symbols_=(s: ProgramSymbols): Unit = _symbols = Some(s)
}
