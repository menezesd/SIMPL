package assignment3

import assignment3.symbol.ProgramSymbols
import scala.compiletime.uninitialized

/** Per-compilation context (Scala port). */
final class CompilerContext {
  private var symbols: ProgramSymbols = uninitialized
  private val labeler = new Labeler()
  private val recorder: TokenRecorder = new ListTokenRecorder()

  def getSymbols: ProgramSymbols = symbols
  def setSymbols(s: ProgramSymbols): Unit = { symbols = s }
  def getLabeler: Labeler = labeler
  def getRecorder: TokenRecorder = recorder
}
