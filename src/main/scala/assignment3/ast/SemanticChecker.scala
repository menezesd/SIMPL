package assignment3.ast

import assignment3.symbol.{MethodSymbol, ProgramSymbols}

/** Minimal forwarder: delegates semantic checks to IdiomaticSemantic. */
final class SemanticChecker(currentMethod: MethodSymbol, defaultLine: Int, programSymbols: ProgramSymbols) {
  def this(currentMethod: MethodSymbol, defaultLine: Int) = this(currentMethod, defaultLine, null)

  def check(e: Expr): Unit = if (e != null) IdiomaticSemantic.checkExpr(e, currentMethod, defaultLine, programSymbols)
  def check(s: Stmt): Unit = if (s != null) IdiomaticSemantic.checkStmt(s, currentMethod, defaultLine, programSymbols)
}
