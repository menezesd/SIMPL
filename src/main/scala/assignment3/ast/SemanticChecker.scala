package assignment3.ast

import assignment3.symbol.{MethodSymbol, ProgramSymbols}
// No longer throw CompilerException; use diagnostics or generic exceptions.

/** Minimal forwarder: delegates semantic checks to IdiomaticSemantic. */
final class SemanticChecker(currentMethod: MethodSymbol, defaultLine: Int, programSymbols: ProgramSymbols) {
  def this(currentMethod: MethodSymbol, defaultLine: Int) = this(currentMethod, defaultLine, null)

  def check(e: Expr): Unit =
    Option(e).foreach { ex => IdiomaticSemantic.checkExprE(ex, currentMethod, defaultLine, programSymbols) match
      case Left(diag) => throw new Exception(diag.message)
      case Right(_) => ()
    }
  def check(s: Stmt): Unit =
    Option(s).foreach { st => IdiomaticSemantic.checkStmtE(st, currentMethod, defaultLine, programSymbols) match
      case Left(diag) => throw new Exception(diag.message)
      case Right(_) => ()
    }

  // Either-based convenience, preserving legacy API above
  def checkE(e: Expr): Either[Diag, Unit] =
    if (e == null) Right(()) else IdiomaticSemantic.checkExprE(e, currentMethod, defaultLine, programSymbols)

  def checkE(s: Stmt): Either[Diag, Unit] =
    if (s == null) Right(()) else IdiomaticSemantic.checkStmtE(s, currentMethod, defaultLine, programSymbols)
}
