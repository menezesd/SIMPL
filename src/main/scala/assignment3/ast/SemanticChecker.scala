package assignment3.ast

import assignment3.symbol.{MethodSymbol, ProgramSymbols}
import assignment3.CompilerException

/** Minimal forwarder: delegates semantic checks to IdiomaticSemantic. */
final class SemanticChecker(currentMethod: MethodSymbol, defaultLine: Int, programSymbols: ProgramSymbols) {
  def this(currentMethod: MethodSymbol, defaultLine: Int) = this(currentMethod, defaultLine, null)

  def check(e: Expr): Unit =
    if (e != null) IdiomaticSemantic.checkExprE(e, currentMethod, defaultLine, programSymbols) match
      case Left(diag) => throw new CompilerException(diag.message, diag.line, diag.column)
      case Right(_) => ()
  def check(s: Stmt): Unit =
    if (s != null) IdiomaticSemantic.checkStmtE(s, currentMethod, defaultLine, programSymbols) match
      case Left(diag) => throw new CompilerException(diag.message, diag.line, diag.column)
      case Right(_) => ()

  // Either-based convenience, preserving legacy API above
  def checkE(e: Expr): Either[Diag, Unit] =
    if (e == null) Right(()) else IdiomaticSemantic.checkExprE(e, currentMethod, defaultLine, programSymbols)

  def checkE(s: Stmt): Either[Diag, Unit] =
    if (s == null) Right(()) else IdiomaticSemantic.checkStmtE(s, currentMethod, defaultLine, programSymbols)
}
