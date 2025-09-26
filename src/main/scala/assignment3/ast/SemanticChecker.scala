package assignment3.ast

import assignment3.symbol.{MethodSymbol, ProgramSymbols}
// No longer throw CompilerException; use diagnostics or generic exceptions.

/** Minimal forwarder: delegates semantic checks to IdiomaticSemantic. */
final class SemanticChecker(currentMethod: MethodSymbol, defaultLine: Int, programSymbols: ProgramSymbols) {
  def this(currentMethod: MethodSymbol, defaultLine: Int) = this(currentMethod, defaultLine, null)

  // Diagnostic-first APIs: callers should use these and handle Left(diag) results.
  // These replace legacy throwing variants so semantic entrypoints consistently
  // return Either[Diag, Unit].
  def checkE(e: Expr): Either[Diag, Unit] =
    if (e == null) Right(()) else IdiomaticSemantic.checkExprE(e, currentMethod, defaultLine, programSymbols)

  def checkE(s: Stmt): Either[Diag, Unit] =
    if (s == null) Right(()) else IdiomaticSemantic.checkStmtE(s, currentMethod, defaultLine, programSymbols)
}
