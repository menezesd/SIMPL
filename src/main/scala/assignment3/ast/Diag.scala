package assignment3.ast

import assignment3.CompilerException

// Structured diagnostics for parser/semantic resolution flows.
sealed trait Diag:
  def message: String
  def line: Int
  def column: Int = -1

final case class SyntaxDiag(message: String, line: Int, override val column: Int = -1) extends Diag
final case class TypeDiag(message: String, line: Int, override val column: Int = -1) extends Diag
final case class ResolveDiag(message: String, line: Int, override val column: Int = -1) extends Diag

object Diag:
  def toCompilerException(d: Diag): CompilerException = new CompilerException(d.message, d.line, d.column)
