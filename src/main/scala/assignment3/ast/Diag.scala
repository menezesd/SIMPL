package assignment3.ast

// Structured diagnostics for parser/semantic resolution flows.
sealed trait Diag:
  def message: String
  def line: Int
  def column: Int = -1

final case class SyntaxDiag(message: String, line: Int, override val column: Int = -1) extends Diag
final case class TypeDiag(message: String, line: Int, override val column: Int = -1) extends Diag
final case class ResolveDiag(message: String, line: Int, override val column: Int = -1) extends Diag
