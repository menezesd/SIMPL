package assignment3

/** Exception thrown to indicate a syntax error at a specific location in the source code (Scala port).
  * @deprecated Use SyntaxDiag instead. Will be removed in 3.0.
  */
@deprecated("Use SyntaxDiag instead", "2.0")
class SyntaxErrorException(message: String, line: Int, column: Int) extends SourcePositionedException(message, line, column) {
  def this(message: String, line: Int) = this(message, line, -1)
}
