package assignment3

/** Exception thrown to indicate a syntax error at a specific location in the source code (Scala port). */
class SyntaxErrorException(message: String, line: Int, column: Int) extends SourcePositionedException(message, line, column) {
  def this(message: String, line: Int) = this(message, line, -1)
}
