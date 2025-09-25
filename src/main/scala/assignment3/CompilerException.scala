package assignment3

/** Exception thrown by the compiler to indicate an error at a specific source position (Scala port). */
class CompilerException(message: String, line: Int, column: Int) extends SourcePositionedException(message, line, column) {
  def this(message: String, line: Int) = this(message, line, -1)
}
