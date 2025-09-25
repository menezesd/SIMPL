package assignment3

/** Exception thrown to indicate a type error at a specific location in the source code (Scala port). */
class TypeErrorException(message: String, line: Int, column: Int) extends CompilerException(message, line, column) {
  def this(message: String, line: Int) = this(message, line, -1)
}
