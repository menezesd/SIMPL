package assignment3

/** Exception thrown to indicate a type error at a specific location in the source code (Scala port).
  * @deprecated Use TypeDiag instead. Will be removed in 3.0.
  */
@deprecated("Use TypeDiag instead", "2.0")
class TypeErrorException(message: String, line: Int, column: Int) extends SourcePositionedException(message, line, column) {
  def this(message: String, line: Int) = this(message, line, -1)
}
