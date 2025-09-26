
package assignment3

/** Exception thrown by the compiler to indicate an error at a specific source position (Scala port, idiomatic). */
case class CompilerException(message: String, override val line: Int, override val column: Int = -1)
  extends SourcePositionedException(message, line, column)
