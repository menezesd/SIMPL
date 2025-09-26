
package assignment3

/** Base class for compiler exceptions that can carry both line and column information (Scala port, idiomatic). */
class SourcePositionedException(message: String, val line: Int, val column: Int) extends Exception(message) {
  override def toString: String = {
    val pos = s"line $line" + (if (column >= 0) s", column $column" else "")
    s"Error at $pos: ${getMessage}"
  }
}
