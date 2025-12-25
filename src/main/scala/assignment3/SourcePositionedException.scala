
package assignment3

/** Base class for compiler exceptions that can carry both line and column information (Scala port, idiomatic).
  * @deprecated Use Diag-based error handling instead. Will be removed in 3.0.
  */
@deprecated("Use Diag-based error handling instead", "2.0")
class SourcePositionedException(message: String, val line: Int, val column: Int) extends Exception(message) {
  override def toString: String = {
    val pos = s"line $line" + (if (column >= 0) s", column $column" else "")
    s"Error at $pos: ${getMessage}"
  }
}
