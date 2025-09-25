package assignment3

/** Records tokens or token-like events for debugging/diagnostics (Scala port). */
trait TokenRecorder {
  def record(token: String): Unit
  def clear(): Unit
}
