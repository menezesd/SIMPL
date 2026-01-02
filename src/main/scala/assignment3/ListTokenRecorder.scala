package assignment3

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

/** Simple in-memory recorder backed by a list (Scala port).
  *
  * @param maxTokens Maximum number of tokens to store (prevents unbounded memory growth).
  *                  When limit is reached, oldest tokens are discarded.
  */
final class ListTokenRecorder(maxTokens: Int = 100000) extends TokenRecorder {
  private val tokens = ListBuffer.empty[String]

  override def record(token: String): Unit = {
    if (tokens.size >= maxTokens) tokens.remove(0)
    tokens += token
  }

  override def clear(): Unit = tokens.clear()

  def snapshot(): java.util.List[String] = java.util.Collections.unmodifiableList(tokens.toList.asJava)

  def size: Int = tokens.size
}
