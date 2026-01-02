package assignment3

import scala.collection.mutable.Queue
import scala.jdk.CollectionConverters._

/** Simple in-memory recorder backed by a queue (Scala port).
  *
  * Uses Queue instead of ListBuffer for O(1) dequeue when discarding oldest tokens,
  * compared to O(n) for ListBuffer.remove(0).
  *
  * @param maxTokens Maximum number of tokens to store (prevents unbounded memory growth).
  *                  When limit is reached, oldest tokens are discarded.
  */
final class ListTokenRecorder(maxTokens: Int = 100000) extends TokenRecorder {
  private val tokens = Queue.empty[String]

  override def record(token: String): Unit = {
    if (tokens.size >= maxTokens) tokens.dequeue()
    tokens.enqueue(token)
  }

  override def clear(): Unit = tokens.clear()

  def snapshot(): java.util.List[String] = java.util.Collections.unmodifiableList(tokens.toList.asJava)

  def size: Int = tokens.size
}
