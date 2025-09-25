package assignment3

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

/** Simple in-memory recorder backed by a list (Scala port). */
final class ListTokenRecorder extends TokenRecorder {
  private val tokens = ListBuffer.empty[String]
  override def record(token: String): Unit = tokens += token
  override def clear(): Unit = tokens.clear()
  def snapshot(): java.util.List[String] = java.util.Collections.unmodifiableList(tokens.toList.asJava)
}
