
package assignment3

final class SamBuilder {
  private val sb = new StringBuilder
  def append(s: String): SamBuilder = { sb.append(Option(s).getOrElse("")); this }
  def line(s: String): SamBuilder = { sb.append(Option(s).getOrElse("")).append('\n'); this }
  def label(name: String): SamBuilder = { sb.append(Option(name).getOrElse("")).append(':').append('\n'); this }
  override def toString: String = sb.toString
}
