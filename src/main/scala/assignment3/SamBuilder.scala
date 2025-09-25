package assignment3

final class SamBuilder {
  private val sb = new StringBuilder
  def append(s: String): SamBuilder = { if (s != null) sb.append(s); this }
  def line(s: String): SamBuilder = { if (s != null) sb.append(s); sb.append('\n'); this }
  def label(name: String): SamBuilder = { if (name != null) sb.append(name).append(':').append('\n'); this }
  override def toString: String = sb.toString
}
