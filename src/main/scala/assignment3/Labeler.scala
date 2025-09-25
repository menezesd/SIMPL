package assignment3

final class Labeler {
  def methodLabel(className: String, methodName: String): String = s"${className}_${methodName}"
}
