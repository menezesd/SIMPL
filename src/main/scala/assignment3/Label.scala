package assignment3

import java.util.UUID

final class Label(val name: String) {
  def this() = this(Label.generateUUID())
  def getName: String = name
}

object Label {
  def apply(name: String): Label = new Label(name)
  def apply(): Label = new Label()
  private[assignment3] def generateUUID(): String = {
    var uuid = UUID.randomUUID().toString.substring(0, 8)
    if (!uuid.head.isLetter) {
      val randomOffset = util.Random.nextInt('z' - 'a' + 1)
      val randomLetter = ('a'.toInt + randomOffset).toChar
      uuid = s"$randomLetter${uuid.substring(1)}"
    }
    uuid
  }
}
