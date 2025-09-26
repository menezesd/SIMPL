
package assignment3

import java.util.UUID

case class Label(name: String) {
  // Maintain compatibility with old `new Label()` usage
  def this() = this(Label.generateUUID())
  // Maintain compatibility with old Java-style getter
  def getName: String = name
}

object Label {
  def apply(): Label = Label(generateUUID())
  private[assignment3] def generateUUID(): String = {
    var uuid = UUID.randomUUID().toString.substring(0, 8)
    if (!uuid.head.isLetter) {
      val randomLetter = (('a' + util.Random.nextInt('z' - 'a' + 1)).toChar)
      uuid = s"$randomLetter${uuid.substring(1)}"
    }
    uuid
  }
}
