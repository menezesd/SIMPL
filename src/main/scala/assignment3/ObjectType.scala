package assignment3

/** Represents a reference to an object of a user defined class (LiveOak3). */
final case class ObjectType(className: String) {
  def getClassName: String = className
  def isCompatibleWith(other: ObjectType): Boolean = this == other
  override def toString: String = className
}
