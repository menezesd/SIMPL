package assignment3

/** Wrapper representing either a primitive Type or an object reference type. */
final class ValueType private (val primitive: Type, val objectRef: ObjectType) {
  def isPrimitive: Boolean = primitive != null
  def isObject: Boolean = objectRef != null
  def getPrimitive: Type = primitive
  def getObject: ObjectType = objectRef

  def isCompatibleWith(other: ValueType): Boolean = {
    if (other == null) return false
    if (this.isPrimitive && other.isPrimitive) this.primitive == other.primitive
    else if (this.isObject && other.isObject) this.objectRef.isCompatibleWith(other.objectRef)
    else false
  }
  override def toString: String =
    if (isPrimitive) primitive.toString
    else if (isObject) s"obj:${objectRef.getClassName}"
    else "<void>"
  override def equals(o: Any): Boolean = o match {
    case v: ValueType => primitive == v.primitive && objectRef == v.objectRef
    case _ => false
  }
  override def hashCode(): Int = java.util.Objects.hash(primitive, objectRef)
}

object ValueType {
  def ofPrimitive(t: Type): ValueType = new ValueType(java.util.Objects.requireNonNull(t), null)
  def ofObject(className: String): ValueType = new ValueType(null, ObjectType(java.util.Objects.requireNonNull(className)))
  def ofObject(ot: ObjectType): ValueType = new ValueType(null, java.util.Objects.requireNonNull(ot))

  def sameObjectClass(a: ValueType, b: ValueType): Boolean =
    a != null && b != null && a.isObject && b.isObject && a.getObject.getClassName == b.getObject.getClassName
  def samePrimitive(a: ValueType, b: ValueType): Boolean =
    a != null && b != null && a.isPrimitive && b.isPrimitive && a.getPrimitive == b.getPrimitive
  def equalsNullable(a: ValueType, b: ValueType): Boolean = java.util.Objects.equals(a, b)
}
