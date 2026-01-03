package assignment3

/** Sealed union representing either a primitive Type or an object reference type. */
sealed trait ValueType {
  def isPrimitive: Boolean = this.isInstanceOf[PrimitiveType]
  def isObject: Boolean = this.isInstanceOf[ObjectRefType]

  /** Safe Option-based extractors (preferred). */
  def primitiveOpt: Option[Type] = this match {
    case PrimitiveType(t) => Some(t)
    case _ => None
  }

  def objectTypeOpt: Option[ObjectType] = this match {
    case ObjectRefType(ot) => Some(ot)
    case _ => None
  }

  def classNameOpt: Option[String] = objectTypeOpt.map(_.getClassName)

  def isCompatibleWith(other: ValueType): Boolean = (this, other) match
    case (PrimitiveType(t1), PrimitiveType(t2)) => t1 == t2
    case (ObjectRefType(ot1), ObjectRefType(ot2)) => ot1.isCompatibleWith(ot2)
    case _ => false

  override def toString: String = this match
    case PrimitiveType(t) => t.toString
    case ObjectRefType(ot) => s"obj:${ot.getClassName}"
}

final case class PrimitiveType(t: Type) extends ValueType
final case class ObjectRefType(ot: ObjectType) extends ValueType

object ValueType {
  def ofPrimitive(t: Type): ValueType = PrimitiveType(t)
  def ofObject(className: String): ValueType = ObjectRefType(ObjectType(className))
  def ofObject(ot: ObjectType): ValueType = ObjectRefType(ot)
}
