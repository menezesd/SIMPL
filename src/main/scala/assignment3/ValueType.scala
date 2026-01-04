package assignment3

/** Enum representing either a primitive Type or an object reference type. */
enum ValueType:
  case Primitive(t: Type)
  case ObjectRef(className: String)

  def isPrimitive: Boolean = this match
    case Primitive(_) => true
    case _ => false

  def isObject: Boolean = this match
    case ObjectRef(_) => true
    case _ => false

  /** Safe Option-based extractors (preferred). */
  def primitiveOpt: Option[Type] = this match
    case Primitive(t) => Some(t)
    case _ => None

  def objectTypeOpt: Option[ObjectType] = this match
    case ObjectRef(cn) => Some(ObjectType(cn))
    case _ => None

  def classNameOpt: Option[String] = this match
    case ObjectRef(cn) => Some(cn)
    case _ => None

  def isCompatibleWith(other: ValueType): Boolean = (this, other) match
    case (Primitive(t1), Primitive(t2)) => t1 == t2
    case (ObjectRef(cn1), ObjectRef(cn2)) => cn1 == cn2
    case _ => false

  override def toString: String = this match
    case Primitive(t) => t.toString
    case ObjectRef(cn) => s"obj:$cn"

object ValueType:
  def ofPrimitive(t: Type): ValueType = Primitive(t)
  def ofObject(className: String): ValueType = ObjectRef(className)
  def ofObject(ot: ObjectType): ValueType = ObjectRef(ot.className)

// Legacy type aliases for backward compatibility during refactoring
type PrimitiveType = ValueType.Primitive
type ObjectRefType = ValueType.ObjectRef
val PrimitiveType = ValueType.Primitive
val ObjectRefType = ValueType.ObjectRef
