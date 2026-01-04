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

  /** Extract both primitive and object type as a tuple for convenience. */
  def toPrimitiveAndObject: (Option[Type], Option[String]) =
    (primitiveOpt, classNameOpt)

  /** Check if this type matches parsed type information. */
  def matches(parsedPrimOpt: Option[Type], parsedObjOpt: Option[String]): Boolean = this match
    case ObjectRef(cn) => parsedObjOpt.contains(cn)
    case Primitive(t) => parsedPrimOpt.contains(t)

  /** Format as user-friendly string for error messages (without obj: prefix). */
  def formatForError: String = this match
    case ObjectRef(cn) => cn
    case Primitive(t) => t.toString

object ValueType:
  def ofPrimitive(t: Type): ValueType = Primitive(t)
  def ofObject(className: String): ValueType = ObjectRef(className)
  def ofObject(ot: ObjectType): ValueType = ObjectRef(ot.className)

  /** Format parsed type options for error messages. */
  def formatParsed(objOpt: Option[String], primOpt: Option[Type]): String =
    objOpt.getOrElse(primOpt.fold("void")(_.toString))

// Legacy type aliases for backward compatibility during refactoring
type PrimitiveType = ValueType.Primitive
type ObjectRefType = ValueType.ObjectRef
val PrimitiveType = ValueType.Primitive
val ObjectRefType = ValueType.ObjectRef
