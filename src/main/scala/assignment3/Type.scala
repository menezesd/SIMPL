
package assignment3

/** Primitive types supported by LiveOak-3. */
enum Type(val typeName: String) {
  case INT extends Type("int")
  case BOOL extends Type("bool")
  case STRING extends Type("String")

  /** Primitive compatibility: only identical primitive types are compatible in LO-3. */
  def isCompatibleWith(other: Type): Boolean = this eq other

  override def toString: String = typeName
}

object Type {
  def fromString(typeString: String): Option[Type] = Option(typeString).flatMap(s => values.find(_.typeName == s))

  /** Parse type string, returning Either for diagnostic-first flow. */
  def parseE(typeString: String, line: Int): Either[String, Type] =
    fromString(typeString).toRight(s"Invalid type: $typeString")

  /** Legacy throwing API - prefer parseE for new code. */
  @deprecated("Use parseE instead", "2.0")
  @throws[TypeErrorException]
  def parse(typeString: String, line: Int): Type =
    fromString(typeString).getOrElse(throw TypeErrorException(s"Invalid type: $typeString", line))
}
