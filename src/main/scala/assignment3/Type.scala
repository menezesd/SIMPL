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
  def fromString(typeString: String): Option[Type] = Option(typeString).flatMap { s =>
    values.find(_.typeName == s)
  }
  @throws[TypeErrorException]
  def parse(typeString: String, line: Int): Type = fromString(typeString).getOrElse(throw new TypeErrorException("Invalid type: " + typeString, line))
}
