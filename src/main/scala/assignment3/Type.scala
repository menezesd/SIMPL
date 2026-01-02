
package assignment3

/** Primitive types supported by LiveOak-3.
  *
  * Implementation note: This is a Scala 3 enum, which means each case (INT, BOOL, STRING)
  * is a singleton object. Reference equality (`eq`) is safe and preferred for comparisons
  * since there is exactly one instance of each type. The `isCompatibleWith` method uses
  * `eq` explicitly to document this singleton guarantee.
  */
enum Type(val typeName: String) {
  case INT extends Type("int")
  case BOOL extends Type("bool")
  case STRING extends Type("String")

  /** Check if this type is compatible with another for assignment/comparison.
    * Uses reference equality (`eq`) since enum cases are singletons.
    * Only identical primitive types are compatible in LO-3.
    */
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
