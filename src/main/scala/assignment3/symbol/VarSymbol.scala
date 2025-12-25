package assignment3.symbol

import assignment3.{Type, ValueType, ObjectType, PrimitiveType, ObjectRefType}

final case class VarSymbol(
  name: String,
  valueType: ValueType,
  parameter: Boolean,
  index: Int,
  line: Int = -1,
  column: Int = -1
) {
  def getName: String = name
  def getValueType: ValueType = valueType
  def isParameter: Boolean = parameter
  def getIndex: Int = index
  def getLine: Int = line
  def getColumn: Int = column

  /** Pattern-matching based type access. */
  def primitiveOpt: Option[Type] = valueType match {
    case PrimitiveType(t) => Some(t)
    case _ => None
  }

  def objectTypeOpt: Option[ObjectType] = valueType match {
    case ObjectRefType(ot) => Some(ot)
    case _ => None
  }

  def classTypeNameOpt: Option[String] = objectTypeOpt.map(_.getClassName)

  /** Stack frame offset based on calling convention. */
  def stackAddress(totalParams: Int): Int =
    if (parameter) -(totalParams - index) else 2 + index

  override def toString: String = {
    val typeRepr = valueType match {
      case PrimitiveType(t) => t.toString
      case ObjectRefType(ot) => s"obj:${ot.getClassName}"
    }
    val loc = if (parameter) s"param@$index" else s"local@$index"
    s"VarSymbol{$name:$typeRepr,$loc}"
  }
}
