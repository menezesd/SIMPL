package assignment3.symbol

import assignment3.{Type, ValueType, ObjectType}

final case class VarSymbol(
  name: String,
  valueType: ValueType,
  parameter: Boolean,
  index: Int,
  line: Int = -1,
  column: Int = -1
) {
  // Convenience secondary constructors (Java style interop if needed)
  def this(name: String, primitive: Type, parameter: Boolean, index: Int) =
    this(name, ValueType.ofPrimitive(primitive), parameter, index)
  def this(name: String, classTypeName: String, parameter: Boolean, index: Int) =
    this(name, ValueType.ofObject(classTypeName), parameter, index)
  def this(name: String, primitive: Type, parameter: Boolean, index: Int, line: Int, column: Int) =
    this(name, ValueType.ofPrimitive(primitive), parameter, index, line, column)
  def this(name: String, classTypeName: String, parameter: Boolean, index: Int, line: Int, column: Int) =
    this(name, ValueType.ofObject(classTypeName), parameter, index, line, column)

  // Backwards helpers
  def getName: String = name
  def getValueType: ValueType = valueType
  def getType: Type = if (valueType.isPrimitive) valueType.getPrimitive else Type.INT // fallback
  def getClassTypeName: String = if (valueType.isObject) valueType.getObject.getClassName else null
  def isObject: Boolean = valueType.isObject
  def getObjectType: ObjectType = if (valueType.isObject) valueType.getObject else null
  def isParameter: Boolean = parameter
  def getIndex: Int = index
  def getLine: Int = line
  def getColumn: Int = column

  /** Stack frame offset based on calling convention. */
  def stackAddress(totalParams: Int): Int = if (parameter) -(totalParams - index) else 2 + index

  override def toString: String = {
    val typeRepr = if (valueType.isObject) s"obj:${valueType.getObject.getClassName}" else String.valueOf(valueType.getPrimitive)
    s"VarSymbol{$name:$typeRepr${if (parameter) ",param@" else ",local@"}$index}";
  }
}
