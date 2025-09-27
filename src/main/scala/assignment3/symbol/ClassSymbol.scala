package assignment3.symbol

import assignment3.ValueType
import scala.collection.immutable.{Map, Vector}

final case class ClassSymbol(
  name: String,
  fields: Vector[VarSymbol],
  methods: Map[String, MethodSymbol],
  fieldOrder: Vector[String]
) {
  def getName: String = name
  def field(n: String): Option[VarSymbol] = fields.find(_.getName == n)
  def method(n: String): Option[MethodSymbol] = methods.get(n)
  def allFields: List[VarSymbol] = fields.toList
  def allMethods: List[MethodSymbol] = methods.values.toList

  def fieldOffset(fieldName: String): Int = fieldOrder.indexOf(fieldName)
  def numFields(): Int = fieldOrder.size

  // Compatibility no-op
  def freeze(): Unit = ()

  def getFieldInfo(fieldName: String): Option[ClassSymbol.FieldInfo] = {
    field(fieldName).map { vs =>
      val off = fieldOffset(fieldName)
      val vt = vs.getValueType
      ClassSymbol.FieldInfo(off, vt, vs)
    }
  }

  override def toString: String = s"ClassSymbol{$name, fields=${fields.map(_.getName).toSet}, methods=${methods.keySet}}"
}

object ClassSymbol {
  final case class FieldInfo(offset: Int, valueType: ValueType, symbol: VarSymbol) {
    def objectType(): Option[assignment3.ObjectType] = if (valueType.isObject) Some(valueType.getObject) else None
  }
}
