package assignment3.symbol

import assignment3.{ValueType, ObjectRefType}
import scala.collection.immutable.{Map, Vector}

final case class ClassSymbol(
  name: String,
  fields: Vector[VarSymbol],
  methods: Map[String, MethodSymbol],
  fieldOrder: Vector[String]
):
  // Cached field offset map for O(1) lookup
  private lazy val fieldOffsetMap: Map[String, Int] =
    fieldOrder.iterator.zipWithIndex.toMap

  // Cached field map for O(1) lookup
  private lazy val fieldMap: Map[String, VarSymbol] =
    fields.iterator.map(f => f.getName -> f).toMap

  def getName: String = name
  def field(n: String): Option[VarSymbol] = fieldMap.get(n)
  def method(n: String): Option[MethodSymbol] = methods.get(n)
  def allFields: List[VarSymbol] = fields.toList
  def allMethods: List[MethodSymbol] = methods.values.toList

  def fieldOffset(fieldName: String): Int = fieldOffsetMap.getOrElse(fieldName, -1)
  def numFields(): Int = fieldOrder.size

  def getFieldInfo(fieldName: String): Option[ClassSymbol.FieldInfo] =
    field(fieldName).map { vs =>
      ClassSymbol.FieldInfo(fieldOffset(fieldName), vs.valueType, vs)
    }

  override def toString: String =
    s"ClassSymbol{$name, fields=${fields.map(_.getName).toSet}, methods=${methods.keySet}}"

object ClassSymbol:
  final case class FieldInfo(offset: Int, valueType: ValueType, symbol: VarSymbol):
    def objectType(): Option[assignment3.ObjectType] = valueType match
      case ObjectRefType(ot) => Some(ot)
      case _ => None
