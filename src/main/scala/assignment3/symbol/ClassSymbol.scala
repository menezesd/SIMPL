package assignment3.symbol

import scala.jdk.CollectionConverters._

import assignment3.ValueType
import scala.collection.immutable.{Map, Vector}

final class ClassSymbol(val name: String) {
  private var fields: Map[String, VarSymbol] = Map.empty
  private var methods: Map[String, MethodSymbol] = Map.empty
  private var fieldOrder: Vector[String] = Vector.empty
  private var frozen = false

  def getName: String = name // temporary for external references; TODO remove

  def addField(v: VarSymbol): Unit = {
    if (frozen) throw new IllegalStateException(s"ClassSymbol '$name' is frozen; cannot add field")
    if (fields.contains(v.getName)) throw new IllegalStateException(s"Duplicate field '${v.getName}' in class '$name'")
    fields = fields + (v.getName -> v)
    fieldOrder = fieldOrder :+ v.getName
  }

  def addMethod(m: MethodSymbol): Unit = {
    if (frozen) throw new IllegalStateException(s"ClassSymbol '$name' is frozen; cannot add method")
    if (methods.contains(m.getName)) throw new IllegalStateException(s"Duplicate method '${m.getName}' in class '$name'")
    methods = methods + (m.getName -> m)
  }

  def field(n: String): Option[VarSymbol] = fields.get(n)
  def method(n: String): Option[MethodSymbol] = methods.get(n)
  def allFields: List[VarSymbol] = fields.values.toList
  def allMethods: List[MethodSymbol] = methods.values.toList

  import scala.jdk.CollectionConverters._
  def fieldOffset(fieldName: String): Int = fieldOrder.indexOf(fieldName)
  def numFields(): Int = fieldOrder.size

  def freeze(): Unit = if (!frozen) { frozen = true; methods.values.foreach(_.freeze()) }

  def getFieldInfo(fieldName: String): Option[ClassSymbol.FieldInfo] = {
    fields.get(fieldName).map { vs =>
      val off = fieldOrder.indexOf(fieldName)
      val vt = vs.getValueType
      ClassSymbol.FieldInfo(off, vt, vs)
    }
  }

  override def toString: String = s"ClassSymbol{$name, fields=${fields.keySet}, methods=${methods.keySet}}"
}

object ClassSymbol {
  final case class FieldInfo(offset: Int, valueType: ValueType, symbol: VarSymbol) {
    def objectType(): Option[assignment3.ObjectType] = if (valueType.isObject) Some(valueType.getObject) else None
  }
}
