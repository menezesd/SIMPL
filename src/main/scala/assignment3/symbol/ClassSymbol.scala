package assignment3.symbol

import scala.jdk.CollectionConverters._

import assignment3.{CompilerException, ValueType}
import scala.collection.mutable

final class ClassSymbol(val name: String) {
  private val fields = mutable.LinkedHashMap.empty[String, VarSymbol]
  private val methods = mutable.LinkedHashMap.empty[String, MethodSymbol]
  private val fieldOrder = mutable.ListBuffer.empty[String]
  private var frozen = false

  def getName: String = name

  def addField(v: VarSymbol): Unit = {
    if (frozen) throw new CompilerException(s"ClassSymbol '$name' is frozen; cannot add field", -1)
    if (fields.contains(v.getName)) throw new CompilerException(s"Duplicate field '${v.getName}' in class '$name'", -1)
    fields += v.getName -> v
    fieldOrder += v.getName
  }

  def addMethod(m: MethodSymbol): Unit = {
    if (frozen) throw new CompilerException(s"ClassSymbol '$name' is frozen; cannot add method", -1)
    if (methods.contains(m.getName)) throw new CompilerException(s"Duplicate method '${m.getName}' in class '$name'", -1)
    methods += m.getName -> m
  }

  def getField(n: String): VarSymbol = fields.getOrElse(n, null)
  def getMethod(n: String): MethodSymbol = methods.getOrElse(n, null)
  def allFields: java.lang.Iterable[VarSymbol] = java.util.Collections.unmodifiableCollection(fields.values.toList.asJava)
  def allMethods: java.lang.Iterable[MethodSymbol] = java.util.Collections.unmodifiableCollection(methods.values.toList.asJava)

  import scala.jdk.CollectionConverters._
  def fieldOffset(fieldName: String): Int = fieldOrder.indexOf(fieldName)
  def numFields(): Int = fieldOrder.size

  def freeze(): Unit = if (!frozen) { frozen = true; methods.values.foreach(_.freeze()) }

  def getFieldInfo(fieldName: String): ClassSymbol.FieldInfo = {
    val vs = fields.getOrElse(fieldName, null)
    if (vs == null) return null
    val off = fieldOrder.indexOf(fieldName)
    val vt = if (vs.isObject) ValueType.ofObject(vs.getObjectType) else ValueType.ofPrimitive(vs.getType)
    ClassSymbol.FieldInfo(off, vt, vs)
  }

  override def toString: String = s"ClassSymbol{$name, fields=${fields.keySet}, methods=${methods.keySet}}"
}

object ClassSymbol {
  final case class FieldInfo(offset: Int, valueType: ValueType, symbol: VarSymbol) {
    def objectType() = if (valueType.isObject) valueType.getObject else null
  }
}
