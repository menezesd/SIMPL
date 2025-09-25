package assignment3.symbol

import scala.jdk.CollectionConverters._

import assignment3.{Type, ValueType}
import scala.collection.mutable.ListBuffer

final class MethodSymbol(val name: String, val returnValueType: ValueType) {
  private val parametersBuf = ListBuffer.empty[VarSymbol]
  private val localsBuf = ListBuffer.empty[VarSymbol]
  private val byName = scala.collection.mutable.LinkedHashMap.empty[String, VarSymbol]
  private var bodyStartLine: Int = -1
  private var returnTypeLine: Int = -1
  private var returnTypeColumn: Int = -1
  private var frozen = false

  // Java compatibility constructors
  def this(name: String, returnType: Type) = this(name, if (returnType == null) null else ValueType.ofPrimitive(returnType))
  def this(name: String, classReturnTypeName: String) = this(name, ValueType.ofObject(classReturnTypeName))
  def this(name: String, returnType: Type, classReturnTypeName: String) =
    this(name,
      if (returnType == null && classReturnTypeName == null) null
      else if (classReturnTypeName != null) ValueType.ofObject(classReturnTypeName)
      else ValueType.ofPrimitive(returnType))

  def getName: String = name
  def getReturnValueType: ValueType = returnValueType
  def getReturnType: Type = if (returnValueType == null || returnValueType.isObject) null else returnValueType.getPrimitive
  def getClassReturnTypeName: String = if (returnValueType != null && returnValueType.isObject) returnValueType.getObject.getClassName else null
  def returnsObject: Boolean = returnValueType != null && returnValueType.isObject
  def isVoidReturn: Boolean = returnValueType == null

  def getParameters: java.util.List[VarSymbol] = java.util.Collections.unmodifiableList(parametersBuf.asJava)
  def getLocals: java.util.List[VarSymbol] = java.util.Collections.unmodifiableList(localsBuf.asJava)

  import scala.jdk.CollectionConverters._
  def lookup(ident: String): VarSymbol = byName.getOrElse(ident, null)

  private def ensureMutable(): Unit = if (frozen) throw new IllegalStateException(s"MethodSymbol '$name' is frozen; cannot modify")

  def addParameter(paramName: String, t: Type): Unit = addParameter(paramName, t, -1, -1)
  def addParameter(paramName: String, t: Type, line: Int, column: Int): Unit = {
    ensureMutable(); val sym = new VarSymbol(paramName, t, true, parametersBuf.size, line, column)
    parametersBuf += sym; byName += paramName -> sym
  }
  def addParameterObject(paramName: String, classTypeName: String): Unit = addParameterObject(paramName, classTypeName, -1, -1)
  def addParameterObject(paramName: String, classTypeName: String, line: Int, column: Int): Unit = {
    ensureMutable(); val sym = new VarSymbol(paramName, classTypeName, true, parametersBuf.size, line, column)
    parametersBuf += sym; byName += paramName -> sym
  }

  def addLocal(localName: String, t: Type): Unit = addLocal(localName, t, -1, -1)
  def addLocal(localName: String, t: Type, line: Int, column: Int): Unit = {
    ensureMutable(); val sym = new VarSymbol(localName, t, false, localsBuf.size, line, column)
    localsBuf += sym; byName += localName -> sym
  }
  def addLocalObject(localName: String, classTypeName: String): Unit = addLocalObject(localName, classTypeName, -1, -1)
  def addLocalObject(localName: String, classTypeName: String, line: Int, column: Int): Unit = {
    ensureMutable(); val sym = new VarSymbol(localName, classTypeName, false, localsBuf.size, line, column)
    localsBuf += sym; byName += localName -> sym
  }

  def numParameters(): Int = parametersBuf.size
  def numLocals(): Int = localsBuf.size
  def expectedUserArgs(): Int = math.max(0, numParameters() - 1)
  def returnAddressOffset(): Int = -(1 + numParameters())

  def setBodyStartLine(line: Int): Unit = if (!frozen) bodyStartLine = line
  def getBodyStartLine(): Int = bodyStartLine
  def setReturnTypePosition(line: Int, column: Int): Unit = if (!frozen) { returnTypeLine = line; returnTypeColumn = column }
  def getReturnTypeLine(): Int = returnTypeLine
  def getReturnTypeColumn(): Int = returnTypeColumn

  def freeze(): Unit = frozen = true

  override def toString: String = {
    val ret = if (isVoidReturn) "void" else if (returnsObject) s"obj:${getClassReturnTypeName}" else String.valueOf(getReturnType)
    s"MethodSymbol{$name, return=$ret, params=${parametersBuf.toList}, locals=${localsBuf.toList}}"
  }
}
