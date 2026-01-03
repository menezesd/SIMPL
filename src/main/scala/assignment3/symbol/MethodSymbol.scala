package assignment3.symbol

import assignment3.{ValueType, PrimitiveType, ObjectRefType}
import scala.collection.immutable.Vector
import assignment3.ast.high.ReturnSig

final case class MethodSymbol(
  name: String,
  parameters: Vector[VarSymbol],
  locals: Vector[VarSymbol],
  returnValueTypeOpt: Option[ValueType],
  bodyStartLine: Int = -1,
  returnTypeLine: Int = -1,
  returnTypeColumn: Int = -1
) {
  // Cached variable map for O(1) lookup
  private lazy val allVarsMap: Map[String, VarSymbol] =
    (parameters ++ locals).iterator.map(v => v.getName -> v).toMap

  def getName: String = name

  def getReturnSig: ReturnSig = returnValueTypeOpt match {
    case None => ReturnSig.Void
    case Some(ObjectRefType(ot)) => ReturnSig.Obj(ot.getClassName)
    case Some(PrimitiveType(t)) => ReturnSig.Prim(t)
  }

  def numParameters(): Int = parameters.size
  def numLocals(): Int = locals.size
  def expectedUserArgs(): Int = math.max(0, numParameters() - 1)
  def returnAddressOffset(): Int = -(1 + numParameters())

  def getBodyStartLine(): Int = bodyStartLine
  def getReturnTypeLine(): Int = returnTypeLine
  def getReturnTypeColumn(): Int = returnTypeColumn

  def lookup(ident: String): Option[VarSymbol] = allVarsMap.get(ident)

  override def toString: String = {
    val ret = getReturnSig match {
      case ReturnSig.Void => "void"
      case ReturnSig.Obj(cn) => s"obj:$cn"
      case ReturnSig.Prim(t) => t.toString
    }
    s"MethodSymbol{$name, return=$ret, params=${parameters.toList}, locals=${locals.toList}}"
  }
}
