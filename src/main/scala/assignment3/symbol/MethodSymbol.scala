package assignment3.symbol

import assignment3.ValueType
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
  def getName: String = name
  // Compatibility: prior API exposed a nullable ValueType (null means void)
  def getReturnValueType: ValueType = returnValueTypeOpt.orNull
  def getReturnSig: ReturnSig = returnValueTypeOpt match {
    case None => ReturnSig.Void
    case Some(vt) if vt.isObject => ReturnSig.Obj(vt.getObject.getClassName)
    case Some(vt) => ReturnSig.Prim(vt.getPrimitive)
  }

  def numParameters(): Int = parameters.size
  def numLocals(): Int = locals.size
  def expectedUserArgs(): Int = math.max(0, numParameters() - 1)
  def returnAddressOffset(): Int = -(1 + numParameters())

  def getBodyStartLine(): Int = bodyStartLine
  def getReturnTypeLine(): Int = returnTypeLine
  def getReturnTypeColumn(): Int = returnTypeColumn

  def lookup(ident: String): Option[VarSymbol] = (parameters ++ locals).find(_.getName == ident)

  override def toString: String = {
    val ret = getReturnSig match {
      case ReturnSig.Void => "void"
      case ReturnSig.Obj(cn) => s"obj:${cn}"
      case ReturnSig.Prim(t) => String.valueOf(t)
    }
    s"MethodSymbol{$name, return=$ret, params=${parameters.toList}, locals=${locals.toList}}"
  }
}
