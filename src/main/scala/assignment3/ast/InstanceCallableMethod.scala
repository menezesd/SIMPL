package assignment3.ast

import assignment3.{Type, ValueType}
import assignment3.symbol.MethodSymbol
import assignment3.ast.high.ReturnSig

/**
 * DEPRECATED: Use ScalaInstanceCallable defined in infrastructure.scala
 */
@deprecated("Use ScalaInstanceCallable instead", since = "3.0")
final class InstanceCallableMethod(className: String, symbol: MethodSymbol) extends ScalaCallableMethod {
  override def getName: String = s"${className}_${symbol.getName}"
  override def getReturnValueType: ValueType = symbol.getReturnValueType
  override def getReturnSig: ReturnSig = symbol.getReturnSig
  // Lower object/void returns to INT for codegen Expr type
  override def getReturnType: Type = {
    val rv = symbol.getReturnValueType
    if (rv == null || rv.isObject) Type.INT else rv.getPrimitive
  }
  override def getParamCount: Int = symbol.numParameters() + 1
  override def getParameterType(index: Int): Type = if (index == 0) Type.INT else symbol.getParameters.get(index - 1).getType
  def getSymbol: MethodSymbol = symbol
}
