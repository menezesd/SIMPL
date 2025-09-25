package assignment3.ast

import assignment3.{Type, ValueType}
import assignment3.symbol.MethodSymbol

/** Callable wrapper for instance methods (Scala port). */
final class InstanceCallableMethod(className: String, symbol: MethodSymbol) extends ScalaCallableMethod {
  override def getName: String = s"${className}_${symbol.getName}"
  override def getReturnValueType: ValueType = symbol.getReturnValueType
  // Lower object/void returns to INT for codegen Expr type
  override def getReturnType: Type = {
    val rv = symbol.getReturnValueType
    if (rv == null || rv.isObject) Type.INT else rv.getPrimitive
  }
  override def getParamCount: Int = symbol.numParameters() + 1
  override def getParameterType(index: Int): Type = if (index == 0) Type.INT else symbol.getParameters.get(index - 1).getType
  def getSymbol: MethodSymbol = symbol
}
