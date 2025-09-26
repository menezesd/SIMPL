package assignment3.ast

import assignment3.{Type, ValueType, Label}
import assignment3.symbol.MethodSymbol

// Method context & frame abstractions used throughout Scala code
import assignment3.symbol.{MethodSymbol, VarSymbol}

trait MethodContext {
  def getName: String
  def getReturnType: Type
  def numParameters(): Int
  def numLocals(): Int
  def returnAddressOffset(): Int
  def lookupVar(name: String): Option[VarSymbol]
  def lookupMethodGlobal(name: String): Option[MethodSymbol]
}

trait MethodFrame {
  def getName: String
  def getReturnType: Type
  def numParameters(): Int
  def numLocals(): Int
  def returnAddressOffset(): Int
  def lookupVar(name: String): Option[VarBinding]
  def setReturnLabel(label: Label): Unit
  def getReturnLabel: Label
}

// Callable abstractions (Scala canonical versions)
trait ScalaCallableMethod extends CallableMethod {
  def getReturnValueType: ValueType
  // ADT for return signature to avoid null checks at call sites
  def getReturnSig: assignment3.ast.high.ReturnSig
}

final class ScalaInstanceCallable(className: String, symbol: MethodSymbol) extends ScalaCallableMethod {
  override def getName: String = s"${className}_${symbol.getName}"
  // Ensure Expr type is always a lowered primitive for codegen; map object/void to INT
  override def getReturnType: Type = {
    val rv = symbol.getReturnValueType
    if (rv == null || rv.isObject) Type.INT else rv.getPrimitive
  }
  override def getReturnValueType: ValueType = symbol.getReturnValueType
  override def getReturnSig: assignment3.ast.high.ReturnSig = symbol.getReturnSig
  override def getParamCount: Int = symbol.numParameters() + 1 // implicit this
  override def getParameterType(index: Int): Type = if (index == 0) Type.INT else symbol.getParameters.get(index - 1).getType
  def getSymbol: MethodSymbol = symbol
}

final class ScalaInstanceCallableFallback(label: String, ret: ValueType, count: Int) extends ScalaCallableMethod {
  override def getName: String = label
  override def getReturnType: Type = if (ret == null) Type.INT else ret.getPrimitive
  override def getReturnValueType: ValueType = ret
  override def getReturnSig: assignment3.ast.high.ReturnSig =
    if ret == null then assignment3.ast.high.ReturnSig.Void
    else if ret.isObject then assignment3.ast.high.ReturnSig.Obj(ret.getObject.getClassName)
    else assignment3.ast.high.ReturnSig.Prim(ret.getPrimitive)
  override def getParamCount: Int = count
  override def getParameterType(index: Int): Type = Type.INT // lowered default
}

// Adapter VarBinding if needed (core.scala defines trait VarBinding)
final case class SimpleVarBinding(name: String, tpe: Type, address: Int) extends VarBinding {
  def getName: String = name
  def getType: Type = tpe
  def getAddress: Int = address
}
