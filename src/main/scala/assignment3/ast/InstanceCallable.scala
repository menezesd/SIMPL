package assignment3.ast

import assignment3.{Type, ValueType}
import assignment3.symbol.MethodSymbol
import assignment3.ast.high.ReturnSig

/**
 * DEPRECATED: Use ScalaInstanceCallable (symbol-backed) or ScalaInstanceCallableFallback (label-only)
 * defined in infrastructure.scala. This legacy class remains for historical parity but is not referenced.
 */
@deprecated("Use ScalaInstanceCallable or ScalaInstanceCallableFallback instead", since = "3.0")
final class InstanceCallable(classNameOrLabel: String, methodSymbol: MethodSymbol, labelOnlyReturn: ValueType, labelOnlyParamCount: Int) extends ScalaCallableMethod {
  // If methodSymbol != null => symbol-backed, else label-only
  private val symbol = methodSymbol
  private val label = if (symbol != null) s"${classNameOrLabel}_${symbol.getName}" else classNameOrLabel
  private val rvType: ValueType = if (symbol != null) symbol.getReturnValueType else labelOnlyReturn
  private val pCount: Int = if (symbol != null) symbol.numParameters() + 1 else labelOnlyParamCount

  def this(className: String, symbol: MethodSymbol) = this(className, symbol, null, -1)
  def this(label: String, returnValueType: ValueType, paramCount: Int) = this(label, null, returnValueType, paramCount)

  override def getName: String = label
  override def getReturnValueType: ValueType = rvType
  override def getReturnSig: ReturnSig =
    if (symbol != null) symbol.getReturnSig
    else if (rvType == null) ReturnSig.Void
    else if (rvType.isObject) ReturnSig.Obj(rvType.getObject.getClassName)
    else ReturnSig.Prim(rvType.getPrimitive)
  // Lower object/void returns to INT for Expr type used in codegen
  override def getReturnType: Type = if (rvType == null || rvType.isObject) Type.INT else rvType.getPrimitive
  override def getParamCount: Int = pCount
  override def getParameterType(index: Int): Type = {
    if (index == 0) return Type.INT // implicit 'this'
    if (symbol != null) CodegenTypes.lowered(symbol.getParameters.get(index - 1).getValueType) else Type.INT
  }
  def hasSymbol: Boolean = symbol != null
  def getSymbol: MethodSymbol = symbol
}
