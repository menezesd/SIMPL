package assignment3.ast

import assignment3.symbol.{MethodSymbol, VarSymbol}
import assignment3.{Type, ValueType}

/** Helpers for lowering high-level types to primitive Type for codegen (Scala port). */
object CodegenTypes {
  def lowered(vt: ValueType): Type =
    if (vt == null || vt.isObject) Type.INT else vt.getPrimitive

  def lowered(v: VarSymbol): Type = lowered(v.getValueType)
  def loweredReturn(ms: MethodSymbol): Type = lowered(ms.getReturnValueType)
  def loweredParam(ms: MethodSymbol, rawIndex: Int): Type = lowered(ms.getParameters.get(rawIndex).getValueType)
  def loweredUserParam(ms: MethodSymbol, userIndex: Int): Type = loweredParam(ms, userIndex + 1)
}
