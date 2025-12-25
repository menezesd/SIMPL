package assignment3.ast

import assignment3.symbol.{MethodSymbol, VarSymbol}
import assignment3.ast.high.ReturnSig
import assignment3.{Type, ValueType, PrimitiveType, ObjectRefType}

/** Helpers for lowering high-level types to primitive Type for codegen. */
object CodegenTypes {
  def lowered(vt: ValueType): Type = vt match {
    case PrimitiveType(t) => t
    case ObjectRefType(_) => Type.INT
  }

  def lowered(v: VarSymbol): Type = lowered(v.valueType)

  def loweredReturn(ms: MethodSymbol): Type = ms.getReturnSig match {
    case ReturnSig.Void      => Type.INT
    case ReturnSig.Obj(_)    => Type.INT
    case ReturnSig.Prim(tpe) => tpe
  }

  def loweredParam(ms: MethodSymbol, rawIndex: Int): Type =
    lowered(ms.parameters(rawIndex).valueType)

  def loweredUserParam(ms: MethodSymbol, userIndex: Int): Type =
    loweredParam(ms, userIndex + 1)
}
