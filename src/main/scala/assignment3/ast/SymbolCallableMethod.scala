package assignment3.ast

import assignment3.Type
import assignment3.symbol.{MethodSymbol, VarSymbol}

/** Adapter wrapping MethodSymbol for invocation metadata (Scala port). */
final class SymbolCallableMethod(val symbol: MethodSymbol) extends ScalaCallableMethod {
  override def getName: String = symbol.getName
  override def getReturnSig: assignment3.ast.high.ReturnSig = symbol.getReturnSig
  // Return type for Expr is always lowered to a primitive Type for codegen; object returns map to INT, void to INT
  override def getReturnType: Type = CodegenTypes.loweredReturn(symbol)
  override def getParamCount: Int = symbol.numParameters()
  override def getParameterType(index: Int): Type = {
    val v: VarSymbol = symbol.parameters(index)
    CodegenTypes.lowered(v.valueType)
  }
  def getSymbol: MethodSymbol = symbol
}
