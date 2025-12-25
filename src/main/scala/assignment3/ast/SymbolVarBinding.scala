package assignment3.ast

import assignment3.Type
import assignment3.symbol.{VarSymbol, MethodSymbol}

/** VarBinding backed by VarSymbol; address computed via MethodSymbol frame convention (Scala port). */
final class SymbolVarBinding(val symbol: VarSymbol, method: MethodSymbol) extends VarBinding {
  override def getName: String = symbol.getName
  override def getType: Type = CodegenTypes.lowered(symbol.valueType)
  override def getAddress: Int = symbol.stackAddress(method.numParameters())
  def getSymbol: VarSymbol = symbol
}
