package assignment3.ast

import assignment3.{Label, Type}
import assignment3.symbol.{MethodSymbol, VarSymbol}

/** Frame backed by new MethodSymbol plus its vars (Scala port). */
final class SymbolMethodFrame(val symbol: MethodSymbol) extends MethodFrame {
  private val vars: Map[String, VarBinding] = {
    val builder = Map.newBuilder[String, VarBinding]
    symbol.parameters.foreach(p => builder += p.getName -> new SymbolVarBinding(p, symbol))
    symbol.locals.foreach(l => builder += l.getName -> new SymbolVarBinding(l, symbol))
    builder.result()
  }
  private var returnLabel: Option[Label] = None

  def getName: String = symbol.getName
  def getReturnType: Type = assignment3.ast.CodegenTypes.loweredReturn(symbol)
  def numParameters(): Int = symbol.numParameters()
  def numLocals(): Int = symbol.numLocals()
  def returnAddressOffset(): Int = symbol.returnAddressOffset()
  def lookupVar(name: String): Option[VarBinding] = vars.get(name)
  def setReturnLabel(label: Label): Unit = { returnLabel = Some(label) }
  def getReturnLabel: Option[Label] = returnLabel
  def getSymbol: MethodSymbol = symbol
}
