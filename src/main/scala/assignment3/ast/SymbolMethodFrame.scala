package assignment3.ast

import assignment3._
import assignment3.symbol.{MethodSymbol, VarSymbol}
import scala.collection.mutable
import scala.compiletime.uninitialized

/** Frame backed by new MethodSymbol plus its vars (Scala port). */
final class SymbolMethodFrame(val symbol: MethodSymbol) extends MethodFrame {
  private val vars = mutable.HashMap.empty[String, VarBinding]
  private var returnLabel: Label = uninitialized
  // Populate
  symbol.parameters.foreach(p => vars += p.getName -> new SymbolVarBinding(p, symbol))
  symbol.locals.foreach(l => vars += l.getName -> new SymbolVarBinding(l, symbol))

  def getName: String = symbol.getName
  def getReturnType: Type = assignment3.ast.CodegenTypes.loweredReturn(symbol)
  def numParameters(): Int = symbol.numParameters()
  def numLocals(): Int = symbol.numLocals()
  def returnAddressOffset(): Int = symbol.returnAddressOffset()
  def lookupVar(name: String): Option[VarBinding] = vars.get(name)
  def setReturnLabel(label: Label): Unit = { returnLabel = label }
  def getReturnLabel: Label = returnLabel
  def getSymbol: MethodSymbol = symbol
}
