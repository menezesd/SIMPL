package assignment3.ast

import assignment3._
import assignment3.symbol.{MethodSymbol, VarSymbol}
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.compiletime.uninitialized

/** Frame backed by new MethodSymbol plus its vars (Scala port). */
final class SymbolMethodFrame(val symbol: MethodSymbol) extends MethodFrame {
  private val vars = mutable.HashMap.empty[String, VarBinding]
  private var returnLabel: Label = uninitialized
  // Populate
  symbol.getParameters.iterator().asScala.foreach(p => vars += p.getName -> new SymbolVarBinding(p, symbol))
  symbol.getLocals.iterator().asScala.foreach(l => vars += l.getName -> new SymbolVarBinding(l, symbol))

  def getName: String = symbol.getName
  def getReturnType: Type = symbol.getReturnSig match {
    case assignment3.ast.high.ReturnSig.Prim(t) => t
    case _ => Type.INT
  }
  def numParameters(): Int = symbol.numParameters()
  def numLocals(): Int = symbol.numLocals()
  def returnAddressOffset(): Int = symbol.returnAddressOffset()
  def lookupVar(name: String): Option[VarBinding] = vars.get(name)
  def setReturnLabel(label: Label): Unit = { returnLabel = label }
  def getReturnLabel: Label = returnLabel
  def getSymbol: MethodSymbol = symbol
}
