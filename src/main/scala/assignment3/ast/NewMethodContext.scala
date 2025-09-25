package assignment3.ast

import assignment3._
import assignment3.symbol.{MethodSymbol, ProgramSymbols}

/** MethodContext backed by MethodSymbol (Scala port). */
final class NewMethodContext(val symbol: MethodSymbol, programSymbols: ProgramSymbols) extends MethodContext {
  override def getName: String = symbol.getName
  override def getReturnType: Type = symbol.getReturnType
  override def numParameters(): Int = symbol.numParameters()
  override def numLocals(): Int = symbol.numLocals()
  override def returnAddressOffset(): Int = symbol.returnAddressOffset()
  override def lookup(name: String): AnyRef = symbol.lookup(name)
  override def lookupMethodGlobal(name: String): AnyRef = {
    if (programSymbols == null) return null
    val it = programSymbols.allClasses().iterator()
    while (it.hasNext) {
      val cs = it.next()
      val ms = cs.getMethod(name)
      if (ms != null) return ms
    }
    null
  }
  def getSymbol: MethodSymbol = symbol
}
