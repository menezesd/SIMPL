package assignment3.ast

import assignment3.Type
import assignment3.symbol.{MethodSymbol, ProgramSymbols, VarSymbol}

/** MethodContext backed by MethodSymbol (Scala port). */
final class NewMethodContext(val symbol: MethodSymbol, programSymbols: ProgramSymbols) extends MethodContext {
  override def getName: String = symbol.getName
  override def getReturnType: Type = assignment3.ast.CodegenTypes.loweredReturn(symbol)
  override def numParameters(): Int = symbol.numParameters()
  override def numLocals(): Int = symbol.numLocals()
  override def returnAddressOffset(): Int = symbol.returnAddressOffset()
  override def lookupVar(name: String): Option[VarSymbol] = symbol.lookup(name)
  override def lookupMethodGlobal(name: String): Option[MethodSymbol] =
    programSymbols.allClasses.iterator
      .flatMap(cs => cs.method(name))
      .nextOption()
  def getSymbol: MethodSymbol = symbol
}
