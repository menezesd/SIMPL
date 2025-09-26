package assignment3.ast

import assignment3.{Type, ValueType}

/** Compatibility forwarders to the idiomatic type utils. */
private[ast] object AstTypeUtils {
  def resolveValueType(e: Expr, method: MethodContext, programSymbols: assignment3.symbol.ProgramSymbols): ValueType = {
    val tOpt = IdiomaticTypeUtils.classNameOf(e, method, programSymbols)
    tOpt.map(ValueType.ofObject).getOrElse(ValueType.ofPrimitive(IdiomaticTypeUtils.typeOf(e, method, programSymbols)))
  }
  def classNameOf(e: Expr, method: MethodContext, programSymbols: assignment3.symbol.ProgramSymbols): String =
  IdiomaticTypeUtils.classNameOf(e, method, programSymbols).getOrElse(null)
}
