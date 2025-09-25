package assignment3.ast

import assignment3.{Type, ValueType}

/** Compatibility forwarders to the idiomatic type utils. */
private[ast] object AstTypeUtils {
  def resolveValueType(e: Expr, method: MethodContext, programSymbols: assignment3.symbol.ProgramSymbols): ValueType = {
    val t = IdiomaticTypeUtils.classNameOf(e, method, programSymbols)
    if (t != null) ValueType.ofObject(t) else ValueType.ofPrimitive(IdiomaticTypeUtils.typeOf(e, method, programSymbols))
  }
  def classNameOf(e: Expr, method: MethodContext, programSymbols: assignment3.symbol.ProgramSymbols): String =
    IdiomaticTypeUtils.classNameOf(e, method, programSymbols)
}
