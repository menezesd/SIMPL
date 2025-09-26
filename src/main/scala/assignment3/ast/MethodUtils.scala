package assignment3.ast

import assignment3.symbol.{MethodSymbol, VarSymbol}
import assignment3.{Type, ValueType}

/** Small helpers for working with MethodSymbol parameters in LO-3 (Scala port). */
object MethodUtils {
  /** Number of user-declared parameters (excludes implicit 'this'). */
  def expectedUserArgs(ms: MethodSymbol): Int = math.max(0, ms.numParameters() - 1)

  /** Get the i-th user parameter VarSymbol (0-based), skipping implicit 'this'. */
  def userParamAt(ms: MethodSymbol, userIndex: Int): VarSymbol = ms.getParameters.get(userIndex + 1)

  /** Whether the method uses implicit 'this' (assumed true for instance methods). */
  def hasThisParam(ms: MethodSymbol): Boolean = ms.numParameters() > 0 && ms.getParameters.get(0).getName == "this"

  def userParamCount(ms: MethodSymbol): Int = expectedUserArgs(ms)
  def returnValueType(ms: MethodSymbol): ValueType = ms.getReturnValueType
  def codegenLabel(className: String, ms: MethodSymbol): String = s"${className}_${ms.getName}"
  // ReturnSig helpers
  def primitiveReturnType(ms: MethodSymbol): Type = ms.getReturnSig match {
    case assignment3.ast.high.ReturnSig.Prim(t) => t
    case _ => Type.INT
  }
}
