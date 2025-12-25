package assignment3.ast

import assignment3.{Type, ValueType, Label, PrimitiveType, ObjectRefType}
// Method context & frame abstractions used throughout Scala code
import assignment3.symbol.{MethodSymbol, VarSymbol}

trait MethodContext {
  def getName: String
  def getReturnType: Type
  def numParameters(): Int
  def numLocals(): Int
  def returnAddressOffset(): Int
  def lookupVar(name: String): Option[VarSymbol]
  def lookupMethodGlobal(name: String): Option[MethodSymbol]
}

trait MethodFrame {
  def getName: String
  def getReturnType: Type
  def numParameters(): Int
  def numLocals(): Int
  def returnAddressOffset(): Int
  def lookupVar(name: String): Option[VarBinding]
  def setReturnLabel(label: Label): Unit
  def getReturnLabel: Label
}

// Callable abstractions (Scala canonical versions)
trait ScalaCallableMethod extends CallableMethod {
  // ADT for return signature to avoid null checks at call sites
  def getReturnSig: assignment3.ast.high.ReturnSig
}

/** Factory methods for creating callable wrappers. */
object ScalaCallableMethod:
  import assignment3.symbol.MethodSymbol

  /** Create a callable for an instance method call (includes implicit 'this' param). */
  def forInstance(className: String, symbol: MethodSymbol): ScalaCallableMethod =
    new ScalaInstanceCallable(className, symbol)

  /** Create a callable for a static/global method call. */
  def forSymbol(symbol: MethodSymbol): ScalaCallableMethod =
    new SymbolCallableMethod(symbol)

  /** Create a fallback callable when symbol resolution fails (permissive mode). */
  def fallback(label: String, returnType: Option[ValueType], paramCount: Int): ScalaCallableMethod =
    new ScalaInstanceCallableFallback(label, returnType, paramCount)

final class ScalaInstanceCallable(className: String, symbol: MethodSymbol) extends ScalaCallableMethod {
  override def getName: String = s"${className}_${symbol.getName}"
  // Ensure Expr type is always a lowered primitive for codegen; map object/void to INT
  override def getReturnType: Type = CodegenTypes.loweredReturn(symbol)
  override def getReturnSig: assignment3.ast.high.ReturnSig = symbol.getReturnSig
  override def getParamCount: Int = symbol.numParameters() + 1 // implicit this
  override def getParameterType(index: Int): Type =
    if (index == 0) Type.INT
    else CodegenTypes.lowered(symbol.parameters(index - 1).valueType)
  def getSymbol: MethodSymbol = symbol
}

final class ScalaInstanceCallableFallback(label: String, ret: Option[ValueType], count: Int) extends ScalaCallableMethod {
  override def getName: String = label
  override def getReturnType: Type = ret match {
    case None => Type.INT
    case Some(ObjectRefType(_)) => Type.INT
    case Some(PrimitiveType(t)) => t
  }
  override def getReturnSig: assignment3.ast.high.ReturnSig = ret match {
    case None => assignment3.ast.high.ReturnSig.Void
    case Some(ObjectRefType(ot)) => assignment3.ast.high.ReturnSig.Obj(ot.getClassName)
    case Some(PrimitiveType(t)) => assignment3.ast.high.ReturnSig.Prim(t)
  }
  override def getParamCount: Int = count
  override def getParameterType(index: Int): Type = Type.INT // lowered default
}

// Adapter VarBinding if needed (core.scala defines trait VarBinding)
final case class SimpleVarBinding(name: String, tpe: Type, address: Int) extends VarBinding {
  def getName: String = name
  def getType: Type = tpe
  def getAddress: Int = address
}

/** Extension methods for diagnostic-first variable lookup. */
extension (mf: MethodFrame)
  /** Lookup variable, returning Either for diagnostic-first flow. */
  def lookupVarE(name: String, pos: Int): Either[Diag, VarBinding] =
    mf.lookupVar(name).toRight(ResolveDiag(assignment3.Messages.undeclaredVariable(name), pos))

extension (mc: MethodContext)
  /** Lookup variable, returning Either for diagnostic-first flow. */
  def lookupVarE(name: String, pos: Int): Either[Diag, VarSymbol] =
    mc.lookupVar(name).toRight(ResolveDiag(assignment3.Messages.undeclaredVariable(name), pos))
