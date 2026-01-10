package assignment3.ast

import assignment3.{Messages, PrimitiveType, ObjectRefType, Type, ValueType}
import assignment3.symbol.{MethodSymbol, ProgramSymbols}

/** Unified type compatibility checking utilities.
  *
  * Consolidates type checking logic used across semantic analysis and code generation.
  */
object TypeCompatibility:
  import IdiomaticTypeUtils as TU

  /** Check if an expression's type is compatible with an expected type. */
  def isAssignable(
    expected: ValueType,
    expr: Expr,
    methodCtx: MethodContext,
    symbols: ProgramSymbols
  ): Boolean = (expected, expr) match
    case (ObjectRefType(_), _: NullLit) => true
    case (ObjectRefType(cn), _) =>
      TU.classNameOf(expr, methodCtx, symbols) match
        case Some(rhsClass) => rhsClass == cn
        case None => true // unknown - be permissive
    case (PrimitiveType(pt), _) =>
      pt.isCompatibleWith(TU.typeOf(expr, methodCtx, symbols))

  /** Check if two ValueTypes are directly compatible. */
  def typesCompatible(a: ValueType, b: ValueType): Boolean =
    a.isCompatibleWith(b)

  /** Check if a primitive type is compatible with another type. */
  def primitiveCompatible(pt: Type, other: Type): Boolean =
    pt.isCompatibleWith(other)

  /** Get the effective type of an expression for type checking. */
  def effectiveType(expr: Expr, methodCtx: MethodContext, symbols: ProgramSymbols): Type =
    TU.typeOf(expr, methodCtx, symbols)

  /** Get the class name of an expression if it has object type. */
  def classNameOf(expr: Expr, methodCtx: MethodContext, symbols: ProgramSymbols): Option[String] =
    TU.classNameOf(expr, methodCtx, symbols)

  /** Check if expression has boolean type. */
  def isBoolType(expr: Expr, methodCtx: MethodContext, symbols: ProgramSymbols): Boolean =
    TU.typeOf(expr, methodCtx, symbols) == Type.BOOL

  /** Check if expression has integer type. */
  def isIntType(expr: Expr, methodCtx: MethodContext, symbols: ProgramSymbols): Boolean =
    TU.typeOf(expr, methodCtx, symbols) == Type.INT

  /** Check if expression has string type. */
  def isStringType(expr: Expr, methodCtx: MethodContext, symbols: ProgramSymbols): Boolean =
    TU.typeOf(expr, methodCtx, symbols) == Type.STRING
