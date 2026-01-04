package assignment3.ast

import assignment3.{Type, ValueType, PrimitiveType, ObjectRefType}
import assignment3.symbol.{MethodSymbol, VarSymbol, ProgramSymbols}

/**
 * Unified type system utilities for the LiveOak-3 compiler.
 *
 * This object consolidates type-related operations that were previously
 * scattered across multiple files:
 * - Type resolution: determining the type of an expression
 * - Type lowering: converting high-level types to codegen-level primitives
 * - Class name extraction: getting the class name from object-typed expressions
 *
 * Related types:
 * - assignment3.Type: Primitive type enum (INT, BOOL, STRING)
 * - assignment3.ValueType: Sealed trait for primitive or object types
 * - assignment3.ast.high.ReturnSig: Method return signature ADT
 */
object TypeSystem:
  // ===== Type Resolution =====

  /** Resolve the primitive Type of an expression within a method context. */
  def typeOf(e: Expr, ctx: MethodContext, symbols: ProgramSymbols): Type =
    IdiomaticTypeUtils.typeOf(e, ctx, symbols)

  /** Extract the class name if the expression has object type. */
  def classNameOf(e: Expr, ctx: MethodContext, symbols: ProgramSymbols): Option[String] =
    IdiomaticTypeUtils.classNameOf(e, ctx, symbols)

  // ===== Type Lowering for Codegen =====

  /** Lower a ValueType to a primitive Type for code generation. Objects become INT (heap pointers). */
  def lowered(vt: ValueType): Type = CodegenTypes.lowered(vt)

  /** Lower a variable's type for code generation. */
  def lowered(v: VarSymbol): Type = CodegenTypes.lowered(v)

  /** Get the lowered return type of a method for code generation. Void and object returns become INT. */
  def loweredReturn(ms: MethodSymbol): Type = CodegenTypes.loweredReturn(ms)

  /** Get the lowered type for the return value of a callable. */
  def loweredReturnOf(m: CallableMethod): Type = IdiomaticTypeUtils.loweredReturnOf(m)

  // ===== Type Compatibility =====

  /** Check if an expression type is compatible with an expected ValueType. */
  def isAssignable(expected: ValueType, actual: Expr, ctx: NewMethodContext, symbols: ProgramSymbols): Boolean =
    (expected, actual) match
      case (ObjectRefType(_), _: NullLit) => true
      case (ObjectRefType(_), _) => classNameOf(actual, ctx, symbols).exists(_ == expected.classNameOpt.get)
      case (PrimitiveType(pt), _) => pt.isCompatibleWith(typeOf(actual, ctx, symbols))
