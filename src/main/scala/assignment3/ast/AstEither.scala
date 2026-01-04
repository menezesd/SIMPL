package assignment3.ast

import assignment3.{BinaryOpMapping, Messages, Operators, Type}
import assignment3.symbol.{MethodSymbol, ProgramSymbols}

object AstEither:
  import Operators._
  import Result.toResult

  /** Resolve the class name of an expression as Either[Diag, String]. */
  def resolveClassNameD(e: Expr, method: MethodContext, ps: ProgramSymbols, line: Int, column: Int = -1): Either[Diag, String] =
    IdiomaticTypeUtils.classNameOf(e, method, ps)
      .toResult(TypeDiag(Messages.TypeCheck.unableToResolveClass, line, column))

  /** Resolve a method symbol on the class of an expression, or produce a Diag error. */
  def resolveMethodOnExprD(e: Expr, methodName: String, method: MethodContext, ps: ProgramSymbols, line: Int, column: Int = -1): Either[Diag, MethodSymbol] =
    for
      cn <- resolveClassNameD(e, method, ps, line, column)
      cs <- ps.getClass(cn).toResult(ResolveDiag(Messages.TypeCheck.unknownClass(cn), line, column))
      ms <- cs.method(methodName).toResult(ResolveDiag(Messages.TypeCheck.unknownMethod(cn, methodName), line, column))
    yield ms

  /** Resolve field info on a target expression. */
  def resolveFieldInfoD(target: Expr, fieldName: String, method: MethodContext, ps: ProgramSymbols, line: Int, column: Int = -1): Either[Diag, assignment3.symbol.ClassSymbol.FieldInfo] =
    IdiomaticTypeUtils.resolveFieldInfo(target, fieldName, method, ps) match
      case Some(fi) => Right(fi)
      case None =>
        // Try to get class name for better error message
        IdiomaticTypeUtils.classNameOf(target, method, ps) match
          case Some(cn) => Left(ResolveDiag(Messages.TypeCheck.unknownField(cn, fieldName), line, column))
          case None => Left(TypeDiag(Messages.TypeCheck.unableToResolveClass, line, column))

  // --- Binary expression helpers ---

  /** Unified helper for string operations. */
  private def tryStringOp(cond: Boolean, op: BinaryOp, left: Expr, right: Expr, resultType: Type): Option[Expr] =
    if cond then Some(Binary(op, left, right, Some(resultType))) else None

  /** Try to build a string repeat expression: String * Int or Int * String */
  private def tryStringRepeat(left: Expr, right: Expr, lt: Type, rt: Type): Option[Expr] =
    tryStringOp((lt == Type.STRING && rt == Type.INT) || (lt == Type.INT && rt == Type.STRING),
                BinaryOp.Mul, left, right, Type.STRING)

  /** Try to build a string concatenation: String + String */
  private def tryStringConcat(left: Expr, right: Expr, lt: Type, rt: Type): Option[Expr] =
    tryStringOp(lt == Type.STRING && rt == Type.STRING,
                BinaryOp.Add, left, right, Type.STRING)

  /** Try to build a string comparison: String < > = String */
  private def tryStringCompare(op: Char, left: Expr, right: Expr, lt: Type, rt: Type): Option[Expr] =
    if lt == Type.STRING && rt == Type.STRING then
      val bop = BinaryOpMapping.fromChar(op).getOrElse(BinaryOp.Eq)
      tryStringOp(true, bop, left, right, Type.BOOL)
    else None

  /** Build logical expression: & or | with BOOL operands */
  private def buildLogicalD(op: Char, left: Expr, right: Expr, lt: Type, rt: Type, line: Int, column: Int): Either[Diag, Expr] =
    if lt != Type.BOOL || rt != Type.BOOL then
      Left(TypeDiag(Messages.TypeCheck.logicalOpRequiresBool, line, column))
    else
      val bop = if op == AND then BinaryOp.And else BinaryOp.Or
      Right(Binary(bop, left, right, Some(Type.BOOL)))

  /** Build arithmetic expression: + - * / % with INT operands */
  private def buildArithmeticD(op: Char, left: Expr, right: Expr, lt: Type, rt: Type, line: Int, column: Int): Either[Diag, Expr] =
    if lt != Type.INT || rt != Type.INT then
      Left(TypeDiag(Messages.TypeCheck.arithmeticOpRequiresInt, line, column))
    else
      val bop = BinaryOpMapping.fromChar(op).getOrElse(BinaryOp.Add) // fallback should not occur
      Right(Binary(bop, left, right, Some(Type.INT)))

  /** Build comparison expression: < > = with compatible types */
  private def buildComparisonD(op: Char, left: Expr, right: Expr, lt: Type, rt: Type, line: Int, column: Int): Either[Diag, Expr] =
    if !lt.isCompatibleWith(rt) then
      Left(TypeDiag(Messages.TypeCheck.comparisonRequiresMatchingTypes, line, column))
    else
      val bop = BinaryOpMapping.fromChar(op).getOrElse(BinaryOp.Eq) // fallback should not occur
      Right(Binary(bop, left, right, Some(Type.BOOL)))

  /** Type-check and build a binary expression with diagnostic result.
    * Delegates to specialized helpers based on operator category.
    */
  def buildBinaryD(left: Expr, op: Char, right: Expr, method: MethodContext, ps: ProgramSymbols, line: Int, column: Int = -1): Either[Diag, Expr] =
    val lt = IdiomaticTypeUtils.typeOf(left, method, ps)
    val rt = IdiomaticTypeUtils.typeOf(right, method, ps)

    op match
      // Multiplication: check for string repeat first, then arithmetic
      case MUL =>
        tryStringRepeat(left, right, lt, rt)
          .map(Right(_))
          .getOrElse(buildArithmeticD(op, left, right, lt, rt, line, column))

      // Addition: check for string concat first, then arithmetic
      case ADD =>
        tryStringConcat(left, right, lt, rt)
          .map(Right(_))
          .getOrElse(buildArithmeticD(op, left, right, lt, rt, line, column))

      // Other arithmetic
      case SUB | DIV | MOD =>
        buildArithmeticD(op, left, right, lt, rt, line, column)

      // Comparison: check for string compare first, then numeric
      case LT | GT | EQ =>
        tryStringCompare(op, left, right, lt, rt)
          .map(Right(_))
          .getOrElse(buildComparisonD(op, left, right, lt, rt, line, column))

      // Logical
      case AND | OR =>
        buildLogicalD(op, left, right, lt, rt, line, column)

      case _ =>
        Left(TypeDiag(Messages.TypeCheck.unsupportedOperator(op), line, column))

  /** Type-check and build a unary expression for '~' or '!' operators with diagnostic result. */
  def buildUnaryD(op: Char, inner: Expr, method: MethodContext, ps: ProgramSymbols, line: Int, column: Int = -1): Either[Diag, Expr] =
    val t = IdiomaticTypeUtils.typeOf(inner, method, ps)
    if op == NEG then
      if t != Type.INT && t != Type.STRING then Left(TypeDiag(Messages.TypeCheck.negRequiresIntOrString, line, column))
      else Right(Unary(UnaryOp.Neg, inner, Some(if t == Type.STRING then Type.STRING else Type.INT)))
    else if op == NOT then
      if t != Type.BOOL then Left(TypeDiag(Messages.TypeCheck.notRequiresBool, line, column))
      else Right(Unary(UnaryOp.Not, inner, Some(Type.BOOL)))
    else Left(SyntaxDiag(Messages.TypeCheck.unsupportedUnaryOperator(op), line, column))

  /** Type-check and build a ternary expression with diagnostic result. */
  def buildTernaryD(cond: Expr, thenE: Expr, elseE: Expr, method: MethodContext, ps: ProgramSymbols, line: Int, column: Int = -1): Either[Diag, Expr] =
    val tt = IdiomaticTypeUtils.typeOf(thenE, method, ps)
    val et = IdiomaticTypeUtils.typeOf(elseE, method, ps)
    if !tt.isCompatibleWith(et) then Left(TypeDiag(Messages.TypeCheck.ternaryBranchTypeMismatch, line, column))
    else Right(Ternary(cond, thenE, elseE, Some(tt)))

  // Parser-specific small diagnostics helpers
  def methodCallOnThisForbiddenD(line: Int, column: Int = -1): Either[Diag, Nothing] =
    Left(SyntaxDiag(Messages.TypeCheck.methodCallOnThisForbidden, line, column))

  def checkSingleChainLevelD(dotCount: Int, line: Int, column: Int = -1): Either[Diag, Unit] =
    if dotCount > 0 then Left(SyntaxDiag(Messages.TypeCheck.inappropriateChaining, line, column)) else Right(())
