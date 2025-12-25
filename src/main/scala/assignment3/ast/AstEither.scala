package assignment3.ast

import assignment3.{Messages, Operators, Type}
import assignment3.symbol.{MethodSymbol, ProgramSymbols}

object AstEither:
  import Operators._

  /** Resolve the class name of an expression as Either[Diag, String]. */
  def resolveClassNameD(e: Expr, method: MethodContext, ps: ProgramSymbols, line: Int, column: Int = -1): Either[Diag, String] =
    IdiomaticTypeUtils.classNameOf(e, method, ps)
      .toRight(TypeDiag(Messages.TypeCheck.unableToResolveClass, line, column))

  /** Resolve a method symbol on the class of an expression, or produce a Diag error. */
  def resolveMethodOnExprD(e: Expr, methodName: String, method: MethodContext, ps: ProgramSymbols, line: Int, column: Int = -1): Either[Diag, MethodSymbol] =
    for
      cn <- resolveClassNameD(e, method, ps, line, column)
      cs <- ps.getClass(cn).toRight(ResolveDiag(Messages.TypeCheck.unknownClass(cn), line, column))
      ms <- cs.method(methodName).toRight(ResolveDiag(Messages.TypeCheck.unknownMethod(cn, methodName), line, column))
    yield ms

  /** Resolve field info on a target expression. */
  def resolveFieldInfoD(target: Expr, fieldName: String, method: MethodContext, ps: ProgramSymbols, line: Int, column: Int = -1): Either[Diag, assignment3.symbol.ClassSymbol.FieldInfo] =
    for
      cn <- resolveClassNameD(target, method, ps, line, column)
      cs <- ps.getClass(cn).toRight(ResolveDiag(Messages.TypeCheck.unknownClass(cn), line, column))
      fi <- cs.getFieldInfo(fieldName).toRight(ResolveDiag(Messages.TypeCheck.unknownField(cn, fieldName), line, column))
    yield fi

  // --- Binary expression helpers ---

  /** Try to build a string repeat expression: String * Int or Int * String */
  private def tryStringRepeat(left: Expr, right: Expr, lt: Type, rt: Type): Option[Expr] =
    if (lt == Type.STRING && rt == Type.INT) || (lt == Type.INT && rt == Type.STRING) then
      Some(Binary(BinaryOp.Mul, left, right, Some(Type.STRING)))
    else None

  /** Try to build a string concatenation: String + String */
  private def tryStringConcat(left: Expr, right: Expr, lt: Type, rt: Type): Option[Expr] =
    if lt == Type.STRING && rt == Type.STRING then
      Some(Binary(BinaryOp.Add, left, right, Some(Type.STRING)))
    else None

  /** Try to build a string comparison: String < > = String */
  private def tryStringCompare(op: Char, left: Expr, right: Expr, lt: Type, rt: Type): Option[Expr] =
    if lt == Type.STRING && rt == Type.STRING then
      val bop = op match
        case LT => BinaryOp.Lt
        case GT => BinaryOp.Gt
        case EQ => BinaryOp.Eq
      Some(Binary(bop, left, right, Some(Type.BOOL)))
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
      val bop = op match
        case ADD => BinaryOp.Add
        case SUB => BinaryOp.Sub
        case MUL => BinaryOp.Mul
        case DIV => BinaryOp.Div
        case MOD => BinaryOp.Mod
      Right(Binary(bop, left, right, Some(Type.INT)))

  /** Build comparison expression: < > = with compatible types */
  private def buildComparisonD(op: Char, left: Expr, right: Expr, lt: Type, rt: Type, line: Int, column: Int): Either[Diag, Expr] =
    if !lt.isCompatibleWith(rt) then
      Left(TypeDiag(Messages.TypeCheck.comparisonRequiresMatchingTypes, line, column))
    else
      val bop = op match
        case LT => BinaryOp.Lt
        case GT => BinaryOp.Gt
        case EQ => BinaryOp.Eq
      Right(Binary(bop, left, right, Some(Type.BOOL)))

  /** Type-check and build a binary expression with diagnostic result. */
  def buildBinaryD(left: Expr, op: Char, right: Expr, method: MethodContext, ps: ProgramSymbols, line: Int, column: Int = -1): Either[Diag, Expr] =
    val lt = IdiomaticTypeUtils.typeOf(left, method, ps)
    val rt = IdiomaticTypeUtils.typeOf(right, method, ps)
    val bothString = lt == Type.STRING && rt == Type.STRING
    val stringInt = (lt == Type.STRING && rt == Type.INT) || (lt == Type.INT && rt == Type.STRING)

    // Pattern match on (operator, type context) for cleaner dispatch
    (op, lt, rt) match
      // String repeat: String * Int or Int * String
      case (MUL, _, _) if stringInt =>
        Right(Binary(BinaryOp.Mul, left, right, Some(Type.STRING)))

      // String concatenation: String + String
      case (ADD, Type.STRING, Type.STRING) =>
        Right(Binary(BinaryOp.Add, left, right, Some(Type.STRING)))

      // String comparison: String <, >, = String
      case (LT | GT | EQ, Type.STRING, Type.STRING) =>
        val bop = op match { case LT => BinaryOp.Lt; case GT => BinaryOp.Gt; case _ => BinaryOp.Eq }
        Right(Binary(bop, left, right, Some(Type.BOOL)))

      // Equality on compatible types
      case (EQ, _, _) if lt.isCompatibleWith(rt) =>
        Right(Binary(BinaryOp.Eq, left, right, Some(Type.BOOL)))

      // Numeric comparison: Int <, > Int
      case (LT | GT, Type.INT, Type.INT) =>
        val bop = if op == LT then BinaryOp.Lt else BinaryOp.Gt
        Right(Binary(bop, left, right, Some(Type.BOOL)))

      // Logical operators: Bool & | Bool
      case (AND | OR, Type.BOOL, Type.BOOL) =>
        val bop = if op == AND then BinaryOp.And else BinaryOp.Or
        Right(Binary(bop, left, right, Some(Type.BOOL)))

      // Arithmetic operators: Int +, -, *, /, % Int
      case (ADD | SUB | MUL | DIV | MOD, Type.INT, Type.INT) =>
        val bop = op match
          case ADD => BinaryOp.Add; case SUB => BinaryOp.Sub; case MUL => BinaryOp.Mul
          case DIV => BinaryOp.Div; case MOD => BinaryOp.Mod
        Right(Binary(bop, left, right, Some(Type.INT)))

      // Type errors for logical/arithmetic with wrong types
      case (AND | OR, _, _) =>
        Left(TypeDiag(Messages.TypeCheck.logicalOpRequiresBool, line, column))

      case (ADD | SUB | MUL | DIV | MOD, _, _) =>
        Left(TypeDiag(Messages.TypeCheck.arithmeticOpRequiresInt, line, column))

      case (LT | GT | EQ, _, _) =>
        Left(TypeDiag(Messages.TypeCheck.comparisonRequiresMatchingTypes, line, column))

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
