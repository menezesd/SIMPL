package assignment3.ast

import assignment3.symbol.{MethodSymbol, ProgramSymbols}
import assignment3.{Messages, PrimitiveType, ObjectRefType}

/** Naming conventions used in this module:
  *  - `ctx`: CheckContext (semantic checking context)
  *  - `method`/`ms`: MethodSymbol
  *  - `symbols`/`ps`: ProgramSymbols
  *  - `methodCtx`: NewMethodContext (for type resolution)
  */

/** Helper for null literal validation. */
private object NullCheck:
  /** Reject null literals with a custom error. */
  def rejectNull(expr: Expr, onNull: => Diag): Result[Unit] = expr match
    case _: NullLit => Left(onNull)
    case _ => Right(())

  /** Require expression to be non-null for an operation. */
  def requireNonNull(expr: Expr, diag: => Diag): Result[Unit] =
    rejectNull(expr, diag)

/** Idiomatic semantic checker for idiomatic AST. */
object IdiomaticSemantic:
  import IdiomaticTypeUtils as TU
  import assignment3.ast.high.ReturnSig
  import assignment3.ast.{Diag, SyntaxDiag, TypeDiag}

  /** Semantic checking context - caches NewMethodContext to avoid repeated allocation. */
  private final class CheckContext(val method: MethodSymbol, val symbols: ProgramSymbols, val line: Int):
    lazy val methodCtx: NewMethodContext = new NewMethodContext(method, symbols)

  // Shared helper: resolve field info from target expression and field name.
  private def resolveFieldInfo(target: Expr, fieldName: String, ctx: CheckContext): Option[assignment3.symbol.ClassSymbol.FieldInfo] =
    TU.resolveFieldInfo(target, fieldName, ctx.methodCtx, ctx.symbols)

  // Shared helper: ensure RHS is assignable to expected ValueType (primitive/object rules, null allowed for objects).
  private def checkAssignable(
      expected: assignment3.ValueType,
      rhs: Expr,
      ctx: CheckContext,
      objMismatchMsg: String,
      primMismatchMsg: String
  ): Either[Diag, Unit] = (expected, rhs) match
    case (ObjectRefType(_), _: NullLit) => Right(())
    case (ObjectRefType(cn), _) =>
      TU.classNameOf(rhs, ctx.methodCtx, ctx.symbols) match
        case Some(rhsClass) if rhsClass != cn => Left(TypeDiag(objMismatchMsg, ctx.line))
        case _ => Right(()) // match or unknown - be permissive
    case (PrimitiveType(pt), _) =>
      Result.require(pt.isCompatibleWith(TU.typeOf(rhs, ctx.methodCtx, ctx.symbols)),
        TypeDiag(primMismatchMsg, ctx.line))

  // ─────────────────────────────────────────────────────────────────────────────
  // Parameter Validation
  // ─────────────────────────────────────────────────────────────────────────────

  /** Validates parameter types and argument counts for method calls. */
  private object ParameterValidation:
    /** Validate a single parameter type match. */
    def checkParameterType(formal: assignment3.symbol.VarSymbol, argExpr: Expr, ctx: CheckContext): Option[Diag] =
      val argType = TU.typeOf(argExpr, ctx.methodCtx, ctx.symbols)
      formal.valueType match
        case PrimitiveType(pt) if !pt.isCompatibleWith(argType) =>
          Some(TypeDiag(Messages.Semantic.argTypeMismatch(formal.getName), ctx.line))
        case _ => None

    /** Validate all parameter types for a method call. */
    def validateArgs(ms: assignment3.symbol.MethodSymbol, args: List[Expr], ctx: CheckContext): Either[Diag, Unit] =
      val expected = ms.expectedUserArgs()
      val provided = args.size
      if expected != provided then
        Left(SyntaxDiag(Messages.Semantic.incorrectArgCount, ctx.line))
      else
        val firstErr: Option[Diag] = (0 until provided).iterator
          .map(i => checkParameterType(ms.parameters(i + 1), args(i), ctx))
          .collectFirst { case Some(diag) => diag }
        firstErr.fold(Right(()))(Left(_))

  // Helper: check that a condition expression is BOOL
  private def requireBoolCondition(c: Expr, ctx: CheckContext, errMsg: String): Either[Diag, Unit] =
    for
      _ <- checkExprImpl(c, ctx)
      _ <- Result.require(TU.typeOf(c, ctx.methodCtx, ctx.symbols) == assignment3.Type.BOOL,
             TypeDiag(errMsg, ctx.line))
    yield ()

  // ─────────────────────────────────────────────────────────────────────────────
  // Expression Checking
  // ─────────────────────────────────────────────────────────────────────────────

  // Public API - creates context and delegates
  def checkExprE(e: Expr, currentMethod: MethodSymbol, defaultLine: Int, programSymbols: ProgramSymbols): Either[Diag, Unit] =
    checkExprImpl(e, CheckContext(currentMethod, programSymbols, defaultLine))

  private def checkExprImpl(e: Expr, ctx: CheckContext): Either[Diag, Unit] = e match
    case _: (IntLit | BoolLit | StrLit | NullLit | This) => Right(())
    case Var(_, _) => Right(())
    case Unary(_, expr, _, _) => checkExprImpl(expr, ctx)
    case Binary(_, l, r, _, _) => for { _ <- checkExprImpl(l, ctx); _ <- checkExprImpl(r, ctx) } yield ()
    case Ternary(c, t, el, _, _) => for { _ <- checkExprImpl(c, ctx); _ <- checkExprImpl(t, ctx); _ <- checkExprImpl(el, ctx) } yield ()
    case Call(_, args, _) =>
      Result.sequenceE(args)(a => checkExprImpl(a, ctx))
    case NewObject(_, args, _) =>
      Result.sequenceE(args)(a => checkExprImpl(a, ctx))
    case InstanceCall(target, method, args, _) =>
      checkInstanceCallImpl(target, method, args, ctx)
    case FieldAccess(target, field, _, _) =>
      checkFieldAccessImpl(target, field, ctx)

  /** Check instance method call: validates target, arguments, and parameter types. */
  private def checkInstanceCallImpl(target: Expr, method: CallableMethod, args: List[Expr], ctx: CheckContext): Either[Diag, Unit] =
    target match
      case This(_) => Left(SyntaxDiag(Messages.Semantic.thisMethodCallNotAllowed, ctx.line))
      case _ =>
        for
          _ <- NullCheck.requireNonNull(target, SyntaxDiag(Messages.Semantic.nullDerefInstanceCall, ctx.line))
          _ <- checkExprImpl(target, ctx)
          _ <- Result.sequenceE(args)(a => checkExprImpl(a, ctx))
          _ <- method match
            case ic: ScalaInstanceCallable => ParameterValidation.validateArgs(ic.getSymbol, args, ctx)
            case _ => Right(())
        yield ()

  /** Check field access: validates target is non-null. */
  private def checkFieldAccessImpl(target: Expr, field: String, ctx: CheckContext): Either[Diag, Unit] =
    for
      _ <- NullCheck.requireNonNull(target, SyntaxDiag(Messages.Semantic.nullDerefFieldAccess(field), ctx.line))
      _ <- checkExprImpl(target, ctx)
    yield ()

  // ─────────────────────────────────────────────────────────────────────────────
  // Statement Checking
  // ─────────────────────────────────────────────────────────────────────────────

  def checkStmtE(s: Stmt, currentMethod: MethodSymbol, defaultLine: Int, programSymbols: ProgramSymbols): Either[Diag, Unit] =
    checkStmtImpl(s, CheckContext(currentMethod, programSymbols, defaultLine))

  private def checkStmtImpl(s: Stmt, ctx: CheckContext): Either[Diag, Unit] = s match
    case Block(stmts, _) => checkBlockImpl(stmts, ctx)
    case If(c, t, e, _) => checkIfImpl(c, t, e, ctx)
    case While(c, b, _) => checkWhileImpl(c, b, ctx)
    case Break(_) => Right(())
    case Return(v, _) => checkReturnImpl(v, ctx)
    case VarDecl(name, vtypeOpt, valueTypeOpt, initOpt, _) => checkVarDeclImpl(name, vtypeOpt, valueTypeOpt, initOpt, ctx)
    case Assign(varName, value, _) => checkAssignImpl(varName, value, ctx)
    case FieldAssign(target, fieldName, _offset, value, _) => checkFieldAssignImpl(target, fieldName, value, ctx)

  private def checkBlockImpl(stmts: List[Stmt], ctx: CheckContext): Either[Diag, Unit] =
    Result.sequenceE(stmts)(st => checkStmtImpl(st, ctx))

  private def checkIfImpl(cond: Expr, thenB: Stmt, elseB: Stmt, ctx: CheckContext): Either[Diag, Unit] =
    for
      _ <- requireBoolCondition(cond, ctx, Messages.Semantic.ifConditionMustBeBool)
      _ <- checkStmtImpl(thenB, ctx)
      _ <- checkStmtImpl(elseB, ctx)
    yield ()

  private def checkWhileImpl(cond: Expr, body: Stmt, ctx: CheckContext): Either[Diag, Unit] =
    for
      _ <- requireBoolCondition(cond, ctx, Messages.Semantic.whileConditionMustBeBool)
      _ <- checkStmtImpl(body, ctx)
    yield ()

  private def checkReturnImpl(valueOpt: Option[Expr], ctx: CheckContext): Either[Diag, Unit] =
    val rs = ctx.method.getReturnSig
    (valueOpt, rs) match
      case (None, ReturnSig.Void) => Right(())
      case (None, _) => Left(TypeDiag(Messages.Semantic.nonVoidMustReturnValue, ctx.line))
      case (Some(_), ReturnSig.Void) => Left(TypeDiag(Messages.Semantic.voidShouldNotReturn, ctx.line))
      case (Some(expr), ReturnSig.Prim(t)) =>
        for
          _ <- checkExprImpl(expr, ctx)
          _ <- Result.require(t.isCompatibleWith(TU.typeOf(expr, ctx.methodCtx, ctx.symbols)),
                 TypeDiag(Messages.Semantic.returnTypeMismatch, ctx.line))
        yield ()
      case (Some(expr: NullLit), ReturnSig.Obj(_)) => Right(())
      case (Some(expr), ReturnSig.Obj(cn)) =>
        for
          _ <- checkExprImpl(expr, ctx)
          _ <- TU.classNameOf(expr, ctx.methodCtx, ctx.symbols) match
            case Some(ec) if ec != cn => Left(TypeDiag(Messages.Semantic.returnObjectTypeMismatch, ctx.line))
            case _ => Right(())
        yield ()

  private def checkVarDeclImpl(name: String, vtypeOpt: Option[assignment3.Type], valueTypeOpt: Option[assignment3.ValueType], initOpt: Option[Expr], ctx: CheckContext): Either[Diag, Unit] =
    initOpt.fold(Right(()))(init =>
      for
        _ <- checkExprImpl(init, ctx)
        expectedOpt = valueTypeOpt.orElse(vtypeOpt.map(assignment3.ValueType.ofPrimitive))
        _ <- expectedOpt.fold(Right(()))(expected =>
               checkAssignable(expected, init, ctx,
                 objMismatchMsg = Messages.Semantic.objAssignMismatch(name),
                 primMismatchMsg = Messages.Semantic.primAssignMismatch(name)
               ))
      yield ()
    )

  private def checkAssignImpl(varName: String, value: Expr, ctx: CheckContext): Either[Diag, Unit] =
    for
      _ <- checkExprImpl(value, ctx)
      _ <- ctx.method.lookup(varName).fold(Right(()))(vs =>
             checkAssignable(vs.valueType, value, ctx,
               objMismatchMsg = Messages.Semantic.objAssignMismatch(varName),
               primMismatchMsg = Messages.Semantic.primAssignMismatch(varName)
             ))
    yield ()

  private def checkFieldAssignImpl(target: Expr, fieldName: String, value: Expr, ctx: CheckContext): Either[Diag, Unit] =
    for
      _ <- checkExprImpl(target, ctx)
      _ <- checkExprImpl(value, ctx)
      _ <- NullCheck.requireNonNull(target, SyntaxDiag(Messages.Semantic.nullDerefFieldAssign(fieldName), ctx.line))
      _ <- resolveFieldInfo(target, fieldName, ctx).fold(Right(()))(fi =>
             checkAssignable(fi.valueType, value, ctx,
               objMismatchMsg = Messages.Semantic.fieldObjAssignMismatch(fieldName),
               primMismatchMsg = Messages.Semantic.fieldPrimAssignMismatch(fieldName)
             ))
    yield ()
