package assignment3.ast

import assignment3.symbol.{MethodSymbol, ProgramSymbols}
import assignment3.{Messages, PrimitiveType, ObjectRefType}

/** Idiomatic semantic checker for idiomatic AST. */
object IdiomaticSemantic:
  import IdiomaticTypeUtils as TU
  import assignment3.ast.high.ReturnSig
  import assignment3.ast.{Diag, SyntaxDiag, TypeDiag}

  /** Semantic checking context - caches NewMethodContext to avoid repeated allocation. */
  private final class CheckContext(val method: MethodSymbol, val symbols: ProgramSymbols, val line: Int):
    lazy val methodCtx: NewMethodContext = new NewMethodContext(method, symbols)

  // Shared helper: ensure RHS is assignable to expected ValueType (primitive/object rules, null allowed for objects).
  private def checkAssignable(
      expected: assignment3.ValueType,
      rhs: Expr,
      ctx: CheckContext,
      objMismatchMsg: String,
      primMismatchMsg: String
  ): Either[Diag, Unit] =
    expected match {
      case ObjectRefType(ot) =>
        if rhs.isInstanceOf[NullLit] then Right(())
        else
          TU.classNameOf(rhs, ctx.methodCtx, ctx.symbols) match
            case Some(rhsClass) =>
              if rhsClass == ot.getClassName then Right(())
              else Left(TypeDiag(objMismatchMsg, ctx.line))
            case None => Right(()) // unknown at this stage; be permissive as before
      case PrimitiveType(pt) =>
        val et = TU.typeOf(rhs, ctx.methodCtx, ctx.symbols)
        if pt.isCompatibleWith(et) then Right(())
        else Left(TypeDiag(primMismatchMsg, ctx.line))
    }

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
      target match
        case This(_) => Left(SyntaxDiag(Messages.Semantic.thisMethodCallNotAllowed, ctx.line))
        case NullLit(_) => Left(SyntaxDiag(Messages.Semantic.nullDerefInstanceCall, ctx.line))
        case _ =>
          val checked = for {
            _ <- checkExprImpl(target, ctx)
            _ <- Result.sequenceE(args)(a => checkExprImpl(a, ctx))
          } yield ()
          checked.flatMap { _ =>
            method match
              case ic: assignment3.ast.ScalaInstanceCallable =>
                val ms = ic.getSymbol
                val expected = ms.expectedUserArgs(); val provided = args.size
                if expected != provided then Left(SyntaxDiag(Messages.Semantic.incorrectArgCount, ctx.line))
                else
                  val firstErr: Option[Diag] = (0 until provided).iterator.flatMap { i =>
                    val formal = ms.parameters(i + 1)
                    val argType = TU.typeOf(args(i), ctx.methodCtx, ctx.symbols)
                    formal.valueType match {
                      case PrimitiveType(pt) if !pt.isCompatibleWith(argType) =>
                        Some(TypeDiag(Messages.Semantic.argTypeMismatch(formal.getName), ctx.line))
                      case _ => None
                    }
                  }.nextOption()
                  firstErr.map(Left(_)).getOrElse(Right(()))
              case _ => Right(())
          }
    case FieldAccess(target, field, _, _) =>
      target match
        case NullLit(_) => Left(SyntaxDiag(Messages.Semantic.nullDerefFieldAccess(field), ctx.line))
        case _ => checkExprImpl(target, ctx)

  def checkStmtE(s: Stmt, currentMethod: MethodSymbol, defaultLine: Int, programSymbols: ProgramSymbols): Either[Diag, Unit] =
    checkStmtImpl(s, CheckContext(currentMethod, programSymbols, defaultLine))

  private def checkStmtImpl(s: Stmt, ctx: CheckContext): Either[Diag, Unit] = s match
    case Block(stmts, _) =>
      Result.sequenceE(stmts)(st => checkStmtImpl(st, ctx))
    case If(c, t, e, _) =>
      for {
        _ <- checkExprImpl(c, ctx)
        _ <- if TU.typeOf(c, ctx.methodCtx, ctx.symbols) == assignment3.Type.BOOL then Right(())
             else Left(TypeDiag(Messages.Semantic.ifConditionMustBeBool, ctx.line))
        _ <- checkStmtImpl(t, ctx)
        _ <- checkStmtImpl(e, ctx)
      } yield ()
    case While(c, b, _) =>
      for {
        _ <- checkExprImpl(c, ctx)
        _ <- if TU.typeOf(c, ctx.methodCtx, ctx.symbols) == assignment3.Type.BOOL then Right(())
             else Left(TypeDiag(Messages.Semantic.whileConditionMustBeBool, ctx.line))
        _ <- checkStmtImpl(b, ctx)
      } yield ()
    case Break(_) => Right(())
    case Return(v, _) =>
      val rs = ctx.method.getReturnSig
      v match
        case None =>
          rs match
            case ReturnSig.Void => Right(())
            case _ => Left(TypeDiag(Messages.Semantic.nonVoidMustReturnValue, ctx.line))
        case Some(expr) =>
          for {
            _ <- checkExprImpl(expr, ctx)
            _ <- rs match
              case ReturnSig.Void => Left(TypeDiag(Messages.Semantic.voidShouldNotReturn, ctx.line))
              case ReturnSig.Prim(t) =>
                val et = TU.typeOf(expr, ctx.methodCtx, ctx.symbols)
                if t.isCompatibleWith(et) then Right(()) else Left(TypeDiag(Messages.Semantic.returnTypeMismatch, ctx.line))
              case ReturnSig.Obj(cn) =>
                if expr.isInstanceOf[NullLit] then Right(())
                else TU.classNameOf(expr, ctx.methodCtx, ctx.symbols) match
                  case Some(ec) => if ec == cn then Right(()) else Left(TypeDiag(Messages.Semantic.returnObjectTypeMismatch, ctx.line))
                  case None => Right(())
          } yield ()
    case VarDecl(name, vtypeOpt, valueTypeOpt, initOpt, _) =>
      initOpt
        .map { init =>
          for {
            _ <- checkExprImpl(init, ctx)
            expectedOpt = valueTypeOpt.orElse(vtypeOpt.map(assignment3.ValueType.ofPrimitive))
            _ <- expectedOpt
              .map { expected =>
                checkAssignable(expected, init, ctx,
                  objMismatchMsg = Messages.Semantic.objAssignMismatch(name),
                  primMismatchMsg = Messages.Semantic.primAssignMismatch(name)
                )
              }
              .getOrElse(Right(()))
          } yield ()
        }
        .getOrElse(Right(()))
    case Assign(varName, value, _) =>
      for {
        _ <- checkExprImpl(value, ctx)
        res <- ctx.method.lookup(varName) match
          case Some(vs) =>
            checkAssignable(vs.valueType, value, ctx,
              objMismatchMsg = Messages.Semantic.objAssignMismatch(varName),
              primMismatchMsg = Messages.Semantic.primAssignMismatch(varName)
            )
          case None => Right(())
      } yield ()
    case FieldAssign(target, fieldName, _offset, value, _) =>
      for {
        _ <- checkExprImpl(target, ctx)
        _ <- checkExprImpl(value, ctx)
        _ <- target match
          case _: NullLit => Left(SyntaxDiag(Messages.Semantic.nullDerefFieldAssign(fieldName), ctx.line))
          case _ => Right(())
        _ <-
          val classNameOpt = TU.classNameOf(target, ctx.methodCtx, ctx.symbols)
          (for {
            cn <- classNameOpt
            cs <- ctx.symbols.getClass(cn)
            fi <- cs.getFieldInfo(fieldName)
          } yield fi) match
            case Some(fi) =>
              checkAssignable(fi.valueType, value, ctx,
                objMismatchMsg = Messages.Semantic.fieldObjAssignMismatch(fieldName),
                primMismatchMsg = Messages.Semantic.fieldPrimAssignMismatch(fieldName)
              )
            case None => Right(())
      } yield ()
