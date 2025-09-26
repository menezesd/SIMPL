package assignment3.ast

import assignment3.symbol.{MethodSymbol, ProgramSymbols}

/** Idiomatic semantic checker for idiomatic AST. */
object IdiomaticSemantic:
  import IdiomaticTypeUtils as TU
  import assignment3.ast.high.ReturnSig
  import assignment3.ast.{Diag, SyntaxDiag, TypeDiag}

  // Shared helper: ensure RHS is assignable to expected ValueType (primitive/object rules, null allowed for objects).
  private def checkAssignable(
      expected: assignment3.ValueType,
      rhs: Expr,
      ctx: assignment3.ast.NewMethodContext,
      programSymbols: ProgramSymbols,
      defaultLine: Int,
      objMismatchMsg: String,
      primMismatchMsg: String
  ): Either[Diag, Unit] =
    if expected.isObject then
      if rhs.isInstanceOf[NullLit] then Right(())
      else
        TU.classNameOf(rhs, ctx, programSymbols) match
          case Some(rhsClass) =>
            val expectedClass = expected.getObject.getClassName
            if rhsClass == expectedClass then Right(())
            else Left(TypeDiag(objMismatchMsg, defaultLine))
          case None => Right(()) // unknown at this stage; be permissive as before
    else
      val et = TU.typeOf(rhs, ctx, programSymbols)
      if expected.getPrimitive.isCompatibleWith(et) then Right(())
      else Left(TypeDiag(primMismatchMsg, defaultLine))

  // Either-based versions
  def checkExprE(e: Expr, currentMethod: MethodSymbol, defaultLine: Int, programSymbols: ProgramSymbols): Either[Diag, Unit] = e match
    case _: (IntLit | BoolLit | StrLit | NullLit | This) => Right(())
    case Var(_, _) => Right(())
    case Unary(_, expr, _, _) => checkExprE(expr, currentMethod, defaultLine, programSymbols)
    case Binary(_, l, r, _, _) => for { _ <- checkExprE(l, currentMethod, defaultLine, programSymbols); _ <- checkExprE(r, currentMethod, defaultLine, programSymbols) } yield ()
    case Ternary(c, t, el, _, _) => for { _ <- checkExprE(c, currentMethod, defaultLine, programSymbols); _ <- checkExprE(t, currentMethod, defaultLine, programSymbols); _ <- checkExprE(el, currentMethod, defaultLine, programSymbols) } yield ()
    case Call(_, args, _) =>
      args.foldLeft[Either[Diag, Unit]](Right(())) { (acc, a) => acc.flatMap(_ => checkExprE(a, currentMethod, defaultLine, programSymbols)) }
    case NewObject(_, args, _) =>
      args.foldLeft[Either[Diag, Unit]](Right(())) { (acc, a) => acc.flatMap(_ => checkExprE(a, currentMethod, defaultLine, programSymbols)) }
    case InstanceCall(target, method, args, _) =>
      target match
        case This(_) => Left(SyntaxDiag("Explicit 'this.method()' is not allowed in LO-3", defaultLine))
        case NullLit(_) => Left(SyntaxDiag("Null dereference in instance call", defaultLine))
        case _ =>
          val checked = for {
            _ <- checkExprE(target, currentMethod, defaultLine, programSymbols)
            _ <- args.foldLeft[Either[Diag, Unit]](Right(())) { (acc, a) => acc.flatMap(_ => checkExprE(a, currentMethod, defaultLine, programSymbols)) }
          } yield ()
          checked.flatMap { _ =>
            method match
              case ic: assignment3.ast.ScalaInstanceCallable =>
                val ms = ic.getSymbol
                val expected = ms.expectedUserArgs(); val provided = args.size
                if expected != provided then Left(SyntaxDiag("Incorrect number of arguments for instance method", defaultLine))
                else
                  val ctx = new assignment3.ast.NewMethodContext(currentMethod, programSymbols)
                  val firstErr: Option[Diag] = (0 until provided).iterator.flatMap { i =>
                    val formal = ms.parameters(i + 1)
                    val argType = TU.typeOf(args(i), ctx, programSymbols)
                    if !formal.getType.isCompatibleWith(argType) then
                      Some(TypeDiag("Argument type mismatch for parameter '" + formal.getName + "'", defaultLine))
                    else None
                  }.toSeq.headOption
                  firstErr.map(Left(_)).getOrElse(Right(()))
              case _ => Right(())
          }
    case FieldAccess(target, field, _, _) =>
      target match
        case NullLit(_) => Left(SyntaxDiag("Null dereference in field access '" + field + "'", defaultLine))
        case _ => checkExprE(target, currentMethod, defaultLine, programSymbols)

  def checkStmtE(s: Stmt, currentMethod: MethodSymbol, defaultLine: Int, programSymbols: ProgramSymbols): Either[Diag, Unit] = s match
    case Block(stmts, _) =>
      stmts.foldLeft[Either[Diag, Unit]](Right(())) { (acc, st) => acc.flatMap(_ => checkStmtE(st, currentMethod, defaultLine, programSymbols)) }
    case If(c, t, e, _) =>
      val ctx = new assignment3.ast.NewMethodContext(currentMethod, programSymbols)
      for {
        _ <- checkExprE(c, currentMethod, defaultLine, programSymbols)
        _ <- if assignment3.ast.IdiomaticTypeUtils.typeOf(c, ctx, programSymbols) == assignment3.Type.BOOL then Right(()) else Left(TypeDiag("If condition must be BOOL", defaultLine))
        _ <- checkStmtE(t, currentMethod, defaultLine, programSymbols)
        _ <- checkStmtE(e, currentMethod, defaultLine, programSymbols)
      } yield ()
    case While(c, b, _) =>
      val ctx = new assignment3.ast.NewMethodContext(currentMethod, programSymbols)
      for {
        _ <- checkExprE(c, currentMethod, defaultLine, programSymbols)
        _ <- if assignment3.ast.IdiomaticTypeUtils.typeOf(c, ctx, programSymbols) == assignment3.Type.BOOL then Right(()) else Left(TypeDiag("While condition must be BOOL", defaultLine))
        _ <- checkStmtE(b, currentMethod, defaultLine, programSymbols)
      } yield ()
    case Break(_) => Right(())
    case Return(v, _) =>
      val ctx = new assignment3.ast.NewMethodContext(currentMethod, programSymbols)
      val rs = currentMethod.getReturnSig
      v match
        case None =>
          rs match
            case ReturnSig.Void => Right(())
            case _ => Left(TypeDiag("Non-void method must return a value", defaultLine))
        case Some(expr) =>
          for {
            _ <- checkExprE(expr, currentMethod, defaultLine, programSymbols)
            _ <- rs match
              case ReturnSig.Void => Left(TypeDiag("Void method should not return a value", defaultLine))
              case ReturnSig.Prim(t) =>
                val et = TU.typeOf(expr, ctx, programSymbols)
                if t.isCompatibleWith(et) then Right(()) else Left(TypeDiag("Return type mismatch", defaultLine))
              case ReturnSig.Obj(cn) =>
                if expr.isInstanceOf[NullLit] then Right(())
                else TU.classNameOf(expr, ctx, programSymbols) match
                  case Some(ec) => if ec == cn then Right(()) else Left(TypeDiag("Return object type mismatch", defaultLine))
                  case None => Right(())
          } yield ()
    case VarDecl(name, vtypeOpt, valueTypeOpt, initOpt, _) =>
      initOpt
        .map { init =>
          val ctx = new assignment3.ast.NewMethodContext(currentMethod, programSymbols)
          for {
            _ <- checkExprE(init, currentMethod, defaultLine, programSymbols)
            expectedOpt = valueTypeOpt.orElse(vtypeOpt.map(assignment3.ValueType.ofPrimitive))
            _ <- expectedOpt
              .map { expected =>
                checkAssignable(expected, init, ctx, programSymbols, defaultLine,
                  objMismatchMsg = "Object assignment type mismatch for variable '" + name + "'",
                  primMismatchMsg = "Type mismatch in assignment to '" + name + "'"
                )
              }
              .getOrElse(Right(()))
          } yield ()
        }
        .getOrElse(Right(()))
    case Assign(varName, value, _) =>
      for {
        _ <- checkExprE(value, currentMethod, defaultLine, programSymbols)
        res <- Option(currentMethod).fold[Either[Diag, Unit]](Right(())) { cm =>
          cm.lookup(varName) match
            case Some(vs) =>
              val ctx = new assignment3.ast.NewMethodContext(cm, programSymbols)
              checkAssignable(
                vs.getValueType,
                value,
                ctx,
                programSymbols,
                defaultLine,
                objMismatchMsg = "Object assignment type mismatch for variable '" + varName + "'",
                primMismatchMsg = "Type mismatch in assignment to '" + varName + "'"
              )
            case None => Right(())
        }
      } yield ()
    case FieldAssign(target, fieldName, _offset, value, _) =>
      for {
        _ <- checkExprE(target, currentMethod, defaultLine, programSymbols)
        _ <- checkExprE(value, currentMethod, defaultLine, programSymbols)
        _ <- target match
          case _: NullLit => Left(SyntaxDiag("Null dereference in field assignment '" + fieldName + "'", defaultLine))
          case _ => Right(())
        _ <-
          val ctx = new assignment3.ast.NewMethodContext(currentMethod, programSymbols)
          val classNameOpt = TU.classNameOf(target, ctx, programSymbols)
          (for {
            cn <- classNameOpt
            cs <- programSymbols.getClass(cn)
            fi <- cs.getFieldInfo(fieldName)
          } yield fi) match
            case Some(fi) =>
              checkAssignable(
                fi.valueType,
                value,
                ctx,
                programSymbols,
                defaultLine,
                objMismatchMsg = "Object assignment type mismatch for field '" + fieldName + "'",
                primMismatchMsg = "Type mismatch assigning to field '" + fieldName + "'"
              )
            case None => Right(())
      } yield ()
