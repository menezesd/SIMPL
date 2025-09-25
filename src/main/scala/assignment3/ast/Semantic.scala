package assignment3.ast

import assignment3.{CompilerException, SyntaxErrorException, TypeErrorException}
import assignment3.symbol.{MethodSymbol, ProgramSymbols}

/** Idiomatic semantic checker for idiomatic AST. */
object IdiomaticSemantic:
  import IdiomaticTypeUtils as TU

  def checkExpr(e: Expr, currentMethod: MethodSymbol, defaultLine: Int, programSymbols: ProgramSymbols): Unit = e match
    case _: (IntLit | BoolLit | StrLit | NullLit | This) => ()
    case Var(_, _) => ()
    case Unary(_, expr, _, _) => checkExpr(expr, currentMethod, defaultLine, programSymbols)
    case Binary(_, l, r, _, _) => checkExpr(l, currentMethod, defaultLine, programSymbols); checkExpr(r, currentMethod, defaultLine, programSymbols)
    case Ternary(c, t, el, _, _) => checkExpr(c, currentMethod, defaultLine, programSymbols); checkExpr(t, currentMethod, defaultLine, programSymbols); checkExpr(el, currentMethod, defaultLine, programSymbols)
    case Call(_, args, _) => args.foreach(a => checkExpr(a, currentMethod, defaultLine, programSymbols))
    case NewObject(_, args, _) => args.foreach(a => checkExpr(a, currentMethod, defaultLine, programSymbols))
    case InstanceCall(target, method, args, _) =>
      target match
        case This(_) => throw new SyntaxErrorException("Explicit 'this.method()' is not allowed in LO-3", defaultLine)
        // No explicit 'this' check needed for idiomatic Var
        case NullLit(_) => throw new SyntaxErrorException("Null dereference in instance call", defaultLine)
        case _ => ()
      checkExpr(target, currentMethod, defaultLine, programSymbols); args.foreach(a => checkExpr(a, currentMethod, defaultLine, programSymbols))
      method match
        case ic: assignment3.ast.ScalaInstanceCallable =>
          val ms = ic.getSymbol
          val expected = ms.expectedUserArgs(); val provided = args.size
          if expected != provided then throw new SyntaxErrorException("Incorrect number of arguments for instance method", defaultLine)
          var i = 0
          while i < provided do
            val formal = ms.getParameters.get(i + 1)
            val argType = TU.typeOf(args(i), new assignment3.ast.NewMethodContext(currentMethod, programSymbols), programSymbols)
            if !formal.getType.isCompatibleWith(argType) then
              throw new TypeErrorException("Argument type mismatch for parameter '" + formal.getName + "'", defaultLine)
            i += 1
        case _ => ()
    case FieldAccess(target, field, _, _) =>
      target match
        case NullLit(_) => throw new SyntaxErrorException("Null dereference in field access '" + field + "'", defaultLine)
        case _ => ()
      checkExpr(target, currentMethod, defaultLine, programSymbols)

  def checkStmt(s: Stmt, currentMethod: MethodSymbol, defaultLine: Int, programSymbols: ProgramSymbols): Unit = s match
    case Block(stmts, _) => stmts.foreach(st => checkStmt(st, currentMethod, defaultLine, programSymbols))
    case If(c, t, e, _) => checkExpr(c, currentMethod, defaultLine, programSymbols); checkStmt(t, currentMethod, defaultLine, programSymbols); checkStmt(e, currentMethod, defaultLine, programSymbols)
    case While(c, b, _) => checkExpr(c, currentMethod, defaultLine, programSymbols); checkStmt(b, currentMethod, defaultLine, programSymbols)
    case Break(_) => ()
    case Return(v, _) => v.foreach(expr => checkExpr(expr, currentMethod, defaultLine, programSymbols))
    case VarDecl(name, vtypeOpt, valueTypeOpt, initOpt, _) =>
      initOpt.foreach(init => checkExpr(init, currentMethod, defaultLine, programSymbols))
      initOpt.foreach { init =>
        if valueTypeOpt.exists(_.isObject) then
          val rhsClass = TU.classNameOf(init, new assignment3.ast.NewMethodContext(currentMethod, programSymbols), programSymbols)
          val isNull = init.isInstanceOf[NullLit]
          if !isNull then
            if rhsClass == null then () // allow unresolved object at this stage
            else if valueTypeOpt.get.getObject.getClassName != rhsClass then
              throw new TypeErrorException("Object assignment type mismatch for variable '" + name + "'", defaultLine)
        else if vtypeOpt.exists(vt => !vt.isCompatibleWith(TU.typeOf(init, new assignment3.ast.NewMethodContext(currentMethod, programSymbols), programSymbols))) then
          throw new TypeErrorException("Type mismatch in assignment to '" + name + "'", defaultLine)
      }
    case Assign(varName, value, _) =>
      checkExpr(value, currentMethod, defaultLine, programSymbols)
      if currentMethod != null then
        val it = currentMethod.getLocals.iterator()
        while it.hasNext do
          val vs = it.next()
          if vs.getName == varName then
            if vs.isObject then
              val rhsClass = TU.classNameOf(value, new assignment3.ast.NewMethodContext(currentMethod, programSymbols), programSymbols)
              val isNull = value.isInstanceOf[NullLit]
              if !isNull then
                if rhsClass == null then ()
                else if vs.getClassTypeName != rhsClass then
                  throw new TypeErrorException("Object assignment type mismatch for variable '" + varName + "'", defaultLine)
            else if !vs.getType.isCompatibleWith(TU.typeOf(value, new assignment3.ast.NewMethodContext(currentMethod, programSymbols), programSymbols)) then
              throw new TypeErrorException("Type mismatch in assignment to '" + varName + "'", defaultLine)
            return
    case FieldAssign(target, fieldName, _offset, value, _) =>
      checkExpr(target, currentMethod, defaultLine, programSymbols); checkExpr(value, currentMethod, defaultLine, programSymbols)
      target match
        case _: NullLit => throw new SyntaxErrorException("Null dereference in field assignment '" + fieldName + "'", defaultLine)
        case _ => ()
      if programSymbols != null then
        val className = TU.classNameOf(target, new assignment3.ast.NewMethodContext(currentMethod, programSymbols), programSymbols)
        if className != null then
          val cs = programSymbols.getClass(className)
          if cs != null then
            val fi = cs.getFieldInfo(fieldName)
            if fi != null then
              if fi.valueType != null && fi.valueType.isObject then
                val rhsClass = TU.classNameOf(value, new assignment3.ast.NewMethodContext(currentMethod, programSymbols), programSymbols)
                val isNull = value.isInstanceOf[NullLit]
                if !isNull then
                  if rhsClass == null then ()
                  else if fi.valueType.getObject.getClassName != rhsClass then
                    throw new TypeErrorException("Object assignment type mismatch for field '" + fieldName + "'", defaultLine)
              else if !fi.valueType.getPrimitive.isCompatibleWith(TU.typeOf(value, new assignment3.ast.NewMethodContext(currentMethod, programSymbols), programSymbols)) then
                throw new TypeErrorException("Type mismatch assigning to field '" + fieldName + "'", defaultLine)
