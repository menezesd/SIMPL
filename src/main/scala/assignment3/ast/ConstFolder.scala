package assignment3.ast

import assignment3.Type

// New idiomatic constant folder with pattern matching
object IdiomaticConstFolder:
  import assignment3.ast._

  def foldExpr(e: Expr): Expr = e match
    case i: IntLit  => i
    case b: BoolLit => b
    case s: StrLit  => s
    case n: NullLit => n
    case v: Var     => v
    case Unary(UnaryOp.Neg, IntLit(v, p), _, _) => IntLit(-v, p)
    case Unary(UnaryOp.Neg, StrLit(s, p), _, _) => StrLit(s, p) // identity for string neg per legacy '~'
    case Unary(UnaryOp.Not, BoolLit(v, p), _, _) => BoolLit(!v, p)
    case Unary(op, ex, rt, p) =>
      val fx = foldExpr(ex); if (fx eq ex) Unary(op, ex, rt, p) else Unary(op, fx, rt, p)
    case Binary(op, l, r, rt, p) =>
      val fl = foldExpr(l); val fr = foldExpr(r)
      (op, fl, fr) match
        case (BinaryOp.Add, StrLit(a,_), StrLit(b,_)) => StrLit(a + b, p)
        case (BinaryOp.Mul, StrLit(a,_), IntLit(n,_)) => StrLit(a * n, p)
        case (BinaryOp.Mul, IntLit(n,_), StrLit(a,_)) => StrLit(a * n, p)
        case _ =>
          (fl, fr) match
            case (IntLit(a,_), IntLit(b,_)) => op match
              case BinaryOp.Add => IntLit(a + b, p)
              case BinaryOp.Sub => IntLit(a - b, p)
              case BinaryOp.Mul => IntLit(a * b, p)
              case BinaryOp.Div if b != 0 => IntLit(a / b, p)
              case BinaryOp.Mod if b != 0 => IntLit(a % b, p)
              case BinaryOp.Lt  => BoolLit(a < b, p)
              case BinaryOp.Gt  => BoolLit(a > b, p)
              case BinaryOp.Eq  => BoolLit(a == b, p)
              case _ => Binary(op, fl, fr, rt, p)
            case (BoolLit(a,_), BoolLit(b,_)) => op match
              case BinaryOp.And => BoolLit(a && b, p)
              case BinaryOp.Or  => BoolLit(a || b, p)
              case BinaryOp.Eq  => BoolLit(a == b, p)
              case _ => Binary(op, fl, fr, rt, p)
            case (StrLit(a,_), StrLit(b,_)) => op match
              case BinaryOp.Lt => BoolLit(a < b, p)
              case BinaryOp.Gt => BoolLit(a > b, p)
              case BinaryOp.Eq => BoolLit(a == b, p)
              case _ => Binary(op, fl, fr, rt, p)
            case _ =>
              if ((fl eq l) && (fr eq r)) Binary(op, l, r, rt, p) else Binary(op, fl, fr, rt, p)
    case Ternary(c, t, e, rt, p) =>
      val fc = foldExpr(c)
      fc match
        case BoolLit(true, _)  => foldExpr(t)
        case BoolLit(false, _) => foldExpr(e)
        case _ =>
          val ft = foldExpr(t); val fe = foldExpr(e)
          if ((fc eq c) && (ft eq t) && (fe eq e)) Ternary(c, t, e, rt, p) else Ternary(fc, ft, fe, rt, p)
    case Call(m, args, p) =>
      val fa = args.map(foldExpr)
      if (fa eq args) Call(m, args, p) else Call(m, fa, p)
    case This(_) => e
    case NewObject(cn, args, p) =>
      val fa = args.map(foldExpr)
      if (fa eq args) NewObject(cn, args, p) else NewObject(cn, fa, p)
    case InstanceCall(t, m, args, p) =>
      val ft = foldExpr(t); val fa = args.map(foldExpr)
      if ((ft eq t) && (fa eq args)) InstanceCall(t, m, args, p) else InstanceCall(ft, m, fa, p)
    case FieldAccess(t, f, info, p) =>
      val ft = foldExpr(t)
      if (ft eq t) FieldAccess(t, f, info, p) else FieldAccess(ft, f, info, p)

  def foldStmt(s: Stmt): Stmt = s match
    case Block(stmts, p) =>
      val fs = stmts.map(foldStmt)
      if (fs eq stmts) Block(stmts, p) else Block(fs, p)
    case VarDecl(n, vt, vvt, init, p) =>
      val fi = init.map(foldExpr)
      if (fi == init) VarDecl(n, vt, vvt, init, p) else VarDecl(n, vt, vvt, fi, p)
    case Assign(n, v, p) =>
      val fv = foldExpr(v); if (fv eq v) s else Assign(n, fv, p)
    case If(c, t, e, p) =>
      val fc = foldExpr(c); val ft = foldStmt(t); val fe = foldStmt(e)
      if ((fc eq c) && (ft eq t) && (fe eq e)) s else If(fc, ft, fe, p)
    case While(c, b, p) =>
      val fc = foldExpr(c); val fb = foldStmt(b)
      if ((fc eq c) && (fb eq b)) s else While(fc, fb, p)
    case Break(_) => s
    case Return(v, p) =>
      val fv = v.map(foldExpr)
      if (fv == v) s else Return(fv, p)
    case FieldAssign(t, f, off, v, p) =>
      val ft = foldExpr(t); val fv = foldExpr(v)
      if ((ft eq t) && (fv eq v)) s else FieldAssign(ft, f, off, fv, p)


