package assignment3.ast

import assignment3._
import assignment3.ast as id
import assignment3.ast.IdiomaticTypeUtils
import assignment3.symbol.ProgramSymbols
import assignment3.ast.MethodFrame
import assignment3.ast.{NewMethodContext, SymbolMethodFrame}
import scala.collection.mutable.ArrayDeque
import scala.jdk.CollectionConverters._

/** Code generation using idiomatic, pattern-matching AST. */
object IdiomaticCodegen:
  final case class Ctx(
    frame: MethodFrame | Null,
    programSymbols: ProgramSymbols | Null,
    loopEndLabels: ArrayDeque[Label] = ArrayDeque.empty[Label],
    returnLabel: Label | Null = null
  )

  private def tOf(e: id.Expr, ictx: Ctx): assignment3.Type =
    if ictx.frame == null || ictx.programSymbols == null then Type.INT
    else
      ictx.frame match
        case smf: SymbolMethodFrame => IdiomaticTypeUtils.typeOf(e, new NewMethodContext(smf.getSymbol, ictx.programSymbols), ictx.programSymbols)
        case _ => Type.INT

  def emitExpr(e: id.Expr, ctx: Ctx): String = e match
    case id.IntLit(v, _)  => s"PUSHIMM ${v}\n"
    case id.BoolLit(v, _) => if v then "PUSHIMM 1\n" else "PUSHIMM 0\n"
    case id.StrLit(v, _)  => s"PUSHIMMSTR \"${v}\"\n"
    case id.NullLit(_)    => "PUSHIMM 0\n"
    case id.Var(name, _) =>
      if ctx.frame != null then
        val vb = ctx.frame.lookupVar(name)
        if (vb == null) throw new CompilerException("Undeclared variable: " + name, -1)
        s"PUSHOFF ${vb.getAddress}\n"
      else "PUSHOFF 0\n" // best-effort fallback
    case id.FieldAccess(target, _field, fieldInfo, _) =>
      val off = fieldInfo.map(_.offset).getOrElse(-1)
      if off < 0 then throw new CompilerException("Unknown field offset for field: " + _field, -1)
      val sam = new StringBuilder
      sam.append(emitExpr(target, ctx))
      sam.append("PUSHIMM ").append(off).append("\nADD\nPUSHIND\n")
      sam.toString
    case id.Unary(op, expr, rt, _) =>
      val base = emitExpr(expr, ctx)
      op match
        case UnaryOp.Neg =>
          if rt.exists(_ == Type.STRING) then base + StringRuntime.reverseString() else base + OperatorUtils.getUnop('~')
        case UnaryOp.Not => base + OperatorUtils.getUnop('!')
    case id.Binary(op, left, right, _, _) =>
      val lt = tOf(left, ctx); val rt = tOf(right, ctx)
      val leftCode = emitExpr(left, ctx); val rightCode = emitExpr(right, ctx)
      // Short-circuit && and || implemented as '&' and '|'
      if ((op == BinaryOp.And || op == BinaryOp.Or) && lt == Type.BOOL && rt == Type.BOOL) then
        val sb = new SamBuilder()
        if (op == BinaryOp.And) then
          val falseLbl = new Label(); val endLbl = new Label()
          sb.append(leftCode).append("ISNIL\nJUMPC ").append(falseLbl.getName).append("\n")
          sb.append(rightCode).append("ISNIL\nJUMPC ").append(falseLbl.getName).append("\n")
          sb.append("PUSHIMM 1\nJUMP ").append(endLbl.getName).append("\n")
          sb.label(falseLbl.getName).append("PUSHIMM 0\n").label(endLbl.getName)
          return sb.toString
        else
          val needRight = new Label(); val falseLbl = new Label(); val endLbl = new Label()
          sb.append(leftCode).append("ISNIL\nJUMPC ").append(needRight.getName).append("\n")
          sb.append("PUSHIMM 1\nJUMP ").append(endLbl.getName).append("\n")
          sb.label(needRight.getName)
          sb.append(rightCode).append("ISNIL\nJUMPC ").append(falseLbl.getName).append("\n")
          sb.append("PUSHIMM 1\nJUMP ").append(endLbl.getName).append("\n")
          sb.label(falseLbl.getName).append("PUSHIMM 0\n").label(endLbl.getName)
          return sb.toString
      // String-special cases
      if (lt == Type.STRING || rt == Type.STRING) then
        op match
          case BinaryOp.Mul if (lt == Type.STRING && rt == Type.INT) || (lt == Type.INT && rt == Type.STRING) =>
            return leftCode + rightCode + StringRuntime.repeatString(lt, rt)
          case BinaryOp.Add =>
            if (lt == Type.STRING && rt == Type.STRING) then return leftCode + rightCode + StringRuntime.concatString()
            else throw new TypeErrorException("'+' only defined for String+String or numeric addition", -1)
          case BinaryOp.Lt | BinaryOp.Gt | BinaryOp.Eq =>
            if (lt == Type.STRING && rt == Type.STRING) then
              val ch = op match
                case BinaryOp.Lt => '<'
                case BinaryOp.Gt => '>'
                case _           => '='
              return leftCode + rightCode + StringRuntime.compareString(ch)
            else throw new TypeErrorException("String comparison requires both operands String", -1)
          case _ => throw new TypeErrorException("Unsupported operator for String operands", -1)
      val ch = op match
        case BinaryOp.Add => '+'
        case BinaryOp.Sub => '-'
        case BinaryOp.Mul => '*'
        case BinaryOp.Div => '/'
        case BinaryOp.Mod => '%'
        case BinaryOp.And => '&'
        case BinaryOp.Or  => '|'
        case BinaryOp.Eq  => '='
        case BinaryOp.Lt  => '<'
        case BinaryOp.Gt  => '>'
        case BinaryOp.Le  => throw new TypeErrorException("'<= not supported by runtime'", -1)
        case BinaryOp.Ge  => throw new TypeErrorException(">= not supported by runtime", -1)
        case BinaryOp.Ne  => throw new TypeErrorException("!= not supported by runtime", -1)
        case BinaryOp.Concat => throw new TypeErrorException("concat is internal-only for strings", -1)
      leftCode + rightCode + OperatorUtils.getBinop(ch)
    case id.Ternary(cond, thenExpr, elseExpr, _, _) =>
      val condCode = emitExpr(cond, ctx)
      val thenCode = emitExpr(thenExpr, ctx)
      val elseCode = emitExpr(elseExpr, ctx)
      val falseLabel = new Label(); val endLabel = new Label()
      val sb = new SamBuilder()
      sb.append(condCode).append("ISNIL\nJUMPC ").append(falseLabel.getName).append("\n")
        .append(thenCode).append("JUMP ").append(endLabel.getName).append("\n")
        .label(falseLabel.getName).append(elseCode).label(endLabel.getName)
      sb.toString
    case id.Call(m, args, _) =>
      val sb = new SamBuilder()
      val hasReturn = m.getReturnType != null // null means void; primitive/object both occupy 1 slot
      // Always push a return slot; callee always stores into it. Drop it later if void.
      sb.append("PUSHIMM 0\n")
      args.foreach(a => sb.append(emitExpr(a, ctx)))
      val paramCount = args.size
      sb.append(emitCall(m.getName, paramCount, hasReturn = hasReturn))
      sb.toString
    case id.This(_) =>
      // Load implicit 'this' using the frame's binding to get the correct offset
      if ctx.frame != null then
        val vb = ctx.frame.lookupVar("this")
        if (vb == null) throw new CompilerException("'this' not found in current frame", -1)
        s"PUSHOFF ${vb.getAddress}\n"
      else
        // Fallback (shouldn't happen in method bodies); assume closest slot
        "PUSHOFF -1\n"
    case id.NewObject(className, args, _) =>
      val ps = ctx.programSymbols
      val cs = if ps != null then ps.getClass(className) else null
      val numFields = if cs != null then cs.numFields() else 1
      val sb = new SamBuilder()
      // allocate object
      sb.append("PUSHIMM ").append(Integer.toString(numFields)).append("\nMALLOC\n")
      if cs != null && cs.getMethod(className) != null then
        // Keep a copy of the object pointer as the expression result
        sb.append("DUP\n")                 // [obj_saved, obj]
        // Push return slot and place it below 'this'
        sb.append("PUSHIMM 0\nSWAP\n")     // [obj_saved, ret_slot, obj]
        // Now push user args
        args.foreach(a => sb.append(emitExpr(a, ctx)))
        // Call constructor (void)
        sb.append("LINK\nJSR ").append(className).append("_").append(className).append("\nUNLINK\n")
        // Pop parameters: 'this' + args, then drop the unused return slot
        val paramCount = args.size + 1
        sb.append("ADDSP -").append(Integer.toString(paramCount)).append("\n")
        sb.append("ADDSP -1\n")
      sb.toString
    case id.InstanceCall(target, m, args, _) =>
      val sb = new SamBuilder()
      val hasReturn = m.getReturnType != null
      // Always push a return slot; drop after if void.
      sb.append("PUSHIMM 0\n")
      sb.append(emitExpr(target, ctx))
      args.foreach(a => sb.append(emitExpr(a, ctx)))
      val paramCount = args.size + 1 // include implicit 'this'
      sb.append(emitCall(m.getName, paramCount, hasReturn))
      sb.toString

  private def emitCall(label: String, paramCount: Int, hasReturn: Boolean): String =
    val sb = new StringBuilder
    sb.append("LINK\n")
    sb.append("JSR ").append(label).append("\n")
    sb.append("UNLINK\n")
    sb.append("ADDSP -").append(paramCount).append("\n")
    // Drop return slot for void calls; keep it for non-void as the expression result
    if !hasReturn then sb.append("ADDSP -1\n")
    sb.toString

  def emitStmt(s: id.Stmt, ctx: Ctx): String = s match
    case id.Block(statements, _) =>
      val sb = new StringBuilder
      statements.foreach(st => sb.append(emitStmt(st, ctx)))
      sb.toString
    case id.VarDecl(_, _, _, initOpt, _) =>
      initOpt.map(emitExpr(_, ctx)).getOrElse("PUSHIMM 0\n")
    case id.Assign(name, value, _) =>
      if ctx.frame == null then throw new CompilerException("Missing frame for assignment codegen", -1)
      val vb = ctx.frame.lookupVar(name)
      if (vb == null) throw new CompilerException("Undeclared variable: " + name, -1)
      val code = emitExpr(value, ctx)
      code + s"STOREOFF ${vb.getAddress}\n"
    case id.FieldAssign(target, _field, offset, value, _) =>
      if offset < 0 then throw new CompilerException("Unknown field offset for field: " + _field, -1)
      val sb = new StringBuilder
      sb.append(emitExpr(target, ctx))
        .append("PUSHIMM ").append(offset).append("\nADD\n")
        .append(emitExpr(value, ctx))
        .append("STOREIND\n")
      sb.toString
    case id.If(cond, thenB, elseB, _) =>
      val elseLbl = new Label(); val endLbl = new Label()
      val condCode = emitExpr(cond, ctx)
      val sb = new StringBuilder
      sb.append(condCode).append("ISNIL\nJUMPC ").append(elseLbl.getName).append("\n")
        .append(emitStmt(thenB, ctx))
        .append("JUMP ").append(endLbl.getName).append("\n")
        .append(elseLbl.getName).append(":\n")
        .append(emitStmt(elseB, ctx))
        .append(endLbl.getName).append(":\n")
      sb.toString
    case id.While(cond, body, _) =>
      val start = new Label(); val stop = new Label()
      ctx.loopEndLabels.append(stop)
      val sb = new StringBuilder
      sb.append(start.getName).append(":\n")
        .append(emitExpr(cond, ctx))
        .append("ISNIL\nJUMPC ").append(stop.getName).append("\n")
        .append(emitStmt(body, ctx))
        .append("JUMP ").append(start.getName).append("\n")
        .append(stop.getName).append(":\n")
      ctx.loopEndLabels.removeLast()
      sb.toString
    case id.Break(_) =>
      if (ctx.loopEndLabels.isEmpty) throw new CompilerException("'break' used outside of loop", -1)
      s"JUMP ${ctx.loopEndLabels.last.getName}\n"
    case id.Return(valueOpt, _) =>
      val sb = new StringBuilder
      valueOpt.foreach(v => sb.append(emitExpr(v, ctx)))
      if (ctx.returnLabel == null) throw new CompilerException("Return label not found in scope", -1)
      sb.append("JUMP ").append(ctx.returnLabel.getName).append("\n").toString
