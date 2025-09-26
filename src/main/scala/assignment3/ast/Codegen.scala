package assignment3.ast

import assignment3._
import assignment3.ast as id
import assignment3.ast.{AstEither, Diag, ResolveDiag, SyntaxDiag, TypeDiag}
import assignment3.ast.IdiomaticTypeUtils
import assignment3.symbol.ProgramSymbols
import assignment3.ast.high.ReturnSig
import assignment3.ast.MethodFrame
import assignment3.ast.{NewMethodContext, SymbolMethodFrame}
import assignment3.Offsets.{FieldOffset, StackOffset}
import scala.jdk.CollectionConverters._

/** Code generation using idiomatic, pattern-matching AST. */
object IdiomaticCodegen:
  final case class Ctx(
    frameOpt: Option[MethodFrame],
    programSymbolsOpt: Option[ProgramSymbols],
    loopEndLabels: List[Label] = Nil,
    returnLabelOpt: Option[Label] = None
  )

  private def tOf(e: id.Expr, ictx: Ctx): assignment3.Type =
    if ictx.frameOpt.isEmpty || ictx.programSymbolsOpt.isEmpty then Type.INT
    else
      ictx.frameOpt.get match
        case smf: SymbolMethodFrame => IdiomaticTypeUtils.typeOf(e, new NewMethodContext(smf.getSymbol, ictx.programSymbolsOpt.get), ictx.programSymbolsOpt.get)
        case _ => Type.INT

  // Non-diagnostic codegen delegates to diagnostic versions and converts Diag -> Exception
  private def emitExprCore(e: id.Expr, ctx: Ctx): Code =
    emitExprD(e, ctx) match
      case Right(c) => c
      case Left(d)  => throw new Exception(d.message)

  private def emitCallC(label: String, paramCount: Int, hasReturn: Boolean): Code =
    val sb = new SamBuilder()
    sb.call(label, paramCount, hasReturn)
    Code.from(sb)

  // Centralized helper: does this callable produce a return value to keep on TOS?
  private def hasReturnValue(m: id.CallableMethod): Boolean = m match
    case sm: ScalaCallableMethod => sm.getReturnSig != ReturnSig.Void
    case _ => false

  // Helper: short-circuit AND (both sides must be truthy; otherwise false)
  private def emitShortCircuitAndC(leftCode: Code, rightCode: Code, sb: SamBuilder): Code =
    val falseLbl = new Label(); val endLabel = new Label()
    sb.append(leftCode).jumpIfNil(falseLbl)
    sb.append(rightCode).jumpIfNil(falseLbl)
    sb.pushImmInt(1).jump(endLabel)
    sb.label(falseLbl).pushImmInt(0).label(endLabel)
    Code.from(sb)

  // Helper: short-circuit OR (if left true, skip right)
  private def emitShortCircuitOrC(leftCode: Code, rightCode: Code, sb: SamBuilder): Code =
    val needRight = new Label(); val falseLbl = new Label(); val endLabel = new Label()
    sb.append(leftCode).jumpIfNil(needRight)
    sb.pushImmInt(1).jump(endLabel)
    sb.label(needRight)
    sb.append(rightCode).jumpIfNil(falseLbl)
    sb.pushImmInt(1).jump(endLabel)
    sb.label(falseLbl).pushImmInt(0).label(endLabel)
    Code.from(sb)

  // Helper: string binary operations
  private def emitStringBinaryC(op: BinaryOp, lt: Type, rt: Type, leftCode: Code, rightCode: Code): Code =
    op match
      case BinaryOp.Mul if (lt == Type.STRING && rt == Type.INT) || (lt == Type.INT && rt == Type.STRING) =>
        leftCode + rightCode + Code.fromString(StringRuntime.repeatString(lt, rt))
      case BinaryOp.Add =>
        if (lt == Type.STRING && rt == Type.STRING) then leftCode + rightCode + Code.fromString(StringRuntime.concatString())
        else throw new Exception("'+' only defined for String+String or numeric addition")
      case BinaryOp.Lt | BinaryOp.Gt | BinaryOp.Eq =>
        if (lt == Type.STRING && rt == Type.STRING) then
          val ch = op match
            case BinaryOp.Lt => '<'
            case BinaryOp.Gt => '>'
            case _           => '='
          leftCode + rightCode + Code.fromString(StringRuntime.compareString(ch))
        else throw new Exception("String comparison requires both operands String")
      case _ => throw new Exception("Unsupported operator for String operands")

  // Non-diagnostic statement codegen delegates to diagnostic version and converts Diag -> Exception
  private def emitStmtCore(s: id.Stmt, ctx: Ctx): Code =
    emitStmtD(s, ctx) match
      case Right(c) => c
      case Left(d)  => throw new Exception(d.message)

  // Public APIs: Code-returning preferred; String-returning wrappers for compatibility
  def emitExprC(e: id.Expr, ctx: Ctx): Code = emitExprCore(e, ctx)
  def emitStmtC(s: id.Stmt, ctx: Ctx): Code = emitStmtCore(s, ctx)
  def emitExpr(e: id.Expr, ctx: Ctx): String = emitExprCore(e, ctx).toString
  def emitStmt(s: id.Stmt, ctx: Ctx): String = emitStmtCore(s, ctx).toString

  // Diagnostic-first APIs (Either[Diag, Code]) for safer composition
  def emitExprD(e: id.Expr, ctx: Ctx): Either[Diag, Code] =
    e match
      case id.IntLit(v, _)  => Right(Code.from(new SamBuilder().pushImmInt(v)))
      case id.BoolLit(v, _) => Right(Code.from(new SamBuilder().pushImmInt(if v then 1 else 0)))
      case id.StrLit(v, _)  => Right(Code.from(new SamBuilder().pushImmStr(s"\"${v}\"")))
      case id.NullLit(_)    => Right(Code.from(new SamBuilder().pushImmInt(0)))
      case id.Var(name, pos) =>
        ctx.frameOpt match
          case Some(frame) =>
            frame.lookupVar(name).toRight(ResolveDiag(s"Undeclared variable: $name", pos)).map { vb =>
              Code.from(new SamBuilder().pushOffS(StackOffset(vb.getAddress)))
            }
          case None => Left(ResolveDiag("No frame in context for variable lookup", pos))
      case id.FieldAccess(target, field, fieldInfoOpt, pos) =>
        val offE: Either[Diag, Int] =
          fieldInfoOpt.map(_.offset) match
            case Some(off) if off >= 0 => Right(off)
            case _ =>
              (ctx.frameOpt, ctx.programSymbolsOpt) match
                case (Some(smf: SymbolMethodFrame), Some(ps)) =>
                  AstEither.resolveFieldInfoD(target, field, new NewMethodContext(smf.getSymbol, ps), ps, pos).map(_.offset)
                case _ => Left(ResolveDiag(s"Unknown field '$field'", pos))
        for
          off <- offE
          base <- emitExprD(target, ctx)
        yield
          val sb = new SamBuilder()
          sb.append(base).addFieldOff(FieldOffset(off)).pushInd()
          Code.from(sb)
      case id.Unary(op, expr, rt, pos) =>
        emitExprD(expr, ctx).map { base =>
          op match
            case UnaryOp.Neg =>
              if rt.exists(_ == Type.STRING) then base + Code.fromString(StringRuntime.reverseString())
              else base + Code.fromString(OperatorUtils.getUnop('~'))
            case UnaryOp.Not =>
              base + Code.fromString(OperatorUtils.getUnop('!'))
        }
      case id.Binary(op, left, right, _, pos) =>
        val lt = tOf(left, ctx); val rt = tOf(right, ctx)
        // Handle boolean short-circuit for And/Or first
        if (op == BinaryOp.And || op == BinaryOp.Or) && lt == Type.BOOL && rt == Type.BOOL then
          for
            leftCode  <- emitExprD(left, ctx)
            rightCode <- emitExprD(right, ctx)
          yield
            val sb = new SamBuilder()
            if op == BinaryOp.And then emitShortCircuitAndC(leftCode, rightCode, sb)
            else emitShortCircuitOrC(leftCode, rightCode, sb)
        else
          // Diagnose unsupported operators (string and numeric)
          val stringy = lt == Type.STRING || rt == Type.STRING
          val diagOpt: Option[Diag] =
            op match
              case BinaryOp.Add if stringy && !(lt == Type.STRING && rt == Type.STRING) => Some(TypeDiag("'+' only defined for String+String or numeric addition", pos))
              case BinaryOp.Mul if stringy && !((lt == Type.STRING && rt == Type.INT) || (lt == Type.INT && rt == Type.STRING)) => Some(TypeDiag("Repeat requires (String*Int) or (Int*String)", pos))
              case BinaryOp.Lt | BinaryOp.Gt | BinaryOp.Eq if stringy && !(lt == Type.STRING && rt == Type.STRING) => Some(TypeDiag("String comparison requires both operands String", pos))
              case BinaryOp.Le | BinaryOp.Ge | BinaryOp.Ne if stringy => Some(TypeDiag("Unsupported string operator", pos))
              case BinaryOp.Concat => Some(TypeDiag("'concat' is internal-only for strings", pos))
              case BinaryOp.Le => Some(TypeDiag("'<= not supported by runtime'", pos))
              case BinaryOp.Ge => Some(TypeDiag(">= not supported by runtime", pos))
              case BinaryOp.Ne => Some(TypeDiag("!= not supported by runtime", pos))
              case _ => None
          diagOpt match
            case Some(d) => Left(d)
            case None =>
              for
                leftCode  <- emitExprD(left, ctx)
                rightCode <- emitExprD(right, ctx)
              yield
                if stringy then
                  op match
                    case BinaryOp.Mul => leftCode + rightCode + Code.fromString(StringRuntime.repeatString(lt, rt))
                    case BinaryOp.Add => leftCode + rightCode + Code.fromString(StringRuntime.concatString())
                    case BinaryOp.Lt | BinaryOp.Gt | BinaryOp.Eq =>
                      val ch = op match
                        case BinaryOp.Lt => '<'
                        case BinaryOp.Gt => '>'
                        case _           => '='
                      leftCode + rightCode + Code.fromString(StringRuntime.compareString(ch))
                    case _ => leftCode + rightCode // should not occur due to diagOpt
                else
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
                    case _            => '=' // shouldn't happen due to diagOpt
                  leftCode + rightCode + Code.fromString(OperatorUtils.getBinop(ch))
      case id.Ternary(cond, thenExpr, elseExpr, _, pos) =>
        for
          condCode <- emitExprD(cond, ctx)
          thenCode <- emitExprD(thenExpr, ctx)
          elseCode <- emitExprD(elseExpr, ctx)
        yield
          val falseLabel = new Label(); val endLabel = new Label()
          val sb = new SamBuilder()
          sb.append(condCode).jumpIfNil(falseLabel).append(thenCode).jump(endLabel).label(falseLabel).append(elseCode).label(endLabel)
          Code.from(sb)
      case id.Call(m, args, pos) =>
        val hasReturn = hasReturnValue(m)
        val init: Either[Diag, Code] = Right(Code.from(new SamBuilder().pushImmInt(0)))
        val argsCodeE = args.foldLeft(init) { (accE, a) =>
          for acc <- accE; code <- emitExprD(a, ctx) yield acc + code
        }
        argsCodeE.map { argsCode =>
          argsCode + emitCallC(m.getName, args.size, hasReturn)
        }
      case id.InstanceCall(target, m, args, pos) =>
        val hasReturn = hasReturnValue(m)
        for
          targetCode <- emitExprD(target, ctx)
          argsCode <- args.foldLeft(Right(Code.from(new SamBuilder())): Either[Diag, Code]) { (accE, a) =>
            for acc <- accE; code <- emitExprD(a, ctx) yield acc + code
          }
        yield
          Code.from(new SamBuilder().pushImmInt(0)) + targetCode + argsCode + emitCallC(m.getName, args.size + 1, hasReturn)
      case id.This(pos) =>
        ctx.frameOpt match
          case Some(frame) =>
            frame.lookupVar("this").toRight(ResolveDiag("'this' not found in current frame", pos)).map { vb =>
              Code.from(new SamBuilder().pushOffS(StackOffset(vb.getAddress)))
            }
          case None => Left(ResolveDiag("No frame in context for 'this'", pos))
      case id.NewObject(className, args, pos) =>
        val numFields = ctx.programSymbolsOpt.flatMap(_.getClass(className)).map(_.numFields()).getOrElse(1)
        val ctorExists = ctx.programSymbolsOpt.flatMap(_.getClass(className)).exists(_.method(className).isDefined)
        val argsCodeE = args.foldLeft(Right(Code.from(new SamBuilder())): Either[Diag, Code]) { (accE, a) =>
          for acc <- accE; code <- emitExprD(a, ctx) yield acc + code
        }
        argsCodeE.map { argsCode =>
          val sb = new SamBuilder()
          sb.pushImmInt(numFields).malloc()
          if ctorExists then
            sb.dup().pushImmInt(0).swap()
            sb.append(argsCode)
            sb.linkCall(s"${className}_${className}")
            val paramCount = args.size + 1
            sb.addSp(-paramCount)
            sb.addSp(-1)
          Code.from(sb)
        }

  def emitStmtD(s: id.Stmt, ctx: Ctx): Either[Diag, Code] =
    s match
      case id.Block(statements, _) =>
        // Recursively emit each statement, concatenating
        statements.foldLeft(Right(Code.from(new SamBuilder())): Either[Diag, Code]) { (accE, st) =>
          for acc <- accE; code <- emitStmtD(st, ctx) yield acc + code
        }
      case id.VarDecl(_, _, _, initOpt, _) =>
        initOpt match
          case Some(init) => emitExprD(init, ctx)
          case None       => Right(Code.from(new SamBuilder().pushImmInt(0)))
      case id.Assign(name, value, pos) =>
        ctx.frameOpt match
          case Some(frame) =>
            for
              vb <- frame.lookupVar(name).toRight(ResolveDiag(s"Undeclared variable: $name", pos))
              valueCode <- emitExprD(value, ctx)
            yield
              val sb = new SamBuilder()
              sb.append(valueCode).storeOffS(StackOffset(vb.getAddress))
              Code.from(sb)
          case None => Left(ResolveDiag("No frame in context for assignment", pos))
      case id.FieldAssign(target, field, offset, value, pos) =>
        if offset < 0 then Left(ResolveDiag(s"Unknown field '$field'", pos))
        else
          for
            targetCode <- emitExprD(target, ctx)
            valueCode  <- emitExprD(value, ctx)
          yield
            val sb = new SamBuilder()
            sb.append(targetCode).addFieldOff(FieldOffset(offset)).append(valueCode).storeInd()
            Code.from(sb)
      case id.If(cond, thenB, elseB, _) =>
        for
          condCode <- emitExprD(cond, ctx)
          thenCode <- emitStmtD(thenB, ctx)
          elseCode <- emitStmtD(elseB, ctx)
        yield
          val elseLbl = new Label(); val endLbl = new Label()
          val sb = new SamBuilder()
          sb.append(condCode).jumpIfNil(elseLbl)
            .append(thenCode)
            .jump(endLbl)
            .label(elseLbl)
            .append(elseCode)
            .label(endLbl)
          Code.from(sb)
      case id.While(cond, body, _) =>
        val endLabel = new Label()
        val innerCtx = ctx.copy(loopEndLabels = endLabel :: ctx.loopEndLabels)
        for
          condCode <- emitExprD(cond, ctx)
          bodyCode <- emitStmtD(body, innerCtx)
        yield
          val start = new Label(); val stop = endLabel
          val sb = new SamBuilder()
          sb.label(start)
            .append(condCode).jumpIfNil(stop)
            .append(bodyCode)
            .jump(start)
            .label(stop)
          Code.from(sb)
      case id.Break(pos) =>
        if ctx.loopEndLabels.headOption.isEmpty then Left(SyntaxDiag("'break' used outside of loop", pos))
        else Right(Code.from(new SamBuilder().jump(ctx.loopEndLabels.head)))
      case id.Return(valueOpt, pos) =>
        ctx.returnLabelOpt match
          case None => Left(ResolveDiag("Return label not found in scope", pos))
          case Some(ret) =>
            val valueCodeE: Either[Diag, Code] = valueOpt match
              case Some(v) => emitExprD(v, ctx)
              case None    => Right(Code.from(new SamBuilder()))
            valueCodeE.map { valueCode =>
              val sb = new SamBuilder()
              sb.append(valueCode)
              sb.jump(ret)
              Code.from(sb)
            }
      case _ => Right(Code.from(new SamBuilder()))
