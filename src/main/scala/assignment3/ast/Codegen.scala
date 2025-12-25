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
  /**
   * Code generation context threaded through emit functions.
   *
   * == Invariants ==
   *  - `frameOpt` must be Some when emitting code inside a method body; None only for
   *    top-level or isolated expression evaluation.
   *  - `programSymbolsOpt` must be Some for class/field lookups; may be None for tests.
   *  - `loopEndLabels` is a stack: head is the innermost loop's end label for `break`.
   *    Push when entering a While, pop (implicitly via copy) when exiting.
   *  - `returnLabelOpt` is Some when inside a method that has a return epilogue label.
   *
   * == Threading Guidelines ==
   *  - Use `ctx.copy(...)` to add context (e.g., new loop label) without mutation.
   *  - Pass `ctx` unchanged to child expression/statement emitters unless scope changes.
   *  - For nested scopes (while/if), create modified copy and pass to body emitters.
   */
  final case class Ctx(
    frameOpt: Option[MethodFrame],
    programSymbolsOpt: Option[ProgramSymbols],
    loopEndLabels: List[Label] = Nil,
    returnLabelOpt: Option[Label] = None
  )

  private def typeOf(e: id.Expr, ctx: Ctx): assignment3.Type =
    (ctx.frameOpt, ctx.programSymbolsOpt) match
      case (Some(smf: SymbolMethodFrame), Some(ps)) =>
        IdiomaticTypeUtils.typeOf(e, new NewMethodContext(smf.getSymbol, ps), ps)
      case _ => Type.INT

  // Helper: emit a literal expression
  private def emitLiteralD(e: id.Expr): Either[Diag, Code] = e match
    case id.IntLit(v, _)  => Right(Code.pushInt(v))
    case id.BoolLit(v, _) => Right(Code.pushBool(v))
    case id.StrLit(v, _)  => Right(Code.from(new SamBuilder().pushImmStr(s"\"${v}\"")))
    case id.NullLit(_)    => Right(Code.pushNull)
    case _ => Left(SyntaxDiag("Not a literal expression", -1))

  // Helper: emit variable access
  private def emitVarD(name: String, pos: Int, ctx: Ctx): Either[Diag, Code] =
    ctx.frameOpt match
      case Some(frame) =>
        frame.lookupVar(name).toRight(ResolveDiag(Messages.undeclaredVariable(name), pos)).map { vb =>
          Code.from(new SamBuilder().pushOffS(StackOffset(vb.getAddress)))
        }
      case None => Left(ResolveDiag(Messages.Codegen.noFrameForVariable, pos))

  // Helper: emit field access
  private def emitFieldAccessD(target: id.Expr, field: String, fieldInfoOpt: Option[assignment3.symbol.ClassSymbol.FieldInfo], pos: Int, ctx: Ctx): Either[Diag, Code] =
    val offE: Either[Diag, Int] =
      fieldInfoOpt.map(_.offset) match
        case Some(off) if off >= 0 => Right(off)
        case _ =>
          (ctx.frameOpt, ctx.programSymbolsOpt) match
            case (Some(smf: SymbolMethodFrame), Some(ps)) =>
              AstEither.resolveFieldInfoD(target, field, new NewMethodContext(smf.getSymbol, ps), ps, pos).map(_.offset)
            case _ => Left(ResolveDiag(Messages.Codegen.unknownField(field), pos))
    for
      off <- offE
      base <- emitExprD(target, ctx)
    yield
      val sb = new SamBuilder()
      sb.append(base).addFieldOff(FieldOffset(off)).pushInd()
      Code.from(sb)

  // Helper: emit binary operation
  private def emitBinaryD(op: id.BinaryOp, left: id.Expr, right: id.Expr, pos: Int, ctx: Ctx): Either[Diag, Code] =
    val lt = typeOf(left, ctx); val rt = typeOf(right, ctx)
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
      checkBinaryOpSupported(op, lt, rt, stringy, pos) match
        case Some(d) => Left(d)
        case None =>
          for
            leftCode  <- emitExprD(left, ctx)
            rightCode <- emitExprD(right, ctx)
          yield emitBinaryOpCode(op, leftCode, rightCode, lt, rt, stringy)

  // Helper: check if binary operation is supported, return diagnostic if not
  private def checkBinaryOpSupported(op: id.BinaryOp, lt: Type, rt: Type, stringy: Boolean, pos: Int): Option[Diag] =
    op match
      case BinaryOp.Add if stringy && !(lt == Type.STRING && rt == Type.STRING) =>
        Some(TypeDiag(Messages.Codegen.plusOnlyStringOrNumeric, pos))
      case BinaryOp.Mul if stringy && !((lt == Type.STRING && rt == Type.INT) || (lt == Type.INT && rt == Type.STRING)) =>
        Some(TypeDiag(Messages.Codegen.repeatRequiresStringInt, pos))
      case BinaryOp.Lt | BinaryOp.Gt | BinaryOp.Eq if stringy && !(lt == Type.STRING && rt == Type.STRING) =>
        Some(TypeDiag(Messages.Codegen.stringComparisonRequiresBothString, pos))
      case BinaryOp.Le | BinaryOp.Ge | BinaryOp.Ne if stringy =>
        Some(TypeDiag(Messages.Codegen.unsupportedStringOperator, pos))
      case BinaryOp.Concat => Some(TypeDiag(Messages.Codegen.concatInternalOnly, pos))
      case BinaryOp.Le => Some(TypeDiag(Messages.Codegen.leNotSupported, pos))
      case BinaryOp.Ge => Some(TypeDiag(Messages.Codegen.geNotSupported, pos))
      case BinaryOp.Ne => Some(TypeDiag(Messages.Codegen.neNotSupported, pos))
      case _ => None

  // Helper: emit string binary operation (repeat, concat, compare)
  private def emitStringOp(op: id.BinaryOp, leftCode: Code, rightCode: Code, lt: Type, rt: Type): Code =
    op match
      case BinaryOp.Mul => leftCode + rightCode + Code.fromString(StringRuntime.repeatString(lt, rt))
      case BinaryOp.Add => leftCode + rightCode + Code.fromString(StringRuntime.concatString())
      case BinaryOp.Lt  => leftCode + rightCode + Code.fromString(StringRuntime.compareString('<'))
      case BinaryOp.Gt  => leftCode + rightCode + Code.fromString(StringRuntime.compareString('>'))
      case BinaryOp.Eq  => leftCode + rightCode + Code.fromString(StringRuntime.compareString('='))
      case _ => leftCode + rightCode // should not occur due to checkBinaryOpSupported

  // Helper: emit numeric binary operation (arithmetic, comparison, logical)
  private def emitNumericOp(op: id.BinaryOp, leftCode: Code, rightCode: Code): Code =
    import Operators._
    val ch = op match
      case BinaryOp.Add => ADD
      case BinaryOp.Sub => SUB
      case BinaryOp.Mul => MUL
      case BinaryOp.Div => DIV
      case BinaryOp.Mod => MOD
      case BinaryOp.And => AND
      case BinaryOp.Or  => OR
      case BinaryOp.Eq  => EQ
      case BinaryOp.Lt  => LT
      case BinaryOp.Gt  => GT
      case _            => EQ
    // Use Either variant; fallback empty for truly unknown operators (shouldn't happen)
    val opCode = OperatorUtils.getBinopE(ch).getOrElse("")
    leftCode + rightCode + Code.fromString(opCode)

  // Helper: emit the actual binary operation code
  private def emitBinaryOpCode(op: id.BinaryOp, leftCode: Code, rightCode: Code, lt: Type, rt: Type, stringy: Boolean): Code =
    if stringy then emitStringOp(op, leftCode, rightCode, lt, rt)
    else emitNumericOp(op, leftCode, rightCode)

  // Helper: emit ternary expression
  private def emitTernaryD(cond: id.Expr, thenExpr: id.Expr, elseExpr: id.Expr, ctx: Ctx): Either[Diag, Code] =
    for
      condCode <- emitExprD(cond, ctx)
      thenCode <- emitExprD(thenExpr, ctx)
      elseCode <- emitExprD(elseExpr, ctx)
    yield
      val falseLabel = new Label(); val endLabel = new Label()
      val sb = new SamBuilder()
      sb.append(condCode).jumpIfNil(falseLabel).append(thenCode).jump(endLabel).label(falseLabel).append(elseCode).label(endLabel)
      Code.from(sb)

  // Helper: emit function call
  private def emitCallD(m: id.CallableMethod, args: List[id.Expr], ctx: Ctx): Either[Diag, Code] =
    val hasReturn = hasReturnValue(m)
    Result.foldE(args, Code.returnSlot) { (acc, a) =>
      emitExprD(a, ctx).map(acc + _)
    }.map(_ + emitCallC(m.getName, args.size, hasReturn))

  // Helper: emit instance call
  private def emitInstanceCallD(target: id.Expr, m: id.CallableMethod, args: List[id.Expr], ctx: Ctx): Either[Diag, Code] =
    val hasReturn = hasReturnValue(m)
    for
      targetCode <- emitExprD(target, ctx)
      argsCode <- Result.foldE(args, Code.empty) { (acc, a) =>
        emitExprD(a, ctx).map(acc + _)
      }
    yield
      Code.returnSlot + targetCode + argsCode + emitCallC(m.getName, args.size + 1, hasReturn)

  // Helper: emit 'this' access
  private def emitThisD(pos: Int, ctx: Ctx): Either[Diag, Code] =
    ctx.frameOpt match
      case Some(frame) =>
        frame.lookupVar("this").toRight(ResolveDiag(Messages.Codegen.thisNotFound, pos)).map { vb =>
          Code.from(new SamBuilder().pushOffS(StackOffset(vb.getAddress)))
        }
      case None => Left(ResolveDiag(Messages.Codegen.noFrameForThis, pos))

  // Helper: emit new object instantiation
  private def emitNewObjectD(className: String, args: List[id.Expr], ctx: Ctx): Either[Diag, Code] =
    val numFields = ctx.programSymbolsOpt.flatMap(_.getClass(className)).map(_.numFields()).getOrElse(1)
    val ctorExists = ctx.programSymbolsOpt.flatMap(_.getClass(className)).exists(_.method(className).isDefined)
    Result.foldE(args, Code.empty) { (acc, a) =>
      emitExprD(a, ctx).map(acc + _)
    }.map { argsCode =>
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

  // Core adapter: return Either so callers can use diagnostic flow.
  private def emitExprCoreE(e: id.Expr, ctx: Ctx): Either[Diag, Code] =
    emitExprD(e, ctx)

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
    sb.pushBool(true).jump(endLabel)
    sb.label(falseLbl).pushBool(false).label(endLabel)
    Code.from(sb)

  // Helper: short-circuit OR (if left true, skip right)
  private def emitShortCircuitOrC(leftCode: Code, rightCode: Code, sb: SamBuilder): Code =
    val needRight = new Label(); val falseLbl = new Label(); val endLabel = new Label()
    sb.append(leftCode).jumpIfNil(needRight)
    sb.pushBool(true).jump(endLabel)
    sb.label(needRight)
    sb.append(rightCode).jumpIfNil(falseLbl)
    sb.pushBool(true).jump(endLabel)
    sb.label(falseLbl).pushBool(false).label(endLabel)
    Code.from(sb)

  // Helper: string binary operations
  // (Removed an unused helper that previously threw Exceptions for unsupported
  // string operators. The diagnostic-first functions below return Eithers and
  // should be used for error handling.)

  // Core adapter: return Either so callers can use diagnostic flow.
  private def emitStmtCoreE(s: id.Stmt, ctx: Ctx): Either[Diag, Code] =
    emitStmtD(s, ctx)

  // Public APIs: Code-returning preferred; String-returning wrappers for compatibility
  // Non-diagnostic (compat) wrappers: convert Left(diag) -> Error for callers that expect exceptions.
  @deprecated("Use emitExprD instead", "2.0")
  def emitExprC(e: id.Expr, ctx: Ctx): Code = emitExprCoreE(e, ctx) match
    case Right(c) => c
    case Left(d)  => throw new Error(d.message)
  @deprecated("Use emitStmtD instead", "2.0")
  def emitStmtC(s: id.Stmt, ctx: Ctx): Code = emitStmtCoreE(s, ctx) match
    case Right(c) => c
    case Left(d)  => throw new Error(d.message)
  @deprecated("Use emitExprD instead", "2.0")
  def emitExpr(e: id.Expr, ctx: Ctx): String = emitExprC(e, ctx).toString
  @deprecated("Use emitStmtD instead", "2.0")
  def emitStmt(s: id.Stmt, ctx: Ctx): String = emitStmtC(s, ctx).toString

  // Diagnostic-first APIs (Either[Diag, Code]) for safer composition
  def emitExprD(e: id.Expr, ctx: Ctx): Either[Diag, Code] = e match
    case lit: (id.IntLit | id.BoolLit | id.StrLit | id.NullLit) => emitLiteralD(lit)
    case id.Var(name, pos) => emitVarD(name, pos, ctx)
    case id.FieldAccess(target, field, fieldInfoOpt, pos) => emitFieldAccessD(target, field, fieldInfoOpt, pos, ctx)
    case id.Unary(op, expr, rt, pos) => emitUnaryD(op, expr, rt, ctx)
    case id.Binary(op, left, right, _, pos) => emitBinaryD(op, left, right, pos, ctx)
    case id.Ternary(cond, thenExpr, elseExpr, _, _) => emitTernaryD(cond, thenExpr, elseExpr, ctx)
    case id.Call(m, args, _) => emitCallD(m, args, ctx)
    case id.InstanceCall(target, m, args, _) => emitInstanceCallD(target, m, args, ctx)
    case id.This(pos) => emitThisD(pos, ctx)
    case id.NewObject(className, args, _) => emitNewObjectD(className, args, ctx)

  // Helper: emit unary operation
  private def emitUnaryD(op: id.UnaryOp, expr: id.Expr, rt: Option[Type], ctx: Ctx): Either[Diag, Code] =
    import Operators._
    emitExprD(expr, ctx).map { base =>
      op match
        case UnaryOp.Neg =>
          if rt.exists(_ == Type.STRING) then base + Code.fromString(StringRuntime.reverseString())
          else base + Code.fromString(OperatorUtils.getUnopE(NEG).getOrElse(""))
        case UnaryOp.Not =>
          base + Code.fromString(OperatorUtils.getUnopE(NOT).getOrElse(""))
    }

  def emitStmtD(s: id.Stmt, ctx: Ctx): Either[Diag, Code] =
    s match
      case id.Block(statements, _) =>
        Result.foldE(statements, Code.empty) { (acc, st) =>
          emitStmtD(st, ctx).map(acc + _)
        }
      case id.VarDecl(_, _, _, initOpt, _) =>
        initOpt match
          case Some(init) => emitExprD(init, ctx)
          case None       => Right(Code.pushNull)
      case id.Assign(name, value, pos) =>
        ctx.frameOpt match
          case Some(frame) =>
            for
              vb <- frame.lookupVar(name).toRight(ResolveDiag(Messages.undeclaredVariable(name), pos))
              valueCode <- emitExprD(value, ctx)
            yield
              val sb = new SamBuilder()
              sb.append(valueCode).storeOffS(StackOffset(vb.getAddress))
              Code.from(sb)
          case None => Left(ResolveDiag(Messages.Codegen.noFrameForAssignment, pos))
      case id.FieldAssign(target, field, offset, value, pos) =>
        if offset < 0 then Left(ResolveDiag(Messages.Codegen.unknownField(field), pos))
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
        if ctx.loopEndLabels.headOption.isEmpty then Left(SyntaxDiag(Messages.Codegen.breakOutsideLoop, pos))
        else Right(Code.from(new SamBuilder().jump(ctx.loopEndLabels.head)))
      case id.Return(valueOpt, pos) =>
        ctx.returnLabelOpt match
          case None => Left(ResolveDiag(Messages.Codegen.returnLabelNotFound, pos))
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
  // No default case: rely on exhaustiveness of `Stmt` variants. If a null
  // is ever passed (shouldn't happen), let it fail loudly.
