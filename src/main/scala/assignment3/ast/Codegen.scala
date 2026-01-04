package assignment3.ast

import assignment3.{BinaryOpMapping, Code, Label, Messages, OperatorUtils, Operators, SamBuilder, StringRuntime, Type}
import assignment3.Offsets.{FieldOffset, StackOffset}
import assignment3.ast as id
import assignment3.ast.high.ReturnSig
import assignment3.symbol.ProgramSymbols

/** Code generation using idiomatic, pattern-matching AST. */
object IdiomaticCodegen:
  /** Constants for code generation. */
  private object CodegenConstants:
    /** Default field count when class symbol is unavailable. */
    val DefaultFieldCount = 1

    /** Return slot position (always 0 in SaM calling convention). */
    val ReturnSlotPosition = 0

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

  /** Extension methods for Ctx to simplify symbol context access. */
  extension (ctx: Ctx)
    /** Execute function with symbol context if available, otherwise return fallback. */
    def withSymbolContext[A](f: (SymbolMethodFrame, ProgramSymbols) => A, fallback: => A): A =
      (ctx.frameOpt, ctx.programSymbolsOpt) match
        case (Some(smf: SymbolMethodFrame), Some(ps)) => f(smf, ps)
        case _ => fallback

    /** Execute function with symbol context if available, otherwise return diagnostic. */
    def withSymbolContextE[A](f: (SymbolMethodFrame, ProgramSymbols) => Either[Diag, A],
                              mkDiag: => Diag): Either[Diag, A] =
      (ctx.frameOpt, ctx.programSymbolsOpt) match
        case (Some(smf: SymbolMethodFrame), Some(ps)) => f(smf, ps)
        case _ => Left(mkDiag)

  private def typeOf(e: id.Expr, ctx: Ctx): assignment3.Type =
    ctx.withSymbolContext(
      (smf, ps) => IdiomaticTypeUtils.typeOf(e, new NewMethodContext(smf.getSymbol, ps), ps),
      Type.INT
    )

  // Helper: emit a literal expression
  private def emitLiteralD(e: id.Expr): Either[Diag, Code] = e match
    case id.IntLit(v, _)  => Right(Code.pushInt(v))
    case id.BoolLit(v, _) => Right(Code.pushBool(v))
    case id.StrLit(v, _)  => Right(Code.from(new SamBuilder().pushImmStr(Code.escapeStringLiteral(v))))
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

  // Helper: resolve field offset from cache or by lookup
  private def resolveFieldOffset(
    target: id.Expr,
    field: String,
    cachedInfoOpt: Option[assignment3.symbol.ClassSymbol.FieldInfo],
    ctx: Ctx,
    pos: Int
  ): Either[Diag, Int] =
    cachedInfoOpt.map(_.offset).filter(_ >= 0) match
      case Some(off) => Right(off)
      case None =>
        ctx.withSymbolContextE(
          (smf, ps) => AstEither.resolveFieldInfoD(target, field, new NewMethodContext(smf.getSymbol, ps), ps, pos).map(_.offset),
          ResolveDiag(Messages.Codegen.unknownField(field), pos)
        )

  // Helper: emit field access
  private def emitFieldAccessD(target: id.Expr, field: String, fieldInfoOpt: Option[assignment3.symbol.ClassSymbol.FieldInfo], pos: Int, ctx: Ctx): Either[Diag, Code] =
    for
      off <- resolveFieldOffset(target, field, fieldInfoOpt, ctx, pos)
      base <- emitExprD(target, ctx)
    yield
      val sb = new SamBuilder()
      sb.append(base).addFieldOff(FieldOffset(off)).pushInd()
      Code.from(sb)

  // Binary operation categories for clearer dispatch
  private enum BinaryOpCategory:
    case ShortCircuit, String, Numeric

  /** ADT for string operations with type-safe emission. */
  private enum StringOperation:
    case Repeat(leftType: Type, rightType: Type)
    case Concat
    case Compare(op: Char)

    /** Emit the runtime code for this string operation. */
    def emitRuntime(): String = this match
      case Repeat(lt, rt) => StringRuntime.repeatString(lt, rt)
      case Concat => StringRuntime.concatString()
      case Compare(op) => StringRuntime.compareString(op)

  private object StringOperation:
    /** Determine the string operation from BinaryOp and types. */
    def from(op: id.BinaryOp, lt: Type, rt: Type): Option[StringOperation] = op match
      case BinaryOp.Mul if lt == Type.STRING || rt == Type.STRING => Some(Repeat(lt, rt))
      case BinaryOp.Add if lt == Type.STRING && rt == Type.STRING => Some(Concat)
      case BinaryOp.Lt  if lt == Type.STRING && rt == Type.STRING => Some(Compare('<'))
      case BinaryOp.Gt  if lt == Type.STRING && rt == Type.STRING => Some(Compare('>'))
      case BinaryOp.Eq  if lt == Type.STRING && rt == Type.STRING => Some(Compare('='))
      case _ => None

  private def categorizeBinaryOp(op: id.BinaryOp, lt: Type, rt: Type): BinaryOpCategory =
    if (op == BinaryOp.And || op == BinaryOp.Or) && lt == Type.BOOL && rt == Type.BOOL then
      BinaryOpCategory.ShortCircuit
    else if lt == Type.STRING || rt == Type.STRING then
      BinaryOpCategory.String
    else
      BinaryOpCategory.Numeric

  // Helper: emit binary operation
  private def emitBinaryD(op: id.BinaryOp, left: id.Expr, right: id.Expr, pos: Int, ctx: Ctx): Either[Diag, Code] =
    val lt = typeOf(left, ctx); val rt = typeOf(right, ctx)
    categorizeBinaryOp(op, lt, rt) match
      case BinaryOpCategory.ShortCircuit =>
        emitShortCircuitBinaryD(op, left, right, ctx)
      case BinaryOpCategory.String =>
        checkBinaryOpSupported(op, lt, rt, stringy = true, pos) match
          case Some(d) => Left(d)
          case None =>
            for
              leftCode  <- emitExprD(left, ctx)
              rightCode <- emitExprD(right, ctx)
            yield emitStringOp(op, leftCode, rightCode, lt, rt)
      case BinaryOpCategory.Numeric =>
        checkBinaryOpSupported(op, lt, rt, stringy = false, pos) match
          case Some(d) => Left(d)
          case None =>
            for
              leftCode  <- emitExprD(left, ctx)
              rightCode <- emitExprD(right, ctx)
            yield emitNumericOp(op, leftCode, rightCode)

  // Helper: emit short-circuit binary operations (And/Or)
  private def emitShortCircuitBinaryD(op: id.BinaryOp, left: id.Expr, right: id.Expr, ctx: Ctx): Either[Diag, Code] =
    for
      leftCode  <- emitExprD(left, ctx)
      rightCode <- emitExprD(right, ctx)
    yield
      val sb = new SamBuilder()
      if op == BinaryOp.And then emitShortCircuitAndC(leftCode, rightCode, sb)
      else emitShortCircuitOrC(leftCode, rightCode, sb)

  /** Operation constraint: defines validation rules for binary operations. */
  private case class OpConstraint(
    ops: Set[id.BinaryOp],
    isValid: (Type, Type) => Boolean,
    errorMsg: String
  )

  /** Table of operation constraints for string operations. */
  private val stringOpConstraints: List[OpConstraint] = List(
    OpConstraint(Set(BinaryOp.Add), (lt, rt) => lt == Type.STRING && rt == Type.STRING, Messages.Codegen.plusOnlyStringOrNumeric),
    OpConstraint(Set(BinaryOp.Mul), (lt, rt) => (lt == Type.STRING && rt == Type.INT) || (lt == Type.INT && rt == Type.STRING), Messages.Codegen.repeatRequiresStringInt),
    OpConstraint(Set(BinaryOp.Lt, BinaryOp.Gt, BinaryOp.Eq), (lt, rt) => lt == Type.STRING && rt == Type.STRING, Messages.Codegen.stringComparisonRequiresBothString),
    OpConstraint(Set(BinaryOp.Le, BinaryOp.Ge, BinaryOp.Ne), (_, _) => false, Messages.Codegen.unsupportedStringOperator)
  )

  /** Unsupported operations (always fail). */
  private val unsupportedOps: Map[id.BinaryOp, String] = Map(
    BinaryOp.Concat -> Messages.Codegen.concatInternalOnly,
    BinaryOp.Le -> Messages.Codegen.leNotSupported,
    BinaryOp.Ge -> Messages.Codegen.geNotSupported,
    BinaryOp.Ne -> Messages.Codegen.neNotSupported
  )

  // Helper: check if binary operation is supported, return diagnostic if not
  private def checkBinaryOpSupported(op: id.BinaryOp, lt: Type, rt: Type, stringy: Boolean, pos: Int): Option[Diag] =
    // Check for always-unsupported operations first
    unsupportedOps.get(op).map(msg => TypeDiag(msg, pos)).orElse {
      // For string operations, check constraints
      if stringy then
        stringOpConstraints.find(c => c.ops.contains(op) && !c.isValid(lt, rt))
          .map(c => TypeDiag(c.errorMsg, pos))
      else None
    }

  // Helper: emit string binary operation (repeat, concat, compare)
  private def emitStringOp(op: id.BinaryOp, leftCode: Code, rightCode: Code, lt: Type, rt: Type): Code =
    StringOperation.from(op, lt, rt) match
      case Some(stringOp) => leftCode + rightCode + Code.fromString(stringOp.emitRuntime())
      case None => leftCode + rightCode // should not occur due to checkBinaryOpSupported

  // Helper: emit numeric binary operation (arithmetic, comparison, logical)
  private def emitNumericOp(op: id.BinaryOp, leftCode: Code, rightCode: Code): Code =
    val ch = BinaryOpMapping.toChar(op)
    // Use Either variant; fail fast if operator is somehow invalid (indicates compiler bug)
    val opCode = OperatorUtils.getBinopE(ch).getOrElse {
      throw new AssertionError(s"Internal error: unknown binary operator '$ch'")
    }
    leftCode + rightCode + Code.fromString(opCode)

  // Helper: emit ternary expression
  private def emitTernaryD(cond: id.Expr, thenExpr: id.Expr, elseExpr: id.Expr, ctx: Ctx): Either[Diag, Code] =
    for
      condCode <- emitExprD(cond, ctx)
      thenCode <- emitExprD(thenExpr, ctx)
      elseCode <- emitExprD(elseExpr, ctx)
    yield
      val falseLabel = Label(); val endLabel = Label()
      val sb = new SamBuilder()
      sb.append(condCode).jumpIfNil(falseLabel).append(thenCode).jump(endLabel).label(falseLabel).append(elseCode).label(endLabel)
      Code.from(sb)

  // Helper: emit function call
  private def emitCallD(m: id.CallableMethod, args: List[id.Expr], ctx: Ctx): Either[Diag, Code] =
    val hasReturn = hasReturnValue(m)
    Result.traverseE(args)(emitExprD(_, ctx)).map { argCodes =>
      Code.concat(List(Code.returnSlot) ++ argCodes :+ emitCallC(m.getName, args.size, hasReturn))
    }

  // Helper: emit instance call
  private def emitInstanceCallD(target: id.Expr, m: id.CallableMethod, args: List[id.Expr], ctx: Ctx): Either[Diag, Code] =
    val hasReturn = hasReturnValue(m)
    for
      targetCode <- emitExprD(target, ctx)
      argCodes <- Result.traverseE(args)(emitExprD(_, ctx))
    yield
      Code.concat(List(Code.returnSlot, targetCode) ++ argCodes :+ emitCallC(m.getName, args.size + 1, hasReturn))

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
    val numFields = ctx.programSymbolsOpt.flatMap(_.getClass(className)).fold(CodegenConstants.DefaultFieldCount)(_.numFields())
    val ctorExists = ctx.programSymbolsOpt.flatMap(_.getClass(className)).exists(_.method(className).isDefined)
    Result.traverseE(args)(emitExprD(_, ctx)).map { argCodes =>
      val sb = new SamBuilder()
      sb.pushImmInt(numFields).malloc()
      if ctorExists then
        sb.dup().pushImmInt(CodegenConstants.ReturnSlotPosition).swap()
        sb.append(Code.concat(argCodes))
        sb.linkCall(s"${className}_${className}")
        val paramCount = args.size + 1
        sb.addSp(-paramCount)
        sb.addSp(-1)
      Code.from(sb)
    }

  private def emitCallC(label: String, paramCount: Int, hasReturn: Boolean): Code =
    val sb = new SamBuilder()
    sb.call(label, paramCount, hasReturn)
    Code.from(sb)

  // Centralized helper: does this callable produce a return value to keep on TOS?
  private def hasReturnValue(m: id.CallableMethod): Boolean = m match
    case sm: ScalaCallableMethod => sm.getReturnSig != ReturnSig.Void
    case _ => false

  // Helper: unified short-circuit boolean logic
  private def emitShortCircuitBooleanC(
    leftCode: Code,
    rightCode: Code,
    sb: SamBuilder,
    isAnd: Boolean
  ): Code =
    if isAnd then
      // AND: both must be truthy, otherwise false
      val falseLbl = Label(); val endLabel = Label()
      sb.append(leftCode).jumpIfNil(falseLbl)
      sb.append(rightCode).jumpIfNil(falseLbl)
      sb.pushBool(true).jump(endLabel)
      sb.label(falseLbl).pushBool(false).label(endLabel)
    else
      // OR: if left true, skip right
      val needRight = Label(); val falseLbl = Label(); val endLabel = Label()
      sb.append(leftCode).jumpIfNil(needRight)
      sb.pushBool(true).jump(endLabel)
      sb.label(needRight)
      sb.append(rightCode).jumpIfNil(falseLbl)
      sb.pushBool(true).jump(endLabel)
      sb.label(falseLbl).pushBool(false).label(endLabel)
    Code.from(sb)

  // Helper: short-circuit AND (both sides must be truthy; otherwise false)
  private def emitShortCircuitAndC(leftCode: Code, rightCode: Code, sb: SamBuilder): Code =
    emitShortCircuitBooleanC(leftCode, rightCode, sb, isAnd = true)

  // Helper: short-circuit OR (if left true, skip right)
  private def emitShortCircuitOrC(leftCode: Code, rightCode: Code, sb: SamBuilder): Code =
    emitShortCircuitBooleanC(leftCode, rightCode, sb, isAnd = false)

  /** Emit code for an expression. Returns Either for diagnostic flow. */
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
    // These operators are guaranteed to exist; fail fast if not (indicates compiler bug)
    def getUnopOrFail(o: Char): String = OperatorUtils.getUnopE(o).getOrElse {
      throw new AssertionError(s"Internal error: unknown unary operator '$o'")
    }
    emitExprD(expr, ctx).map { base =>
      op match
        case UnaryOp.Neg =>
          if rt.exists(_ == Type.STRING) then base + Code.fromString(StringRuntime.reverseString())
          else base + Code.fromString(getUnopOrFail(NEG))
        case UnaryOp.Not =>
          base + Code.fromString(getUnopOrFail(NOT))
    }

  // --- Statement emission helpers ---

  private def emitBlockD(statements: List[id.Stmt], ctx: Ctx): Either[Diag, Code] =
    Result.traverseE(statements)(emitStmtD(_, ctx)).map(Code.concat)

  private def emitVarDeclD(initOpt: Option[id.Expr], ctx: Ctx): Either[Diag, Code] =
    initOpt match
      case Some(init) => emitExprD(init, ctx)
      case None       => Right(Code.pushNull)

  private def emitAssignD(name: String, value: id.Expr, pos: Int, ctx: Ctx): Either[Diag, Code] =
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

  private def emitFieldAssignD(target: id.Expr, field: String, offset: Int, value: id.Expr, pos: Int, ctx: Ctx): Either[Diag, Code] =
    if offset < 0 then Left(ResolveDiag(Messages.Codegen.unknownField(field), pos))
    else
      for
        targetCode <- emitExprD(target, ctx)
        valueCode  <- emitExprD(value, ctx)
      yield
        val sb = new SamBuilder()
        sb.append(targetCode).addFieldOff(FieldOffset(offset)).append(valueCode).storeInd()
        Code.from(sb)

  private def emitIfD(cond: id.Expr, thenB: id.Stmt, elseB: id.Stmt, ctx: Ctx): Either[Diag, Code] =
    for
      condCode <- emitExprD(cond, ctx)
      thenCode <- emitStmtD(thenB, ctx)
      elseCode <- emitStmtD(elseB, ctx)
    yield
      val elseLbl = Label(); val endLbl = Label()
      val sb = new SamBuilder()
      sb.append(condCode).jumpIfNil(elseLbl)
        .append(thenCode)
        .jump(endLbl)
        .label(elseLbl)
        .append(elseCode)
        .label(endLbl)
      Code.from(sb)

  private def emitWhileD(cond: id.Expr, body: id.Stmt, ctx: Ctx): Either[Diag, Code] =
    val endLabel = Label()
    val innerCtx = ctx.copy(loopEndLabels = endLabel :: ctx.loopEndLabels)
    for
      condCode <- emitExprD(cond, ctx)
      bodyCode <- emitStmtD(body, innerCtx)
    yield
      val start = Label(); val stop = endLabel
      val sb = new SamBuilder()
      sb.label(start)
        .append(condCode).jumpIfNil(stop)
        .append(bodyCode)
        .jump(start)
        .label(stop)
      Code.from(sb)

  private def emitBreakD(pos: Int, ctx: Ctx): Either[Diag, Code] =
    ctx.loopEndLabels match
      case Nil       => Left(SyntaxDiag(Messages.Codegen.breakOutsideLoop, pos))
      case head :: _ => Right(Code.from(new SamBuilder().jump(head)))

  private def emitReturnD(valueOpt: Option[id.Expr], pos: Int, ctx: Ctx): Either[Diag, Code] =
    ctx.returnLabelOpt match
      case None => Left(ResolveDiag(Messages.Codegen.returnLabelNotFound, pos))
      case Some(ret) =>
        val valueCodeE = valueOpt.fold(Right(Code.empty))(emitExprD(_, ctx))
        valueCodeE.map { valueCode =>
          val sb = new SamBuilder()
          sb.append(valueCode).jump(ret)
          Code.from(sb)
        }

  /** Emit code for a statement. Dispatches to specialized helpers. */
  def emitStmtD(s: id.Stmt, ctx: Ctx): Either[Diag, Code] = s match
    case id.Block(statements, _)                        => emitBlockD(statements, ctx)
    case id.VarDecl(_, _, _, initOpt, _)                => emitVarDeclD(initOpt, ctx)
    case id.Assign(name, value, pos)                    => emitAssignD(name, value, pos, ctx)
    case id.FieldAssign(target, field, offset, value, pos) => emitFieldAssignD(target, field, offset, value, pos, ctx)
    case id.If(cond, thenB, elseB, _)                   => emitIfD(cond, thenB, elseB, ctx)
    case id.While(cond, body, _)                        => emitWhileD(cond, body, ctx)
    case id.Break(pos)                                  => emitBreakD(pos, ctx)
    case id.Return(valueOpt, pos)                       => emitReturnD(valueOpt, pos, ctx)
