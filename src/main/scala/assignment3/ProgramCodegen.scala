package assignment3

import assignment3.ast.high._
import assignment3.symbol._
import assignment3.ast as id
import assignment3.ast.{Diag, ResolveDiag, SyntaxDiag}
import assignment3.ast.SymbolMethodFrame

/** Emits SAM code from a ProgramNode using existing lower-level codegen utilities (Scala port, idiomatic). */
private object ProgramCodegen {
  def emit(program: ProgramNode, ctx: CompilerContext): String = {
    ctx.symbols match {
      case Some(symbols) => new ProgramCodegen(symbols, ctx).generate(program)
      case None => throw CompilerException("CompilerContext.symbols is not set", -1)
    }
  }

  // Optional: Code-returning API for safer composition
  def emitC(program: ProgramNode, ctx: CompilerContext): Code =
    emitD(program, ctx) match
      case Right(code) => code
      case Left(diag)  => throw new Exception(diag.message)

  // Diagnostic-first API: returns Either[Diag, Code]
  def emitD(program: ProgramNode, ctx: CompilerContext): Either[Diag, Code] =
    ctx.symbols match
      case Some(symbols) => new ProgramCodegen(symbols, ctx).generateD(program)
      case None => Left(ResolveDiag("CompilerContext.symbols is not set", -1))
}

private final class ProgramCodegen(symbols: ProgramSymbols, ctx: CompilerContext) {
  private val sb = new SamBuilder()

  private def generate(program: ProgramNode): String = {
    appendProgramPreamble(sb, ctx)
    for {
      cls <- program.classes
      m <- cls.methods
    } emitMethod(m)
    sb.append(StringRuntime.emitAllStringFunctionsC())
    sb.toString
  }

  private def generateD(program: ProgramNode): Either[Diag, Code] = {
    val localBuilder = new SamBuilder()
    appendProgramPreamble(localBuilder, ctx)
    for {
      cls <- program.classes
      m <- cls.methods
    } {
      emitMethodD(localBuilder, m) match
        case Left(diag) => return Left(diag)
        case Right(_) => ()
    }
    localBuilder.append(StringRuntime.emitAllStringFunctionsC())
    Right(Code.from(localBuilder))
  }

  private def emitMethod(m: MethodNode): Unit = {
    val ms = symbols.getMethod(m.className, m.name)
      .getOrElse(throw CompilerException(s"Method symbol missing for '${m.className}.${m.name}'", -1))
  val label = ctx.labeler.methodLabel(m.className, m.name)
  sb.append("\n").label(label)
    val localCount = ms.numLocals()
    val frame = new SymbolMethodFrame(ms)
    val cleanup = MethodEmit.begin(sb, frame, localCount)
    try {
      m.body match {
        case b: id.Block =>
          // Use idiomatic codegen directly for method bodies (Code-returning API)
          val ctx2 = id.IdiomaticCodegen.Ctx(Some(frame), Some(symbols), returnLabelOpt = Some(frame.getReturnLabel))
          sb.append(id.IdiomaticCodegen.emitStmtC(b, ctx2))
          import assignment3.ast.high.ReturnSig
          if (m.returnSig == ReturnSig.Void) {
            val stmts = b.statements
            if (stmts.isEmpty || !stmts.last.isInstanceOf[id.Return]) sb.pushImm(0)
          }
        case other => throw CompilerException(s"Unknown method body type: ${other.getClass.getName}", -1)
      }
    } catch {
      case e: Exception => throw CompilerException(s"Body codegen failed: ${e.getMessage}", -1)
    }
    MethodEmit.end(sb, frame, cleanup)
  }

  private def emitMethodD(local: SamBuilder, m: MethodNode): Either[Diag, Unit] = {
    val msOpt = symbols.getMethod(m.className, m.name)
    val ms = msOpt.getOrElse(return Left(ResolveDiag(s"Method symbol missing for '${m.className}.${m.name}'", -1)))
    val label = ctx.labeler.methodLabel(m.className, m.name)
    local.append("\n").label(label)
    val localCount = ms.numLocals()
    val frame = new SymbolMethodFrame(ms)
    val cleanup = MethodEmit.begin(local, frame, localCount)
    try {
      m.body match
        case b: id.Block =>
          val ctx2 = id.IdiomaticCodegen.Ctx(Some(frame), Some(symbols), returnLabelOpt = Some(frame.getReturnLabel))
          id.IdiomaticCodegen.emitStmtD(b, ctx2) match
            case Left(diag) => return Left(diag)
            case Right(code) => local.append(code)
          import assignment3.ast.high.ReturnSig
          if (m.returnSig == ReturnSig.Void) {
            val stmts = b.statements
            if (stmts.isEmpty || !stmts.last.isInstanceOf[id.Return]) local.pushImm(0)
          }
        case other => return Left(SyntaxDiag(s"Unknown method body type: ${other.getClass.getName}", -1))
    } catch {
      case e: Exception => return Left(SyntaxDiag(s"Body codegen failed: ${e.getMessage}", -1))
    } finally {
      MethodEmit.end(local, frame, cleanup)
    }
    Right(())
  }

  private def appendProgramPreamble(sb: SamBuilder, ctx: CompilerContext): Unit = {
    val mainFields = mainFieldsCount(ctx)
  sb.pushImmInt(mainFields)
  sb.malloc()
    // Prepare return slot and 'this' for Main.main
  sb.pushImmInt(0) // return slot
  sb.swap()       // place 'this' below return slot
  sb.call(ctx.labeler.methodLabel(LiveOak3Compiler.ENTRY_CLASS, LiveOak3Compiler.ENTRY_METHOD), 1, returns = false)
  sb.stop()
  }

  private def mainFieldsCount(ctx: CompilerContext): Int = {
    val mainClsOpt = ctx.symbols.flatMap(sym => sym.getClass(LiveOak3Compiler.ENTRY_CLASS))
    mainClsOpt.map(_.numFields()).getOrElse(0)
  }
}
