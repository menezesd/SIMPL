package assignment3

import assignment3.ast.high._
import assignment3.symbol._
import assignment3.ast as id
import assignment3.ast.{Diag, ResolveDiag, SyntaxDiag}
import assignment3.ast.SymbolMethodFrame

/** Emits SAM code from a ProgramNode using existing lower-level codegen utilities (Scala port, idiomatic). */
private object ProgramCodegen {
  // Diagnostic-first API: emitD is the canonical public API and returns Either[Diag, Code].
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
    // Fold over classes and methods, short-circuiting on the first Left(diag)
    val res: Either[Diag, Unit] = program.classes.foldLeft[Either[Diag, Unit]](Right(())) { (accCls, cls) =>
      accCls.flatMap { _ =>
        cls.methods.foldLeft[Either[Diag, Unit]](Right(())) { (accM, m) =>
          accM.flatMap(_ => emitMethodD(localBuilder, m))
        }
      }
    }
    res.map { _ =>
      localBuilder.append(StringRuntime.emitAllStringFunctionsC())
      Code.from(localBuilder)
    }
  }

  private def emitMethod(m: MethodNode): Unit = {
    // Delegate to the diagnostic emitMethodD which returns Either[Diag, Unit].
    // Keep the original behavior for callers that expect an exception on failure.
    emitMethodD(sb, m) match
      case Right(_) => ()
      case Left(diag) => throw new Error(diag.message)
  }

  private def emitMethodD(local: SamBuilder, m: MethodNode): Either[Diag, Unit] = {
    val msOpt = symbols.getMethod(m.className, m.name)
    msOpt match
      case None => Left(ResolveDiag(s"Method symbol missing for '${m.className}.${m.name}'", -1))
      case Some(ms) =>
        val label = ctx.labeler.methodLabel(m.className, m.name)
        local.append("\n").label(label)
        val localCount = ms.numLocals()
        val frame = new SymbolMethodFrame(ms)
        val cleanup = MethodEmit.begin(local, frame, localCount)
        try
          m.body match
            case b: id.Block =>
              val ctx2 = id.IdiomaticCodegen.Ctx(Some(frame), Some(symbols), returnLabelOpt = Some(frame.getReturnLabel))
              id.IdiomaticCodegen.emitStmtD(b, ctx2) match
                case Left(diag) => Left(diag)
                case Right(code) =>
                  local.append(code)
                  import assignment3.ast.high.ReturnSig
                  if (m.returnSig == ReturnSig.Void) then
                    val stmts = b.statements
                    if (stmts.isEmpty || !stmts.last.isInstanceOf[id.Return]) local.pushImm(0)
                  Right(())
            case other => Left(SyntaxDiag(s"Unknown method body type: ${other.getClass.getName}", -1))
        catch
          case e: Exception => Left(SyntaxDiag(s"Body codegen failed: ${e.getMessage}", -1))
        finally MethodEmit.end(local, frame, cleanup)
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
