package assignment3

import assignment3.ast.high._
import assignment3.symbol._
import assignment3.ast as id
import assignment3.ast.{Diag, ResolveDiag, SyntaxDiag}
import assignment3.ast.SymbolMethodFrame
import scala.util.control.NonFatal

/** Emits SAM code from a ProgramNode using existing lower-level codegen utilities (Scala port, idiomatic). */
private object ProgramCodegen {
  // Diagnostic-first API: emitD is the canonical public API and returns Either[Diag, Code].
  def emitD(program: ProgramNode, ctx: CompilerContext): Either[Diag, Code] =
    ctx.symbols match
      case Some(symbols) => new ProgramCodegen(symbols, ctx).generateD(program)
      case None => Left(ResolveDiag("CompilerContext.symbols is not set", -1))
}

private final class ProgramCodegen(symbols: ProgramSymbols, ctx: CompilerContext) {
  private def generateD(program: ProgramNode): Either[Diag, Code] = {
    val localBuilder = new SamBuilder()
    appendProgramPreamble(localBuilder, ctx)
    val res: Either[Diag, Unit] = emitMethodsD(localBuilder, program)
    res.map { _ =>
      localBuilder.append(StringRuntime.emitAllStringFunctionsC())
      Code.from(localBuilder)
    }
  }

  private def emitMethodD(local: SamBuilder, m: MethodNode): Either[Diag, Unit] = {
    ParserSupport.requireMethodResolved(symbols, m.className, m.name).flatMap { ms =>
      val label = Labeler.methodLabel(m.className, m.name)
      local.append("\n").label(label)
      val localCount = ms.numLocals()
      val frame = new SymbolMethodFrame(ms)
      val cleanup = MethodEmit.begin(local, frame, localCount)
      try
        m.body match
          case b: id.Block =>
            val ctx2 = id.IdiomaticCodegen.Ctx(Some(frame), Some(symbols), returnLabelOpt = frame.getReturnLabel)
            id.IdiomaticCodegen.emitStmtD(b, ctx2) match
              case Left(diag) => Left(diag)
              case Right(code) =>
                local.append(code)
                import assignment3.ast.high.ReturnSig
                ensureVoidReturn(local, b, m.returnSig)
                Right(())
          case other => Left(SyntaxDiag(s"Unknown method body type: ${other.getClass.getName}", -1))
      catch
        case NonFatal(e) =>
          val detail = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
          Left(SyntaxDiag(s"Body codegen failed: $detail", -1))
      finally MethodEmit.end(local, frame, cleanup)
    }
  }

  private def emitMethodsD(local: SamBuilder, program: ProgramNode): Either[Diag, Unit] =
    id.Result.sequenceE(program.classes) { cls =>
      id.Result.sequenceE(cls.methods)(m => emitMethodD(local, m))
    }

  private def ensureVoidReturn(local: SamBuilder, b: id.Block, returnSig: assignment3.ast.high.ReturnSig): Unit =
    import assignment3.ast.high.ReturnSig
    if returnSig == ReturnSig.Void && !b.statements.lastOption.exists(_.isInstanceOf[id.Return]) then
      local.pushImm(0)

  private def appendProgramPreamble(sb: SamBuilder, ctx: CompilerContext): Unit = {
    val mainFields = mainFieldsCount(ctx)
    sb.pushImmInt(mainFields)
    sb.malloc()
    // Prepare return slot and 'this' for Main.main
    sb.pushImmInt(0) // return slot
    sb.swap()       // place 'this' below return slot
    sb.call(Labeler.methodLabel(LiveOak3Compiler.ENTRY_CLASS, LiveOak3Compiler.ENTRY_METHOD), 1, returns = false)
    sb.stop()
  }

  private def mainFieldsCount(ctx: CompilerContext): Int = {
    val mainClsOpt = ctx.symbols.flatMap(sym => sym.getClass(LiveOak3Compiler.ENTRY_CLASS))
    mainClsOpt.map(_.numFields()).getOrElse(0)
  }
}
