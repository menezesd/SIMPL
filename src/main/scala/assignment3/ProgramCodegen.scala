package assignment3

import assignment3.ast.high._
import assignment3.symbol._
import assignment3.ast as id
import assignment3.ast.SymbolMethodFrame

/** Emits SAM code from a ProgramNode using existing lower-level codegen utilities (Scala port). */
private object ProgramCodegen {
  def emit(program: ProgramNode, ctx: CompilerContext): String = {
    new ProgramCodegen(ctx.getSymbols, ctx).generate(program)
  }
}

private final class ProgramCodegen(symbols: ProgramSymbols, ctx: CompilerContext) {
  private val sb = new SamBuilder()

  private def generate(program: ProgramNode): String = {
    appendProgramPreamble(sb, ctx)
    for (cls <- program.classes) {
      for (m <- cls.methods) emitMethod(m)
    }
    sb.append(StringRuntime.emitAllStringFunctions())
    sb.toString
  }

  private def emitMethod(m: MethodNode): Unit = {
  val ms = symbols.getMethod(m.className, m.name).getOrElse(throw new CompilerException(s"Method symbol missing for '${m.className}.${m.name}'", -1))
    val label = ctx.getLabeler.methodLabel(m.className, m.name)
    sb.append("\n").append(label).append(":\n")
    val localCount = ms.numLocals()
    val frame = new SymbolMethodFrame(ms)
    val cleanup = MethodEmit.begin(sb, frame, localCount)
    try {
      m.body match {
        case b: id.Block =>
          // Use idiomatic codegen directly for method bodies
          val ctx = id.IdiomaticCodegen.Ctx(Some(frame), Some(symbols), returnLabelOpt = Some(frame.getReturnLabel))
          sb.append(id.IdiomaticCodegen.emitStmt(b, ctx))
          import assignment3.ast.high.ReturnSig
          if (m.returnSig == ReturnSig.Void) {
            val stmts = b.statements
            if (stmts.isEmpty || !stmts.last.isInstanceOf[id.Return]) {
              sb.append("PUSHIMM 0\n")
            }
          }
        case other => throw new CompilerException("Unknown method body type: " + other.getClass.getName, -1)
      }
    } catch {
      case e: Exception => throw new CompilerException("Body codegen failed: " + e.getMessage, -1)
    }
    MethodEmit.end(sb, frame, cleanup)
  }

  private def appendProgramPreamble(sb: SamBuilder, ctx: CompilerContext): Unit = {
    val mainFields = mainFieldsCount(ctx)
    sb.append("PUSHIMM ").append(Integer.toString(mainFields)).append("\nMALLOC\n")
    // Prepare return slot and 'this' for Main.main
    sb.append("PUSHIMM 0\n") // return slot
    sb.append("SWAP\n")      // place 'this' below return slot
    sb.append("LINK\nJSR ").append(ctx.getLabeler.methodLabel(LiveOak3Compiler.ENTRY_CLASS, LiveOak3Compiler.ENTRY_METHOD)).append("\nUNLINK\n")
    // Pop parameter ('this') and then drop void return slot
    sb.append("ADDSP -1\n") // pop 'this'
    sb.append("ADDSP -1\nSTOP\n") // drop return slot and stop
  }

  private def mainFieldsCount(ctx: CompilerContext): Int = {
    val mainClsOpt = Option(ctx).flatMap(c => Option(c.getSymbols)).flatMap(_.getClass(LiveOak3Compiler.ENTRY_CLASS))
    mainClsOpt.map(_.numFields()).getOrElse(0)
  }
}
