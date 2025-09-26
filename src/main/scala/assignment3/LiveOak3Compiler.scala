package assignment3

import assignment3.symbol._
import edu.utexas.cs.sam.io.SamTokenizer
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters._
import scala.annotation.static

class LiveOak3Compiler // companion for static members

object LiveOak3Compiler {
  @static final val ENTRY_CLASS: String  = "Main"
  @static final val ENTRY_METHOD: String = "main"

  @static def reset(): Unit = CompilerUtils.clearTokens()

  @static @throws[Exception]
  def compile(fileName: String): String = {
    reset()
    val ctx = new CompilerContext()
    CompilerUtils.setRecorder(ctx.recorder)
    try {
      val pass1 = new SamTokenizer(fileName, SamTokenizer.TokenizerOptions.PROCESS_STRINGS)
      val stb   = new SymbolTableBuilder()
      val symbols = stb.build(pass1)
      symbols.freeze()
  ctx.symbols = symbols
      if (Debug.enabled("symbols")) dumpSymbols(symbols)
      validateEntrypoint(symbols)
      val program = ProgramParser.parse(fileName, symbols)
  if (Debug.enabled("tokens")) dumpRecordedTokens(ctx)
      val out = ProgramCodegen.emit(program, ctx)
      if (Debug.enabled("sam")) Debug.log("sam", () => s"Generated SAM size: ${out.length} chars")
      out
    } catch {
      case ce: CompilerException => throw ce
      case t: Throwable          => throw new CompilerException(t.getMessage, -1)
    } finally { cleanup() }
  }

  private def cleanup(): Unit = { CompilerUtils.clearTokens(); CompilerUtils.clearRecorder() }

  private def validateEntrypoint(symbols: ProgramSymbols): Unit = {
    val ms = symbols.getEntrypoint().getOrElse(throw new CompilerException(s"Missing $ENTRY_CLASS.$ENTRY_METHOD entry point", -1))
    if (ms.expectedUserArgs() != 0)
      throw new CompilerException(s"$ENTRY_CLASS.$ENTRY_METHOD must not declare parameters", entrypointErrorLine(ms))
    if (ms.parameters.isEmpty || !ms.parameters.head.isObject || ms.parameters.head.getClassTypeName != ENTRY_CLASS)
      throw new CompilerException(s"$ENTRY_CLASS.$ENTRY_METHOD must be an instance method of $ENTRY_CLASS", entrypointErrorLine(ms))
  }

  private def entrypointErrorLine(ms: MethodSymbol): Int = { val ln = ms.getBodyStartLine(); if (ln > 0) ln else -1 }

  @static def compiler(fileName: String): String = {
    try compile(fileName) catch {
      case t: Throwable =>
        System.err.println("Failed to compile " + normalizePathUnix(fileName))
        cleanup()
        throw new Error(t)
    }
  }

  private def normalizePathUnix(path: String): String = Option(path).map(_.replace('\\', '/')).orNull

  private def dumpSymbols(symbols: ProgramSymbols): Unit = {
    System.err.println("[DEBUG] Program symbols:")
    for (cs <- symbols.allClasses) {
      System.err.println(s"  class ${cs.getName} (fields=${cs.numFields()})")
      for (ms <- cs.allMethods) {
        val ret = ms.getReturnSig match {
          case assignment3.ast.high.ReturnSig.Void => "void"
          case assignment3.ast.high.ReturnSig.Obj(cn) => cn
          case assignment3.ast.high.ReturnSig.Prim(t) => String.valueOf(t)
        }
        System.err.println(s"    method ${ms.getName} -> $ret params=${ms.numParameters()} locals=${ms.numLocals()}")
      }
    }
  }

  private def dumpRecordedTokens(ctx: CompilerContext): Unit = ctx.recorder match {
    case l: ListTokenRecorder =>
      val toks = l.snapshot()
      System.err.println(s"[DEBUG] Recorded tokens (${toks.size}):")
      val limit = Math.min(80, toks.size)
      val head = toks.subList(0, limit).asScala.mkString(" ")
      System.err.print(head)
      if (toks.size > limit) System.err.print(" ...")
      System.err.println()
    case _ => ()
  }

  @static @throws[IOException]
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      System.err.println("Usage: java LiveOak3Compiler <inputFile> <outputFile>")
      System.exit(1)
    }
    val inFile = args(0)
    val outFile = args(1)
    try {
      val samCode = compile(inFile)
      val w = new BufferedWriter(new FileWriter(outFile))
      try w.write(samCode) finally w.close()
    } catch {
      case e: Exception =>
        System.err.println("Failed to compile " + normalizePathUnix(inFile))
        cleanup()
        throw new Error(e)
    }
  }
}
