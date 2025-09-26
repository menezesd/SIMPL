package assignment3

import assignment3.symbol._
import edu.utexas.cs.sam.io.SamTokenizer
import assignment3.ast.Diag
import assignment3.ast.ResolveDiag
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.util.Using
import scala.jdk.CollectionConverters._
import scala.annotation.static

class LiveOak3Compiler // companion for static members

object LiveOak3Compiler {
  @static final val ENTRY_CLASS: String  = "Main"
  @static final val ENTRY_METHOD: String = "main"

  @static def reset(): Unit = CompilerUtils.clearTokens()

  // NOTE: compileD is the canonical public API (diagnostic-first). Remove the
  // legacy throwing adapter to discourage exception-based flow.

  // Diagnostic-first API: returns Either[Diag, String]
  def compileD(fileName: String): Either[Diag, String] = {
    reset()
    val ctx = new CompilerContext()
    CompilerUtils.setRecorder(ctx.recorder)
    try {
      val pass1 = new SamTokenizer(fileName, SamTokenizer.TokenizerOptions.PROCESS_STRINGS)
      val stb   = new SymbolTableBuilder()
  val symbols = stb.buildD(pass1) match { case Left(d) => return Left(d); case Right(s) => s }
  symbols.freeze()
      ctx.symbols = symbols
      if (Debug.enabled("symbols")) dumpSymbols(symbols)
      validateEntrypointD(symbols) match
        case Left(d) => return Left(d)
        case Right(_) => ()
  val program = ProgramParser.parseD(fileName, symbols) match { case Left(d) => return Left(d); case Right(p) => p }
      if (Debug.enabled("tokens")) dumpRecordedTokens(ctx)
      ProgramCodegen.emitD(program, ctx) match
        case Left(d) => Left(d)
        case Right(code) =>
          val out = code.toString
          if (Debug.enabled("sam")) Debug.log("sam", () => s"Generated SAM size: ${out.length} chars")
          Right(out)
    } catch {
      case t: Throwable => throw t
    } finally { cleanup() }
  }

  private def cleanup(): Unit = { CompilerUtils.clearTokens(); CompilerUtils.clearRecorder() }

  // legacy throwing variant removed; use validateEntrypointD instead when you
  // need a diagnostic result.

  private def validateEntrypointD(symbols: ProgramSymbols): Either[Diag, Unit] = {
    symbols.getEntrypoint() match
      case None => Left(ResolveDiag(s"Missing $ENTRY_CLASS.$ENTRY_METHOD entry point", -1))
      case Some(ms) =>
        if (ms.expectedUserArgs() != 0) Left(ResolveDiag(s"$ENTRY_CLASS.$ENTRY_METHOD must not declare parameters", entrypointErrorLine(ms)))
        else if (ms.parameters.isEmpty || !ms.parameters.head.isObject || ms.parameters.head.getClassTypeName != ENTRY_CLASS)
          Left(ResolveDiag(s"$ENTRY_CLASS.$ENTRY_METHOD must be an instance method of $ENTRY_CLASS", entrypointErrorLine(ms)))
        else Right(())
  }

  private def entrypointErrorLine(ms: MethodSymbol): Int = { val ln = ms.getBodyStartLine(); if (ln > 0) ln else -1 }

  @static def compiler(fileName: String): String = {
    try {
      compileD(fileName) match
        case Right(code) => code
        case Left(diag) =>
          // Preserve test/legacy behavior: print failure message and throw Error
          System.err.println("Failed to compile " + normalizePathUnix(fileName))
          cleanup()
          throw new Error(diag.message + " at " + diag.line + ":" + diag.column)
    } catch {
      // Only handle unexpected non-Error Throwables here; if we intentionally
      // threw an Error above, let it propagate so we don't double-print.
      case t: Throwable if !t.isInstanceOf[Error] =>
        System.err.println("Failed to compile " + normalizePathUnix(fileName))
        cleanup()
        throw new Error(t)
    }
  }

  private def normalizePathUnix(path: String): String = Option(path).map(_.replace('\\', '/')).getOrElse(null)

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
    compileD(inFile) match
      case Right(samCode) =>
        Using.resource(new BufferedWriter(new FileWriter(outFile))) { w =>
          w.write(samCode)
        }
      case Left(diag) =>
        System.err.println(s"${diag.message} at ${diag.line}:${diag.column}")
        cleanup()
        System.exit(1)
  }
}
