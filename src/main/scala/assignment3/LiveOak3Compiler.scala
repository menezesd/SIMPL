package assignment3

import assignment3.symbol._
import edu.utexas.cs.sam.io.SamTokenizer
import assignment3.ast.Diag
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.util.Using
import scala.jdk.CollectionConverters._
import scala.annotation.static

class LiveOak3Compiler // companion for static members

object LiveOak3Compiler {
  @static final val ENTRY_CLASS: String  = "Main"
  @static final val ENTRY_METHOD: String = "main"

  @static def reset(): Unit =
    CompilerUtils.clearTokens()(using CompilerUtils.RecorderContext.default)

  // Helper to run side-effecting code within for-comprehension
  private inline def unit(body: => Unit): Either[Diag, Unit] = { body; Right(()) }

  // NOTE: compileD is the canonical public API (diagnostic-first). Remove the
  // legacy throwing adapter to discourage exception-based flow.

  // Diagnostic-first API: returns CompilationResult
  def compileD(fileName: String): CompilationResult = {
    val ctx = new CompilerContext()
    given CompilerUtils.RecorderContext = CompilerUtils.RecorderContext(Some(ctx.recorder))
    try {
      val resultEither =
        for
          _ <- unit(logStage(CompilationStage.Symbols, "build symbols"))
          symbols <- buildSymbolsD(fileName, ctx)
          _ <- unit(logStage(CompilationStage.Validate, "validate entrypoint"))
          _ <- ValidationStage.validateEntrypointD(symbols, ENTRY_CLASS, ENTRY_METHOD)
          _ <- unit(logStage(CompilationStage.Parse, "parse program"))
          program <- parseProgramD(fileName, symbols)
          _ <- unit(if (Debug.enabled("tokens")) dumpRecordedTokens(ctx))
          _ <- unit(logStage(CompilationStage.Codegen, "emit code"))
          code <- emitCodeD(program, ctx)
        yield
          val out = code.toString
          if (Debug.enabled("sam")) Debug.log("sam", () => s"Generated SAM size: ${out.length} chars")
          out
      resultEither match
        case Right(out) => CompilationResult.success(out)
        case Left(diag) => CompilationResult.failure(diag)
    } finally { cleanup(ctx) }
  }

  private def cleanup(ctx: CompilerContext): Unit = ctx.recorder.clear()

  // legacy throwing variant removed; use ValidationStage.validateEntrypointD instead when you
  // need a diagnostic result.

  private def buildSymbolsD(fileName: String, ctx: CompilerContext)(using CompilerUtils.RecorderContext): Either[Diag, ProgramSymbols] = {
    val pass1 = new SamTokenizer(fileName, SamTokenizer.TokenizerOptions.PROCESS_STRINGS)
    val stb = new SymbolTableBuilder()
    stb.buildD(pass1).map { symbols =>
      ctx.symbols = symbols
      if (Debug.enabled("symbols")) dumpSymbols(symbols)
      symbols
    }
  }

  private def parseProgramD(fileName: String, symbols: ProgramSymbols)(using CompilerUtils.RecorderContext): Either[Diag, assignment3.ast.high.ProgramNode] =
    ProgramParser.parseD(fileName, symbols)

  private def emitCodeD(program: assignment3.ast.high.ProgramNode, ctx: CompilerContext): Either[Diag, Code] =
    ProgramCodegen.emitD(program, ctx)

  private def logStage(stage: CompilationStage, msg: => String): Unit =
    if (Debug.enabled("stage")) Debug.log("stage", () => s"${stage.toString.toLowerCase}: $msg")

  def compileEitherD(fileName: String): Either[Diag, String] =
    compileD(fileName).toEither

  @deprecated("Use compileEitherD or compileD instead for diagnostic-first error handling", "3.0")
  @static def compiler(fileName: String): String = {
    try {
      compileEitherD(fileName) match
        case Right(code) => code
        case Left(diag) =>
          // Preserve test/legacy behavior: print failure message and throw Error
          System.err.println("Failed to compile " + normalizePathUnix(fileName))
          throw new Error(diag.message + " at " + diag.line + ":" + diag.column)
    } catch {
      // Only handle unexpected non-Error Throwables here; if we intentionally
      // threw an Error above, let it propagate so we don't double-print.
      case t: Throwable if !t.isInstanceOf[Error] =>
        System.err.println("Failed to compile " + normalizePathUnix(fileName))
        throw new Error(t)
    }
  }

  private def normalizePathUnix(path: String): String =
    Option(path).map(_.replace('\\', '/')).getOrElse("")

  private def dumpSymbols(symbols: ProgramSymbols): Unit = {
    Debug.log("symbols", () => "Program symbols:")
    for (cs <- symbols.allClasses) {
      Debug.log("symbols", () => s"  class ${cs.getName} (fields=${cs.numFields()})")
      for (ms <- cs.allMethods) {
        val ret = ms.getReturnSig match {
          case assignment3.ast.high.ReturnSig.Void => "void"
          case assignment3.ast.high.ReturnSig.Obj(cn) => cn
          case assignment3.ast.high.ReturnSig.Prim(t) => String.valueOf(t)
        }
        Debug.log("symbols", () => s"    method ${ms.getName} -> $ret params=${ms.numParameters()} locals=${ms.numLocals()}")
      }
    }
  }

  private def dumpRecordedTokens(ctx: CompilerContext): Unit = ctx.recorder match {
    case l: ListTokenRecorder =>
      val toks = l.snapshot()
      Debug.log("tokens", () => s"Recorded tokens (${toks.size}):")
      val limit = Math.min(80, toks.size)
      val head = toks.subList(0, limit).asScala.mkString(" ")
      val suffix = if toks.size > limit then " ..." else ""
      Debug.log("tokens", () => head + suffix)
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
    compileEitherD(inFile) match
      case Right(samCode) =>
        Using.resource(new BufferedWriter(new FileWriter(outFile))) { w =>
          w.write(samCode)
        }
      case Left(diag) =>
        System.err.println(s"${diag.message} at ${diag.line}:${diag.column}")
        System.exit(1)
  }
}
