package assignment3

import assignment3.ast.{Diag, ResolveDiag, Result, SyntaxDiag, TypeDiag}
import edu.utexas.cs.sam.io.SamTokenizer

object ParserSupport {
  final case class At[A](value: A, line: Int, col: Int)

  def at[A](tz: SamTokenizer, value: A): At[A] =
    At(value, tz.lineNo(), CompilerUtils.column(tz))

  def syntax(msg: String, tz: SamTokenizer): Diag =
    SyntaxDiag(msg, tz.lineNo(), CompilerUtils.column(tz))

  def typeE(msg: String, tz: SamTokenizer): Diag =
    TypeDiag(msg, tz.lineNo(), CompilerUtils.column(tz))

  def resolveE(msg: String, tz: SamTokenizer): Diag =
    ResolveDiag(msg, tz.lineNo(), CompilerUtils.column(tz))

  def wrapSyntax[A](tz: SamTokenizer)(body: => A): Either[Diag, A] =
    try Right(body)
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  def requireClass(symbols: assignment3.symbol.ProgramSymbols, className: String, tz: SamTokenizer): Result[assignment3.symbol.ClassSymbol] =
    symbols.getClass(className) match
      case Some(cs) => Right(cs)
      case None => Left(syntax(Messages.classSymbolMissing(className), tz))

  def requireMethod(
    symbols: assignment3.symbol.ProgramSymbols,
    className: String,
    methodName: String,
    tz: SamTokenizer
  ): Result[assignment3.symbol.MethodSymbol] =
    symbols.getMethod(className, methodName) match
      case Some(ms) => Right(ms)
      case None => Left(syntax(Messages.methodSymbolMissing(className, methodName), tz))

  def requireMethodResolved(
    symbols: assignment3.symbol.ProgramSymbols,
    className: String,
    methodName: String
  ): Result[assignment3.symbol.MethodSymbol] =
    symbols.getMethod(className, methodName) match
      case Some(ms) => Right(ms)
      case None => Left(ResolveDiag(Messages.methodSymbolMissing(className, methodName), -1))

  def consumeChar(tz: SamTokenizer, ch: Char)(using CompilerUtils.RecorderContext): Boolean =
    CompilerUtils.check(tz, ch)

  def consumeWord(tz: SamTokenizer, word: String)(using CompilerUtils.RecorderContext): Boolean =
    CompilerUtils.check(tz, word)
}
