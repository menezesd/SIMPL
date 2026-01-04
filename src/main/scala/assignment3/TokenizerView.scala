package assignment3

import assignment3.ast.{Diag, Result, SyntaxDiag}
import edu.utexas.cs.sam.io.SamTokenizer
import edu.utexas.cs.sam.io.Tokenizer.TokenType

final class TokenizerView(val tz: SamTokenizer, rules: CompilerUtils.LexicalRules)(using CompilerUtils.RecorderContext) {
  def line: Int = tz.lineNo()
  def col: Int = CompilerUtils.column(tz)
  def at[A](value: A): ParserSupport.At[A] = ParserSupport.at(tz, value)

  def peekKind(): TokenType = tz.peekAtKind()
  def test(word: String): Boolean = tz.test(word)
  def test(ch: Char): Boolean = tz.test(ch)
  def isTypeWord(symbols: assignment3.symbol.ProgramSymbols, currentClassName: String, config: CompilerUtils.TypeCheckConfig): Boolean =
    CompilerUtils.isTypeWord(tz, symbols, currentClassName, config)

  /** Deprecated overload with boolean parameters - use TypeCheckConfig instead. */
  @deprecated("Use TypeCheckConfig instead of boolean parameters", "3.0")
  def isTypeWord(symbols: assignment3.symbol.ProgramSymbols, currentClassName: String, allowUnknown: Boolean, excludeStmtStarters: Boolean): Boolean =
    val config = CompilerUtils.TypeCheckConfig(allowUnknown, excludeStmtStarters)
    isTypeWord(symbols, currentClassName, config)

  def consumeChar(ch: Char): Boolean = ParserSupport.consumeChar(tz, ch)
  def consumeWord(word: String): Boolean = ParserSupport.consumeWord(tz, word)

  def expectChar(ch: Char): Result[Unit] =
    CompilerUtils.expectE(tz, ch, tz.lineNo())

  def expectWord(word: String): Result[Unit] =
    CompilerUtils.expectWordE(tz, word, tz.lineNo())

  def getIdentifier: Result[String] =
    CompilerUtils.getIdentifierE(tz, rules)

  def getWord: Result[String] =
    if tz.peekAtKind() != TokenType.WORD then Left(SyntaxDiag("Expected word", tz.lineNo(), col))
    else Right(CompilerUtils.getWord(tz))

  def getInt: Result[Int] =
    if tz.peekAtKind() != TokenType.INTEGER then Left(SyntaxDiag("Expected integer", tz.lineNo(), col))
    else Right(CompilerUtils.getInt(tz))

  def getString: Result[String] =
    if tz.peekAtKind() != TokenType.STRING then Left(SyntaxDiag("Expected string", tz.lineNo(), col))
    else Right(CompilerUtils.getString(tz))

  def getOp: Result[Char] =
    if tz.peekAtKind() != TokenType.OPERATOR then Left(SyntaxDiag("Expected operator", tz.lineNo(), col))
    else Right(CompilerUtils.getOp(tz))
}
