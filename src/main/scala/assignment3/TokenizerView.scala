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

  def consumeChar(ch: Char): Boolean = ParserSupport.consumeChar(tz, ch)
  def consumeWord(word: String): Boolean = ParserSupport.consumeWord(tz, word)

  def expectChar(ch: Char): Result[Unit] =
    CompilerUtils.expectE(tz, ch, tz.lineNo())

  def expectWord(word: String): Result[Unit] =
    CompilerUtils.expectWordE(tz, word, tz.lineNo())

  def getIdentifier: Result[String] =
    CompilerUtils.getIdentifierE(tz, rules)

  /** Extract token of expected type, returning error diagnostic if mismatch. */
  private def expectToken[A](
    expectedKind: TokenType,
    errorMsg: String,
    extract: => A
  ): Result[A] =
    if tz.peekAtKind() != expectedKind then Left(SyntaxDiag(errorMsg, tz.lineNo(), col))
    else Right(extract)

  def getWord: Result[String] =
    expectToken(TokenType.WORD, "Expected word", CompilerUtils.getWord(tz))

  def getInt: Result[Int] =
    expectToken(TokenType.INTEGER, "Expected integer", CompilerUtils.getInt(tz))

  def getString: Result[String] =
    expectToken(TokenType.STRING, "Expected string", CompilerUtils.getString(tz))

  def getOp: Result[Char] =
    expectToken(TokenType.OPERATOR, "Expected operator", CompilerUtils.getOp(tz))
}
