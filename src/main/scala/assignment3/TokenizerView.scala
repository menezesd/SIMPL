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
  def isTypeWord(symbols: assignment3.symbol.ProgramSymbols, currentClassName: String, allowUnknown: Boolean, excludeStmtStarters: Boolean): Boolean =
    CompilerUtils.isTypeWord(tz, symbols, currentClassName, allowUnknown, excludeStmtStarters, rules)

  def consumeChar(ch: Char): Boolean = ParserSupport.consumeChar(tz, ch)
  def consumeWord(word: String): Boolean = ParserSupport.consumeWord(tz, word)

  def expectChar(ch: Char): Result[Unit] =
    CompilerUtils.expectE(tz, ch, tz.lineNo())

  def expectWord(word: String): Result[Unit] =
    CompilerUtils.expectWordE(tz, word, tz.lineNo())

  def getIdentifier: Result[String] =
    CompilerUtils.getIdentifierE(tz, rules)

  def getWord: Result[String] =
    wrapSyntax { CompilerUtils.getWord(tz) }

  def getInt: Result[Int] =
    wrapSyntax { CompilerUtils.getInt(tz) }

  def getString: Result[String] =
    wrapSyntax { CompilerUtils.getString(tz) }

  def getOp: Result[Char] =
    wrapSyntax { CompilerUtils.getOp(tz) }

  private def wrapSyntax[A](body: => A): Result[A] =
    ParserSupport.wrapSyntax(tz)(body)
}
