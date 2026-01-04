package assignment3

import edu.utexas.cs.sam.io.SamTokenizer
import edu.utexas.cs.sam.io.Tokenizer.TokenType
import scala.util.matching.Regex

object CompilerUtils {
  final case class RecorderContext(recorder: Option[TokenRecorder])
  object RecorderContext {
    given default: RecorderContext = RecorderContext(None)
  }
  def noRecorder(): RecorderContext = RecorderContext(None)
  def withRecorder(recorder: TokenRecorder): RecorderContext = RecorderContext(Some(recorder))

  /** Type alias for context function with RecorderContext. */
  type WithRecorder[A] = RecorderContext ?=> A

  final case class LexicalRules(
    identifierPattern: Regex,
    reservedWords: Set[String],
    statementStarters: Set[String]
  )
  object LexicalRules {
    val Default: LexicalRules = LexicalRules(
      "^[A-Za-z][A-Za-z0-9_]*$".r,
      Set("class", "if", "else", "while", "return", "break", "true", "false", "null", "new", "this", "int", "bool", "String", "void"),
      Set("return", "if", "else", "while", "class", "new", "this", "true", "false", "null", "void")
    )
  }

  def clearTokens()(using ctx: RecorderContext): Unit = ctx.recorder.foreach(_.clear())
  private def rec(t: String)(using ctx: RecorderContext): Unit =
    Option(t).foreach(s => ctx.recorder.foreach(_.record(s)))

  import assignment3.ast.{Diag, SyntaxDiag}

  // --- Either-returning variants (preferred for new code) ---

  /** Generic expectation framework for token types. */
  private def expectTokenType[A](
    f: SamTokenizer,
    expectedKind: TokenType,
    errorMsg: String,
    extract: SamTokenizer => A,
    record: A => String,
    line: Int
  )(using RecorderContext): Either[Diag, A] =
    if f.peekAtKind != expectedKind then Left(SyntaxDiag(errorMsg, line, column(f)))
    else
      val v = extract(f)
      rec(record(v))
      Right(v)

  def expectE(f: SamTokenizer, expected: Char, line: Int)(using RecorderContext): Either[Diag, Unit] =
    if (!f.check(expected)) Left(SyntaxDiag(s"Expected '$expected'", line, column(f)))
    else { rec(expected.toString); Right(()) }

  def expectE(f: SamTokenizer, expected: String, line: Int)(using RecorderContext): Either[Diag, Unit] =
    if (!f.check(expected)) Left(SyntaxDiag(s"Expected '$expected'", line, column(f)))
    else { rec(expected); Right(()) }

  def expectIdentifierE(f: SamTokenizer, line: Int)(using RecorderContext): Either[Diag, String] =
    expectTokenType(f, TokenType.WORD, "Expected identifier", _.getWord, identity, line)

  def expectIntE(f: SamTokenizer, line: Int)(using RecorderContext): Either[Diag, Int] =
    expectTokenType(f, TokenType.INTEGER, "Expected integer", _.getInt, _.toString, line)

  def expectStringE(f: SamTokenizer, line: Int)(using RecorderContext): Either[Diag, String] =
    expectTokenType(f, TokenType.STRING, "Expected string literal", _.getString, s => s"\"$s\"", line)

  def expectWordE(f: SamTokenizer, expected: String, line: Int)(using RecorderContext): Either[Diag, Unit] =
    if (f.peekAtKind != TokenType.WORD) Left(SyntaxDiag(s"Expected '$expected'", line, column(f)))
    else {
      val w = f.getWord; rec(w)
      if (w != expected) Left(SyntaxDiag(s"Expected '$expected'", line, column(f)))
      else Right(())
    }

  def getIdentifierE(f: SamTokenizer)(using RecorderContext): Either[Diag, String] =
    getIdentifierE(f, LexicalRules.Default)

  def getIdentifierE(f: SamTokenizer, rules: LexicalRules)(using RecorderContext): Either[Diag, String] =
    if (f.peekAtKind != TokenType.WORD) Left(SyntaxDiag("Expected identifier", f.lineNo, column(f)))
    else {
      val id = f.getWord; rec(id)
      if (rules.reservedWords.contains(id)) Left(SyntaxDiag(s"Reserved word cannot be used as identifier: $id", f.lineNo, column(f)))
      else if (!rules.identifierPattern.matches(id)) Left(SyntaxDiag(s"Invalid identifier: $id", f.lineNo, column(f)))
      else Right(id)
    }

  def expectOperatorE(f: SamTokenizer, line: Int)(using RecorderContext): Either[Diag, Char] =
    expectTokenType(f, TokenType.OPERATOR, "Expected operator", _.getOp, _.toString, line)

  // --- Non-throwing utility methods ---

  def check(f: SamTokenizer, expected: Char)(using RecorderContext): Boolean = {
    val ok = f.check(expected); if (ok) rec(expected.toString); ok
  }
  def check(f: SamTokenizer, expected: String)(using RecorderContext): Boolean = {
    val ok = f.check(expected); if (ok) rec(expected); ok
  }
  def getWord(f: SamTokenizer)(using RecorderContext): String = { val w = f.getWord; rec(w); w }
  def getString(f: SamTokenizer)(using RecorderContext): String = { val s = f.getString; rec(s"\"$s\""); s }
  def getInt(f: SamTokenizer)(using RecorderContext): Int = { val v = f.getInt; rec(v.toString); v }
  def getOp(f: SamTokenizer)(using RecorderContext): Char = { val o = f.getOp; rec(o.toString); o }
  def skipToken(f: SamTokenizer)(using RecorderContext): Unit = { f.skipToken(); rec(".") }
  def column(f: SamTokenizer): Int = TokenizerOps.column(f)

  /** Java-compatible overload with default LexicalRules. */
  def isTypeWord(
    tz: SamTokenizer,
    symbols: assignment3.symbol.ProgramSymbols,
    currentClassName: String,
    allowUnknownNames: Boolean,
    excludeStmtStarters: Boolean
  ): Boolean =
    isTypeWord(tz, symbols, currentClassName, allowUnknownNames, excludeStmtStarters, LexicalRules.Default)

  /** Scala 3 version with using clause for custom LexicalRules. */
  def isTypeWord(
    tz: SamTokenizer,
    symbols: assignment3.symbol.ProgramSymbols,
    currentClassName: String,
    allowUnknownNames: Boolean,
    excludeStmtStarters: Boolean,
    rules: LexicalRules
  ): Boolean = {
    if (tz.peekAtKind != TokenType.WORD) return false
    if (excludeStmtStarters && rules.statementStarters.exists(tz.test)) return false
    if (tz.test("int") || tz.test("bool") || tz.test("String")) return true
    if (Option(currentClassName).exists(tz.test)) return true
    val classMatch = Option(symbols).exists(sym => sym.allClasses.exists(cls => tz.test(cls.getName)))
    if (classMatch) true else allowUnknownNames
  }
  def parseTypeOrObjectName(raw: String, line: Int): ValueType =
    Type.parseE(raw, line) match
      case Right(t) => ValueType.ofPrimitive(t)
      case Left(_)  => ValueType.ofObject(raw)
}
