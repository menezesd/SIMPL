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

  def expect(f: SamTokenizer, expected: Char, line: Int)(using RecorderContext): Unit =
    if (!f.check(expected)) throw SyntaxErrorException(s"Expected '$expected'", line, column(f)) else rec(expected.toString)
  def expect(f: SamTokenizer, expected: String, line: Int)(using RecorderContext): Unit =
    if (!f.check(expected)) throw SyntaxErrorException(s"Expected '$expected'", line, column(f)) else rec(expected)
  def expectIdentifier(f: SamTokenizer, line: Int)(using RecorderContext): String = {
    if (f.peekAtKind != TokenType.WORD) throw SyntaxErrorException("Expected identifier", line, column(f))
    val w = f.getWord; rec(w); w
  }
  def expectInt(f: SamTokenizer, line: Int)(using RecorderContext): Int = {
    if (f.peekAtKind != TokenType.INTEGER) throw SyntaxErrorException("Expected integer", line, column(f))
    val v = f.getInt; rec(v.toString); v
  }
  def expectString(f: SamTokenizer, line: Int)(using RecorderContext): String = {
    if (f.peekAtKind != TokenType.STRING) throw SyntaxErrorException("Expected string literal", line, column(f))
    val s = f.getString; rec(s"\"$s\""); s
  }
  def expectWord(f: SamTokenizer, expected: String, line: Int)(using RecorderContext): Unit = {
    if (f.peekAtKind != TokenType.WORD) throw SyntaxErrorException(s"Expected '$expected'", line, column(f))
    val w = f.getWord; rec(w); if (w != expected) throw SyntaxErrorException(s"Expected '$expected'", line, column(f))
  }
  def getIdentifier(f: SamTokenizer)(using RecorderContext): String =
    getIdentifier(f, LexicalRules.Default)

  def getIdentifier(f: SamTokenizer, rules: LexicalRules)(using RecorderContext): String = {
    if (f.peekAtKind != TokenType.WORD) throw SyntaxErrorException("Expected identifier", f.lineNo, column(f))
    val id = f.getWord; rec(id)
    if (rules.reservedWords.contains(id)) throw SyntaxErrorException(s"Reserved word cannot be used as identifier: $id", f.lineNo, column(f))
    if (!rules.identifierPattern.matches(id)) throw SyntaxErrorException(s"Invalid identifier: $id", f.lineNo, column(f))
    id
  }
  def expectChar(f: SamTokenizer, expected: Char, line: Int)(using RecorderContext): Unit = expect(f, expected, line)
  def expectOperator(f: SamTokenizer, line: Int)(using RecorderContext): Char = {
    if (f.peekAtKind != TokenType.OPERATOR) throw SyntaxErrorException("Expected operator", line, column(f))
    val op = f.getOp; rec(op.toString); op
  }
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
  def isTypeWord(
    tz: SamTokenizer,
    symbols: assignment3.symbol.ProgramSymbols,
    currentClassName: String,
    allowUnknownNames: Boolean,
    excludeStmtStarters: Boolean
  ): Boolean =
    isTypeWord(tz, symbols, currentClassName, allowUnknownNames, excludeStmtStarters, LexicalRules.Default)

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
  def parseTypeOrObjectName(raw: String, line: Int): ValueType = try ValueType.ofPrimitive(Type.parse(raw, line)) catch { case _: TypeErrorException => ValueType.ofObject(raw) }
}
