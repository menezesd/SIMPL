package assignment3

import edu.utexas.cs.sam.io.SamTokenizer
import edu.utexas.cs.sam.io.Tokenizer.TokenType
import scala.util.matching.Regex

object CompilerUtils {
  val IdentifierPattern: Regex = "^[A-Za-z][A-Za-z0-9_]*$".r
  val ReservedWords: Set[String] = Set(
    "class", "if", "else", "while", "return", "break", "true", "false", "null", "new", "this", "int", "bool", "String", "void"
  )
  val StatementStarters: Set[String] = Set(
    "return", "if", "else", "while", "class", "new", "this", "true", "false", "null", "void"
  )
  private var recorder: Option[TokenRecorder] = None
  def setRecorder(r: TokenRecorder): Unit = recorder = Some(r)
  def clearRecorder(): Unit = recorder = None
  def clearTokens(): Unit = recorder.foreach(_.clear())
  private def rec(t: String): Unit = Option(t).foreach(s => recorder.foreach(_.record(s)))

  def expect(f: SamTokenizer, expected: Char, line: Int): Unit =
    if (!f.check(expected)) throw SyntaxErrorException(s"Expected '$expected'", line, column(f)) else rec(expected.toString)
  def expect(f: SamTokenizer, expected: String, line: Int): Unit =
    if (!f.check(expected)) throw SyntaxErrorException(s"Expected '$expected'", line, column(f)) else rec(expected)
  def expectIdentifier(f: SamTokenizer, line: Int): String = {
    if (f.peekAtKind != TokenType.WORD) throw SyntaxErrorException("Expected identifier", line, column(f))
    val w = f.getWord; rec(w); w
  }
  def expectInt(f: SamTokenizer, line: Int): Int = {
    if (f.peekAtKind != TokenType.INTEGER) throw SyntaxErrorException("Expected integer", line, column(f))
    val v = f.getInt; rec(v.toString); v
  }
  def expectString(f: SamTokenizer, line: Int): String = {
    if (f.peekAtKind != TokenType.STRING) throw SyntaxErrorException("Expected string literal", line, column(f))
    val s = f.getString; rec(s"\"$s\""); s
  }
  def expectWord(f: SamTokenizer, expected: String, line: Int): Unit = {
    if (f.peekAtKind != TokenType.WORD) throw SyntaxErrorException(s"Expected '$expected'", line, column(f))
    val w = f.getWord; rec(w); if (w != expected) throw SyntaxErrorException(s"Expected '$expected'", line, column(f))
  }
  def getIdentifier(f: SamTokenizer): String = {
    if (f.peekAtKind != TokenType.WORD) throw SyntaxErrorException("Expected identifier", f.lineNo, column(f))
    val id = f.getWord; rec(id)
    if (ReservedWords.contains(id)) throw SyntaxErrorException(s"Reserved word cannot be used as identifier: $id", f.lineNo, column(f))
    if (!IdentifierPattern.matches(id)) throw SyntaxErrorException(s"Invalid identifier: $id", f.lineNo, column(f))
    id
  }
  def expectChar(f: SamTokenizer, expected: Char, line: Int): Unit = expect(f, expected, line)
  def expectOperator(f: SamTokenizer, line: Int): Char = {
    if (f.peekAtKind != TokenType.OPERATOR) throw SyntaxErrorException("Expected operator", line, column(f))
    val op = f.getOp; rec(op.toString); op
  }
  def check(f: SamTokenizer, expected: Char): Boolean = {
    val ok = f.check(expected); if (ok) rec(expected.toString); ok
  }
  def check(f: SamTokenizer, expected: String): Boolean = {
    val ok = f.check(expected); if (ok) rec(expected); ok
  }
  def getWord(f: SamTokenizer): String = { val w = f.getWord; rec(w); w }
  def getString(f: SamTokenizer): String = { val s = f.getString; rec(s"\"$s\""); s }
  def getInt(f: SamTokenizer): Int = { val v = f.getInt; rec(v.toString); v }
  def getOp(f: SamTokenizer): Char = { val o = f.getOp; rec(o.toString); o }
  def skipToken(f: SamTokenizer): Unit = { f.skipToken(); rec(".") }
  def column(f: SamTokenizer): Int = TokenizerOps.column(f)
  def isTypeWord(
    tz: SamTokenizer,
    symbols: assignment3.symbol.ProgramSymbols,
    currentClassName: String,
    allowUnknownNames: Boolean,
    excludeStmtStarters: Boolean
  ): Boolean = {
    if (tz.peekAtKind != TokenType.WORD) return false
    if (excludeStmtStarters && StatementStarters.exists(tz.test)) return false
    if (tz.test("int") || tz.test("bool") || tz.test("String")) return true
    if (Option(currentClassName).exists(tz.test)) return true
    Option(symbols).foreach { sym => for (cls <- sym.allClasses) if (tz.test(cls.getName)) return true }
    allowUnknownNames
  }
  def parseTypeOrObjectName(raw: String, line: Int): ValueType = try ValueType.ofPrimitive(Type.parse(raw, line)) catch { case _: TypeErrorException => ValueType.ofObject(raw) }
}
