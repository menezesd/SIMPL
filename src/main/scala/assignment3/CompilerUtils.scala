package assignment3

import edu.utexas.cs.sam.io.SamTokenizer
import edu.utexas.cs.sam.io.Tokenizer.TokenType
import java.util.regex.Pattern
import scala.compiletime.uninitialized

object CompilerUtils {
  val IDENTIFIER_PATTERN: Pattern = Pattern.compile("^[A-Za-z][A-Za-z0-9_]*$")
  val RESERVED_WORDS: java.util.Set[String] = java.util.Collections.unmodifiableSet(new java.util.LinkedHashSet(java.util.Arrays.asList(
    "class","if","else","while","return","break","true","false","null","new","this","int","bool","String","void"
  )))
  val STATEMENT_STARTERS: java.util.Set[String] = java.util.Collections.unmodifiableSet(new java.util.LinkedHashSet(java.util.Arrays.asList(
    "return","if","else","while","class","new","this","true","false","null","void"
  )))
  private var recorder: TokenRecorder = uninitialized
  def setRecorder(r: TokenRecorder): Unit = recorder = r
  def clearRecorder(): Unit = recorder = null
  def clearTokens(): Unit = if (recorder != null) recorder.clear()
  private def rec(t: String): Unit = if (recorder != null && t != null) recorder.record(t)

  def expect(f: SamTokenizer, expected: Char, line: Int): Unit = if (!f.check(expected)) throw new SyntaxErrorException(s"Expected '$expected'", line, column(f)) else rec(expected.toString)
  def expect(f: SamTokenizer, expected: String, line: Int): Unit = if (!f.check(expected)) throw new SyntaxErrorException(s"Expected '$expected'", line, column(f)) else rec(expected)
  def expectIdentifier(f: SamTokenizer, line: Int): String = {
    if (f.peekAtKind != TokenType.WORD) throw new SyntaxErrorException("Expected identifier", line, column(f))
    val w = f.getWord; rec(w); w
  }
  def expectInt(f: SamTokenizer, line: Int): Int = { if (f.peekAtKind != TokenType.INTEGER) throw new SyntaxErrorException("Expected integer", line, column(f)); val v = f.getInt; rec(v.toString); v }
  def expectString(f: SamTokenizer, line: Int): String = {
    if (f.peekAtKind != TokenType.STRING) throw new SyntaxErrorException("Expected string literal", line, column(f))
    val s = f.getString; rec("\"" + s + "\""); s
  }
  def expectWord(f: SamTokenizer, expected: String, line: Int): Unit = {
    if (f.peekAtKind != TokenType.WORD) throw new SyntaxErrorException(s"Expected '$expected'", line, column(f))
    val w = f.getWord; rec(w); if (w != expected) throw new SyntaxErrorException(s"Expected '$expected'", line, column(f))
  }
  def getIdentifier(f: SamTokenizer): String = {
    if (f.peekAtKind != TokenType.WORD) throw new SyntaxErrorException("Expected identifier", f.lineNo, column(f))
    val id = f.getWord; rec(id)
    if (RESERVED_WORDS.contains(id)) throw new SyntaxErrorException("Reserved word cannot be used as identifier: " + id, f.lineNo, column(f))
    if (!IDENTIFIER_PATTERN.matcher(id).matches()) throw new SyntaxErrorException("Invalid identifier: " + id, f.lineNo, column(f))
    id
  }
  def expectChar(f: SamTokenizer, expected: Char, line: Int): Unit = expect(f, expected, line)
  def expectOperator(f: SamTokenizer, line: Int): Char = { if (f.peekAtKind != TokenType.OPERATOR) throw new SyntaxErrorException("Expected operator", line, column(f)); val op = f.getOp; rec(op.toString); op }
  def check(f: SamTokenizer, expected: Char): Boolean = { val ok = f.check(expected); if (ok) rec(expected.toString); ok }
  def check(f: SamTokenizer, expected: String): Boolean = { val ok = f.check(expected); if (ok) rec(expected); ok }
  def getWord(f: SamTokenizer): String = { val w = f.getWord; rec(w); w }
  def getString(f: SamTokenizer): String = { val s = f.getString; rec("\"" + s + "\""); s }
  def getInt(f: SamTokenizer): Int = { val v = f.getInt; rec(v.toString); v }
  def getOp(f: SamTokenizer): Char = { val o = f.getOp; rec(o.toString); o }
  def skipToken(f: SamTokenizer): Unit = { f.skipToken(); rec(".") }
  private def currentColumn(f: SamTokenizer): Int = try { val m = f.getClass.getMethod("colNo"); m.invoke(f).asInstanceOf[Int] } catch { case _: Throwable => -1 }
  def column(f: SamTokenizer): Int = currentColumn(f)
  def isTypeWord(tz: SamTokenizer, symbols: assignment3.symbol.ProgramSymbols, currentClassName: String, allowUnknownNames: Boolean, excludeStmtStarters: Boolean): Boolean = {
    if (tz.peekAtKind != TokenType.WORD) return false
    if (excludeStmtStarters && STATEMENT_STARTERS.asInstanceOf[java.util.Set[String]].stream().anyMatch(tz.test)) return false
    if (tz.test("int") || tz.test("bool") || tz.test("String")) return true
    if (currentClassName != null && tz.test(currentClassName)) return true
    if (symbols != null) {
      val it = symbols.allClasses().iterator()
      while (it.hasNext) if (tz.test(it.next().getName)) return true
    }
    allowUnknownNames
  }
  def parseTypeOrObjectName(raw: String, line: Int): ValueType = try ValueType.ofPrimitive(Type.parse(raw, line)) catch { case _: TypeErrorException => ValueType.ofObject(raw) }
}
