package assignment3

import edu.utexas.cs.sam.io.SamTokenizer
import edu.utexas.cs.sam.io.Tokenizer.TokenType
import scala.util.matching.Regex

/** Classifies tokens for type detection with clear separation of concerns. */
private object TokenClassifier:
  /** Check if current token is a primitive type keyword. */
  private def isPrimitiveType(tz: SamTokenizer): Boolean =
    tz.test("int") || tz.test("bool") || tz.test("String")

  /** Check if current token matches the current class name. */
  private def isSelfReference(tz: SamTokenizer, currentClassName: String): Boolean =
    Option(currentClassName).exists(tz.test)

  /** Check if current token matches any known class name. */
  private def isKnownClass(tz: SamTokenizer, symbols: assignment3.symbol.ProgramSymbols): Boolean =
    Option(symbols).exists(_.allClasses.exists(cls => tz.test(cls.getName)))

  /** Main classification: determine if token represents a type word. */
  def isTypeWord(
    tz: SamTokenizer,
    symbols: assignment3.symbol.ProgramSymbols,
    currentClassName: String,
    config: CompilerUtils.TypeCheckConfig,
    rules: CompilerUtils.LexicalRules
  ): Boolean =
    // Early exits
    if tz.peekAtKind != TokenType.WORD then return false
    if config.excludeStmtStarters && rules.statementStarters.exists(tz.test) then return false

    // Positive classifications
    if isPrimitiveType(tz) then return true
    if isSelfReference(tz, currentClassName) then return true
    if isKnownClass(tz, symbols) then return true

    // Fallback: allow unknown names if permitted
    config.allowUnknownNames

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

  /** Configuration for type word classification behavior. */
  final case class TypeCheckConfig(
    allowUnknownNames: Boolean,
    excludeStmtStarters: Boolean
  )
  object TypeCheckConfig {
    /** Accept unknown class names, exclude statement starters (symbol table building). */
    val AllowUnknown: TypeCheckConfig = TypeCheckConfig(allowUnknownNames = true, excludeStmtStarters = true)

    /** Strict mode: only known types, exclude statement starters (parsing). */
    val Strict: TypeCheckConfig = TypeCheckConfig(allowUnknownNames = false, excludeStmtStarters = true)

    /** Permissive mode: allow unknown, don't exclude statement starters. */
    val Permissive: TypeCheckConfig = TypeCheckConfig(allowUnknownNames = true, excludeStmtStarters = false)
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

  /** Check if current token represents a type word (primitive or class name). */
  def isTypeWord(
    tz: SamTokenizer,
    symbols: assignment3.symbol.ProgramSymbols,
    currentClassName: String,
    config: TypeCheckConfig
  ): Boolean =
    TokenClassifier.isTypeWord(tz, symbols, currentClassName, config, LexicalRules.Default)

  def parseTypeOrObjectName(raw: String, line: Int): ValueType =
    Type.parseE(raw, line) match
      case Right(t) => ValueType.ofPrimitive(t)
      case Left(_)  => ValueType.ofObject(raw)
}
