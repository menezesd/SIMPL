package assignment3.ast

import edu.utexas.cs.sam.io.SamTokenizer

/** Structured diagnostics for parser/semantic resolution flows (Scala 3 enum). */
enum Diag(val message: String, val line: Int, val column: Int = -1):
  case Syntax(override val message: String, override val line: Int, override val column: Int = -1)
    extends Diag(message, line, column)
  case Type(override val message: String, override val line: Int, override val column: Int = -1)
    extends Diag(message, line, column)
  case Resolve(override val message: String, override val line: Int, override val column: Int = -1)
    extends Diag(message, line, column)

  /** Create a new diagnostic with updated message. */
  def withMessage(newMsg: String): Diag = this match
    case Syntax(_, l, c) => Syntax(newMsg, l, c)
    case Type(_, l, c) => Type(newMsg, l, c)
    case Resolve(_, l, c) => Resolve(newMsg, l, c)

// Shorter aliases for common diagnostic constructors
val SyntaxDiag = Diag.Syntax
val TypeDiag = Diag.Type
val ResolveDiag = Diag.Resolve

/** Standardized factory methods for creating diagnostics. */
object Diag:
  import assignment3.TokenizerOps

  /** Create a syntax error from a tokenizer position. */
  def syntax(msg: String, tz: SamTokenizer): Syntax =
    Syntax(msg, tz.lineNo(), TokenizerOps.column(tz))

  /** Create a type error from a tokenizer position. */
  def typeErr(msg: String, tz: SamTokenizer): Type =
    Type(msg, tz.lineNo(), TokenizerOps.column(tz))

  /** Create a resolve error from a tokenizer position. */
  def resolve(msg: String, tz: SamTokenizer): Resolve =
    Resolve(msg, tz.lineNo(), TokenizerOps.column(tz))

  /** Create a syntax error from explicit line/column. */
  def syntax(msg: String, line: Int, col: Int): Syntax =
    Syntax(msg, line, col)

  /** Create a type error from explicit line/column. */
  def typeErr(msg: String, line: Int, col: Int): Type =
    Type(msg, line, col)

  /** Create a resolve error from explicit line/column. */
  def resolve(msg: String, line: Int, col: Int): Resolve =
    Resolve(msg, line, col)

  // Result-returning versions for cleaner error construction

  /** Create a syntax error Result from a tokenizer position. */
  inline def syntaxResult[A](msg: String, tz: SamTokenizer): Result[A] =
    Left(syntax(msg, tz))

  /** Create a type error Result from a tokenizer position. */
  inline def typeErrResult[A](msg: String, tz: SamTokenizer): Result[A] =
    Left(typeErr(msg, tz))

  /** Create a resolve error Result from a tokenizer position. */
  inline def resolveResult[A](msg: String, tz: SamTokenizer): Result[A] =
    Left(resolve(msg, tz))

  /** Create a syntax error Result from explicit line/column. */
  inline def syntaxResult[A](msg: String, line: Int, col: Int = -1): Result[A] =
    Left(Syntax(msg, line, col))

  /** Create a type error Result from explicit line/column. */
  inline def typeErrResult[A](msg: String, line: Int, col: Int = -1): Result[A] =
    Left(Type(msg, line, col))

  /** Create a resolve error Result from explicit line/column. */
  inline def resolveResult[A](msg: String, line: Int, col: Int = -1): Result[A] =
    Left(Resolve(msg, line, col))

  /** Context-aware diagnostic builders using ParserSupport.At. */
  def syntaxAt[A](msg: String, at: assignment3.ParserSupport.At[_]): Syntax =
    Syntax(msg, at.line, at.col)

  def typeAt[A](msg: String, at: assignment3.ParserSupport.At[_]): Type =
    Type(msg, at.line, at.col)

  def resolveAt[A](msg: String, at: assignment3.ParserSupport.At[_]): Resolve =
    Resolve(msg, at.line, at.col)

  /** Common diagnostic builders for frequently-used patterns. */
  def missingSymbol(kind: String, name: String, line: Int, col: Int = -1): Resolve =
    Resolve(s"Missing $kind symbol: $name", line, col)

  def typeMismatch(expected: String, actual: String, line: Int, col: Int = -1): Type =
    Type(s"Type mismatch: expected $expected, got $actual", line, col)

  def unexpectedToken(expected: String, actual: String, line: Int, col: Int = -1): Syntax =
    Syntax(s"Expected $expected but found $actual", line, col)
