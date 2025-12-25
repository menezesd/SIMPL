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

// Type aliases for backward compatibility with existing code
type SyntaxDiag = Diag.Syntax
type TypeDiag = Diag.Type
type ResolveDiag = Diag.Resolve
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
