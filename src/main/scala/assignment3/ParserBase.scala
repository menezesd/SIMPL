package assignment3

import assignment3.ast.{Diag, Result}

trait ParserBase {
  protected def tv: TokenizerView

  protected inline def ok[A](a: A): Result[A] = Result.ok(a)
  protected inline def err[A](d: Diag): Result[A] = Result.err(d)
  protected inline def syntax[A](msg: String): Result[A] = Left(ParserSupport.syntax(msg, tv.tz))
  protected inline def typeE[A](msg: String): Result[A] = Left(ParserSupport.typeE(msg, tv.tz))
}
