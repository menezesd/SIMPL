package assignment3

import assignment3.ast.{Diag, Result}

trait ParserBase {
  protected def tv: TokenizerView

  // Result helpers
  protected inline def ok[A](a: A): Result[A] = Result.ok(a)
  protected inline def err[A](d: Diag): Result[A] = Result.err(d)
  protected inline def syntax[A](msg: String): Result[A] = Left(ParserSupport.syntax(msg, tv.tz))
  protected inline def typeE[A](msg: String): Result[A] = Left(ParserSupport.typeE(msg, tv.tz))

  // Common tokenizer operations - delegating to TokenizerView
  protected def expectCharR(ch: Char): Result[Unit] = tv.expectChar(ch)
  protected def expectWordR(word: String): Result[Unit] = tv.expectWord(word)
  protected def getIdentifierR(): Result[String] = tv.getIdentifier
  protected def getWordR(): Result[String] = tv.getWord
  protected def getIntR(): Result[Int] = tv.getInt
  protected def getStringR(): Result[String] = tv.getString
  protected def getOpR(): Result[Char] = tv.getOp

  // Common loop patterns

  /** Parse a comma-separated list enclosed by closingChar (already consumed opening char).
   *  Returns empty list if closingChar is immediately consumed.
   */
  protected def parseCommaSeparatedList[A](closingChar: Char)(parseItem: => Result[A]): Result[List[A]] =
    if tv.consumeChar(closingChar) then ok(Nil)
    else
      parseItem.flatMap { first =>
        def loop(acc: List[A]): Result[List[A]] =
          if tv.consumeChar(',') then
            parseItem.flatMap(e => loop(acc :+ e))
          else expectCharR(closingChar).map(_ => acc)
        loop(List(first))
      }

  /** Collect items until terminator character is consumed.
   *  Useful for parsing method bodies, class bodies, etc.
   */
  protected def collectUntil[A](terminator: Char)(parseItem: => Result[A]): Result[List[A]] = {
    val buf = scala.collection.mutable.ListBuffer.empty[A]
    def loop(): Result[List[A]] =
      if tv.consumeChar(terminator) then ok(buf.toList)
      else parseItem.flatMap { item => buf += item; loop() }
    loop()
  }
}
