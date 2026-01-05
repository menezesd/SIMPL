package assignment3

import assignment3.ast.{Diag, Result}

/**
 * Base trait for parsers providing common utilities and loop patterns.
 *
 * == List Building Pattern ==
 * All list-collecting methods use the efficient "prepend + reverse" pattern:
 * {{{
 *   def loop(acc: List[A]): Result[List[A]] =
 *     if done then ok(acc.reverse)
 *     else parseItem.flatMap(item => loop(item :: acc))
 *   loop(Nil)
 * }}}
 *
 * This pattern is O(n) total: each prepend is O(1), and the final reverse is O(n).
 * Using append (`acc :+ item`) would be O(n²) since each append copies the list.
 *
 * The methods in this trait abstract over this pattern for common use cases:
 *   - `parseCommaSeparatedList`: items separated by commas, enclosed by delimiter
 *   - `collectUntil`: items until terminator character
 *   - `parseSemicolonSeparated`: items while condition holds
 *   - `parseWhile`: items while condition is true
 */
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

  // TokenType check helpers (inline for zero-cost abstraction)
  protected inline def isWordAhead: Boolean = tv.peekKind() == edu.utexas.cs.sam.io.Tokenizer.TokenType.WORD
  protected inline def isIntAhead: Boolean = tv.peekKind() == edu.utexas.cs.sam.io.Tokenizer.TokenType.INTEGER
  protected inline def isStringAhead: Boolean = tv.peekKind() == edu.utexas.cs.sam.io.Tokenizer.TokenType.STRING
  protected inline def isOpAhead: Boolean = tv.peekKind() == edu.utexas.cs.sam.io.Tokenizer.TokenType.OPERATOR

  // Common loop patterns (all use prepend + reverse for O(n) performance)

  /** Parse comma-separated list enclosed by closingChar (opening char already consumed).
   *  Returns empty list if closingChar is immediately consumed.
   */
  protected def parseCommaSeparatedList[A](closingChar: Char)(parseItem: => Result[A]): Result[List[A]] =
    if tv.consumeChar(closingChar) then ok(Nil)
    else
      parseItem.flatMap { first =>
        def loop(acc: List[A]): Result[List[A]] =
          if tv.consumeChar(',') then
            parseItem.flatMap(e => loop(e :: acc))
          else expectCharR(closingChar).map(_ => acc.reverse)
        loop(List(first))
      }

  /** Collect items until terminator character is consumed. */
  protected def collectUntil[A](terminator: Char)(parseItem: => Result[A]): Result[List[A]] =
    def loop(acc: List[A]): Result[List[A]] =
      if tv.consumeChar(terminator) then ok(acc.reverse)
      else parseItem.flatMap(item => loop(item :: acc))
    loop(Nil)

  /** Parse items while predicate returns true. */
  protected def parseSemicolonSeparated[A](shouldContinue: => Boolean)(parseItem: => Result[A]): Result[List[A]] =
    def loop(acc: List[A]): Result[List[A]] =
      if !shouldContinue then ok(acc.reverse)
      else parseItem.flatMap(item => loop(item :: acc))
    loop(Nil)

  /** Parse optional element if condition is met. */
  protected def parseOptional[A](cond: => Boolean)(parse: => Result[A]): Result[Option[A]] =
    if cond then parse.map(Some(_)) else ok(None)

  /** Repeatedly parse while condition is true, accumulating results. */
  protected def parseWhile[A](cond: => Boolean)(parse: => Result[A]): Result[List[A]] =
    def loop(acc: List[A]): Result[List[A]] =
      if !cond then ok(acc.reverse)
      else parse.flatMap(item => loop(item :: acc))
    loop(Nil)

  /** Parse a list with separator or terminator.
   *  Each item is followed by either the separator (continue) or terminator (stop).
   *  Returns Unit after consuming terminator.
   */
  protected def parseSeparatedOrTerminated[A](separator: Char, terminator: Char, errorMsg: String)
    (parseItem: => Result[A]): Result[Unit] =
    def loop(): Result[Unit] =
      for
        _ <- parseItem
        res <-
          if tv.consumeChar(separator) then loop()
          else if tv.consumeChar(terminator) then ok(())
          else syntax(errorMsg)
      yield res
    loop()

  /** Skip balanced braces, consuming tokens until depth returns to 0.
   *  Starts at depth 1 (assumes opening brace already consumed).
   *  Useful for skipping method bodies in first-pass parsing.
   */
  protected def skipBalancedBraces(errorMsg: String = "Unbalanced braces"): Result[Unit] =
    import edu.utexas.cs.sam.io.Tokenizer.TokenType
    @scala.annotation.tailrec
    def loop(depth: Int): Result[Unit] =
      if depth == 0 then ok(())
      else if tv.peekKind() == TokenType.EOF then syntax(errorMsg)
      else if tv.consumeChar('{') then loop(depth + 1)
      else if tv.consumeChar('}') then loop(depth - 1)
      else { CompilerUtils.skipToken(tv.tz); loop(depth) }
    loop(1)
}
