package assignment3.ast

/**
 * Canonical diagnostic-first result type for parser/semantic/codegen flows.
 *
 * ## Naming Conventions for Error-Handling Methods
 *
 * This codebase uses consistent suffixes to indicate error-handling patterns:
 *
 * ### `*D` suffix: **Diagnostic-first public API**
 * - Returns `Either[Diag, A]` (same as `Result[A]`)
 * - Used for top-level entry points and public interfaces
 * - Examples: `compileD()`, `parseD()`, `emitD()`, `buildD()`
 * - Typical callers: External code, top-level orchestration
 *
 * ### `*R` suffix: **Result-returning internal methods**
 * - Returns `Result[A]` (which is `Either[Diag, A]`)
 * - Used primarily in parser internals for intermediate parsing steps
 * - Examples: `parseExprR()`, `parseStmtR()`, `parseClassR()`
 * - Typical callers: Internal parser logic, recursive parsing
 * - Often composed with for-comprehensions for error propagation
 *
 * ### `*E` suffix: **Either-returning validation/checks**
 * - Returns `Either[Diag, A]` (semantically same as Result but conceptually different)
 * - Used for semantic validation, type checking, and constraint enforcement
 * - Examples: `checkExprE()`, `checkStmtE()`, `validateTypeE()`
 * - Typical callers: Semantic analysis passes, validation phases
 *
 * ### `*C` suffix: **Code-returning (infallible codegen)**
 * - Returns `Code` (never fails)
 * - Used for code generation that cannot fail
 * - Examples: `emitLiteralC()`, `emitBinaryC()`
 * - Typical callers: Code generation where failure is impossible
 *
 * ### No suffix: **Legacy or infallible operations**
 * - May throw exceptions, return non-Either values, or be simple accessors
 * - Examples: `parse()` (legacy throwing API), `getName()` (simple getter)
 * - Avoid in new code; prefer diagnostic-first approach
 *
 * ## Best Practices
 *
 * 1. **Public APIs**: Always use `*D` suffix for top-level functions
 * 2. **Parser internals**: Use `*R` for parsing methods that return Results
 * 3. **Validation**: Use `*E` for semantic/type checking methods
 * 4. **Infallible codegen**: Use `*C` when code emission cannot fail
 * 5. **Error messages**: Use lazy parameters (`=> Diag`) to avoid unnecessary computation
 * 6. **Composition**: Leverage for-comprehensions and Result extension methods
 *
 * ## Example Usage
 *
 * ```scala
 * // Public API (diagnostic-first)
 * def compileD(source: String): Either[Diag, Program] =
 *   for
 *     tokens <- tokenizeR(source)
 *     ast <- parseR(tokens)
 *     checked <- checkE(ast)
 *     code <- emitD(checked)
 *   yield code
 *
 * // Internal parser method
 * private def parseExprR(): Result[Expr] =
 *   for
 *     left <- parseTermR()
 *     _ <- expectCharR('+')
 *     right <- parseTermR()
 *   yield Binary(left, right)
 *
 * // Semantic validation
 * private def checkTypeE(expr: Expr, expected: Type): Either[Diag, Unit] =
 *   if expr.tpe == expected then Right(())
 *   else Left(TypeDiag(s"Expected $expected, got ${expr.tpe}", expr.line))
 *
 * // Infallible codegen
 * private def emitLiteralC(lit: IntLit): Code =
 *   Code.from(s"PUSHIMM ${lit.value}")
 * ```
 */
type Result[+A] = Either[Diag, A]

object Result:
  inline def ok[A](a: A): Result[A] = Right(a)
  inline def err[A](d: Diag): Result[A] = Left(d)
  inline def unit: Result[Unit] = Right(())

  /** Sequence a list of items through a fallible function, short-circuiting on first error. */
  def sequenceE[A](items: Iterable[A])(f: A => Result[Unit]): Result[Unit] =
    items.foldLeft[Result[Unit]](Right(())) { (acc, item) =>
      acc.flatMap(_ => f(item))
    }

  /** Traverse a list, collecting results or short-circuiting on first error. */
  def traverseE[A, B](items: List[A])(f: A => Result[B]): Result[List[B]] =
    items.foldLeft[Result[List[B]]](Right(Nil)) { (accE, a) =>
      for { acc <- accE; b <- f(a) } yield b :: acc
    }.map(_.reverse)

  /** Fold items with an accumulator, short-circuiting on first error. */
  def foldE[A, B](items: Iterable[A], init: B)(f: (B, A) => Result[B]): Result[B] =
    items.foldLeft[Result[B]](Right(init)) { (accE, a) =>
      accE.flatMap(acc => f(acc, a))
    }

  /** Lift an Option to Result with a fallback diagnostic. */
  def fromOption[A](opt: Option[A], ifNone: => Diag): Result[A] =
    opt.toRight(ifNone)

  /** Lift a boolean condition to Result. */
  def require(cond: Boolean, ifFalse: => Diag): Result[Unit] =
    if cond then Right(()) else Left(ifFalse)

  /** Extension methods for Result values. */
  extension [A](self: Result[A])
    /** Recover from error with a default value. */
    def recover(default: => A): A = self.getOrElse(default)

    /** Convert to Option, discarding any error. */
    def toOpt: Option[A] = self.toOption

    /** Execute side effect on success. */
    def tapOk(f: A => Unit): Result[A] =
      self.foreach(f)
      self

    /** Execute side effect on error. */
    def tapErr(f: Diag => Unit): Result[A] =
      self.left.foreach(f)
      self

    /** Map the error. */
    def mapErr(f: Diag => Diag): Result[A] =
      self.left.map(f)

    /** Provide alternative on error. */
    def orElseResult(alt: => Result[A]): Result[A] =
      self.orElse(alt)

    /** Execute side effect on both success and error. */
    def tapBoth(onOk: A => Unit, onErr: Diag => Unit): Result[A] =
      self match
        case Right(a) => onOk(a); self
        case Left(d)  => onErr(d); self

  /** Validate all items against a predicate, short-circuiting on first failure. */
  def validateAll[A](items: Iterable[A])(pred: A => Boolean, mkDiag: A => Diag): Result[Unit] =
    items.find(!pred(_)) match
      case Some(a) => Left(mkDiag(a))
      case None    => Right(())

  /** Combine two results into a tuple, failing if either fails. */
  def zip[A, B](ra: Result[A], rb: Result[B]): Result[(A, B)] =
    for { a <- ra; b <- rb } yield (a, b)

  /** Combine three results into a tuple, failing if any fails. */
  def zip3[A, B, C](ra: Result[A], rb: Result[B], rc: Result[C]): Result[(A, B, C)] =
    for { a <- ra; b <- rb; c <- rc } yield (a, b, c)

  /** Lift a partial function to Result. */
  def fromPartial[A, B](a: A)(pf: PartialFunction[A, B], ifUndefined: => Diag): Result[B] =
    pf.lift(a).toRight(ifUndefined)

  /** Ensure a condition on the result value. */
  def ensure[A](result: Result[A])(pred: A => Boolean, ifFalse: => Diag): Result[A] =
    result.flatMap(a => if pred(a) then Right(a) else Left(ifFalse))

  /** Extension methods for Option to Result conversions. */
  extension [A](opt: Option[A])
    /** Convert Option to Result with a diagnostic for None. */
    inline def toResult(onNone: => Diag): Result[A] = opt.toRight(onNone)

    /** Convert Option to Result with a lazy diagnostic constructor. */
    inline def toResultWith(mkDiag: => Diag): Result[A] = opt.toRight(mkDiag)

    /** Check an Option with a validation function, succeeding if None or if check passes. */
    def checkWith[B](f: A => Result[B]): Result[Unit] =
      opt.fold(Result.unit)(f(_).map(_ => ()))

  /** Check an Option with a validation function, succeeding if None or if check passes. */
  def ifSome[A](opt: Option[A])(f: A => Result[Unit]): Result[Unit] =
    opt.fold(Result.unit)(f)

  /** Validate all items with indexed errors. */
  def validateIndexed[A](items: List[A])(pred: (A, Int) => Boolean, mkDiag: (A, Int) => Diag): Result[Unit] =
    items.zipWithIndex.find { case (a, i) => !pred(a, i) } match
      case Some((a, i)) => Left(mkDiag(a, i))
      case None => Right(())

  /** Check that exactly one of the options is defined. */
  def requireExactlyOne[A](opts: Option[A]*)(noneErr: => Diag, multipleErr: => Diag): Result[A] =
    opts.flatten.toList match
      case Nil => Left(noneErr)
      case single :: Nil => Right(single)
      case _ => Left(multipleErr)

  /** Validate a non-empty collection. */
  def requireNonEmpty[A](items: Iterable[A], ifEmpty: => Diag): Result[Unit] =
    if items.nonEmpty then Right(()) else Left(ifEmpty)

  /** Chain validations with early exit. */
  def validateChain[A](value: A)(validators: (A => Result[Unit])*): Result[A] =
    validators.foldLeft[Result[Unit]](Right(())) { (acc, validator) =>
      acc.flatMap(_ => validator(value))
    }.map(_ => value)

  /** Extension methods for Result composition. */
  extension [A](self: Result[A])
    /** Flatten nested Results. */
    def flatten[B](using ev: A <:< Result[B]): Result[B] =
      self.flatMap(identity)

    /** Apply a function that may fail to a successful result. */
    def andThen[B](f: A => Result[B]): Result[B] =
      self.flatMap(f)

    /** Execute an action for its side effects, preserving the result. */
    def tap(f: A => Unit): Result[A] =
      self.foreach(f); self

    /** Conditional flatMap. */
    def flatMapIf(cond: Boolean)(f: A => Result[A]): Result[A] =
      if cond then self.flatMap(f) else self

    /** Provide a default error message if Left. */
    def withErrorDefault(msg: => String): Result[A] =
      self.left.map(d => if d.message.isEmpty then d.withMessage(msg) else d)

  /** Extension methods for Either[String, A] to Diag conversions. */
  extension [A](either: Either[String, A])
    /** Convert Either[String, A] to Result[A] with line/column info. */
    def toResultAt(line: Int, col: Int = -1): Result[A] =
      either.left.map(msg => SyntaxDiag(msg, line, col))

    /** Convert Either[String, A] to Result[A] with a Diag constructor. */
    def toResultWith(mkDiag: String => Diag): Result[A] =
      either.left.map(mkDiag)