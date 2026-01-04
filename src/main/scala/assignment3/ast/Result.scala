package assignment3.ast

/**
 * Canonical diagnostic-first result type for parser/semantic/codegen flows.
 *
 * Naming conventions for Result/Either-returning functions:
 * - `*D` suffix: Diagnostic-first public APIs (e.g., compileD, parseD, emitD)
 * - `*R` suffix: Result-returning internal parser methods (e.g., parseExprR, parseStmtR)
 * - `*E` suffix: Either-returning semantic/validation checks (e.g., checkExprE, checkStmtE)
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
      for { acc <- accE; b <- f(a) } yield acc :+ b
    }

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

  /** Extension methods for Either[String, A] to Diag conversions. */
  extension [A](either: Either[String, A])
    /** Convert Either[String, A] to Result[A] with line/column info. */
    def toResultAt(line: Int, col: Int = -1): Result[A] =
      either.left.map(msg => SyntaxDiag(msg, line, col))

    /** Convert Either[String, A] to Result[A] with a Diag constructor. */
    def toResultWith(mkDiag: String => Diag): Result[A] =
      either.left.map(mkDiag)