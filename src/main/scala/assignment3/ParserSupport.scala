package assignment3

import assignment3.ast.{Diag, ResolveDiag, Result}
import assignment3.ast.Result.{toResult, toResultWith}  // Import extension methods
import edu.utexas.cs.sam.io.SamTokenizer

object ParserSupport:
  final case class At[A](value: A, line: Int, col: Int)

  def at[A](tz: SamTokenizer, value: A): At[A] =
    At(value, tz.lineNo(), CompilerUtils.column(tz))

  // Delegate to Diag companion object for consistency
  def syntax(msg: String, tz: SamTokenizer): Diag = Diag.syntax(msg, tz)
  def typeE(msg: String, tz: SamTokenizer): Diag = Diag.typeErr(msg, tz)
  def resolveE(msg: String, tz: SamTokenizer): Diag = Diag.resolve(msg, tz)

  def requireClass(symbols: assignment3.symbol.ProgramSymbols, className: String, tz: SamTokenizer): Result[assignment3.symbol.ClassSymbol] =
    symbols.getClass(className) match
      case Some(cs) => Right(cs)
      case None => Left(syntax(Messages.classSymbolMissing(className), tz))

  def requireMethod(
    symbols: assignment3.symbol.ProgramSymbols,
    className: String,
    methodName: String,
    tz: SamTokenizer
  ): Result[assignment3.symbol.MethodSymbol] =
    symbols.getMethod(className, methodName) match
      case Some(ms) => Right(ms)
      case None => Left(syntax(Messages.methodSymbolMissing(className, methodName), tz))

  def requireMethodResolved(
    symbols: assignment3.symbol.ProgramSymbols,
    className: String,
    methodName: String
  ): Result[assignment3.symbol.MethodSymbol] =
    symbols.getMethod(className, methodName) match
      case Some(ms) => Right(ms)
      case None => Left(ResolveDiag(Messages.methodSymbolMissing(className, methodName), -1))

  /** Symbol lookup helpers with custom diagnostic support. */
  object SymbolLookup:
    import assignment3.symbol.{VarSymbol, ClassSymbol, MethodSymbol, ProgramSymbols}
    import assignment3.ast.MethodContext

    /** Require variable lookup to succeed, with custom diagnostic on failure. */
    def requireVar(method: MethodContext, name: String, diag: => Diag): Result[VarSymbol] =
      method.lookupVar(name).toResult(diag)

    /** Require class lookup to succeed, with custom diagnostic on failure. */
    def requireClass(symbols: ProgramSymbols, name: String, diag: => Diag): Result[ClassSymbol] =
      symbols.getClass(name).toResult(diag)

    /** Require method lookup to succeed, with custom diagnostic on failure. */
    def requireMethod(symbols: ProgramSymbols, className: String, methodName: String, diag: => Diag): Result[MethodSymbol] =
      symbols.getMethod(className, methodName).toResult(diag)

    /** Require field lookup to succeed, with custom diagnostic on failure. */
    def requireField(classSymbol: ClassSymbol, fieldName: String, diag: => Diag): Result[VarSymbol] =
      classSymbol.field(fieldName).toResult(diag)

    /** Require field info lookup to succeed, with custom diagnostic on failure. */
    def requireFieldInfo(classSymbol: ClassSymbol, fieldName: String, diag: => Diag): Result[ClassSymbol.FieldInfo] =
      classSymbol.getFieldInfo(fieldName).toResult(diag)

    /** Check if a class has a constructor (method with same name as class). */
    def hasConstructor(classSymbol: ClassSymbol): Boolean =
      classSymbol.method(classSymbol.getName).isDefined

    /** Get class info from optional ProgramSymbols. */
    def getClassOpt(symbolsOpt: Option[ProgramSymbols], className: String): Option[ClassSymbol] =
      symbolsOpt.flatMap(_.getClass(className))

    /** Check if a constructor exists for the given class name in optional symbols. */
    def hasConstructorOpt(symbolsOpt: Option[ProgramSymbols], className: String): Boolean =
      getClassOpt(symbolsOpt, className).exists(hasConstructor)

  def consumeChar(tz: SamTokenizer, ch: Char)(using CompilerUtils.RecorderContext): Boolean =
    CompilerUtils.check(tz, ch)

  def consumeWord(tz: SamTokenizer, word: String)(using CompilerUtils.RecorderContext): Boolean =
    CompilerUtils.check(tz, word)
