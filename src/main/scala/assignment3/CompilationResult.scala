package assignment3

import assignment3.ast.Diag

final case class CompilationResult(code: Option[String], diag: Option[Diag]) {
  def toEither: Either[Diag, String] = diag match
    case Some(d) => Left(d)
    case None => Right(code.getOrElse(""))

  def isSuccess: Boolean = diag.isEmpty
  override def toString: String = code.getOrElse("")
}

object CompilationResult {
  def success(code: String): CompilationResult = CompilationResult(Some(code), None)
  def failure(diag: Diag): CompilationResult = CompilationResult(None, Some(diag))
}
