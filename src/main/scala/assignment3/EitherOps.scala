package assignment3

import assignment3.ast.Diag

object EitherOps {
  def lift[A](value: A): Either[Diag, A] = Right(value)
  def unit(body: => Unit): Either[Diag, Unit] = { body; Right(()) }
}
