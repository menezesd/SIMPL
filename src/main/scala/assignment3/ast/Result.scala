package assignment3.ast

// Canonical diagnostic-first result type for parser/semantic/codegen flows
type Result[+A] = Either[Diag, A]

object Result:
  inline def ok[A](a: A): Result[A] = Right(a)
  inline def err[A](d: Diag): Result[A] = Left(d)