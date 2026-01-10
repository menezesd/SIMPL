package assignment3.symbol

import assignment3.{Messages, ObjectRefType}
import assignment3.ast.{Result, TypeDiag}
import assignment3.ast.high.ReturnSig

/** Validates type references in symbol table after construction. */
object TypeValidator:
  import Result.{ok, err}

  /** Validate all type references in the program symbols. */
  def validateTypes(program: ProgramSymbols): Result[Unit] =
    validateList(program.allClasses)(validateClass(_, program))

  /** Generic validation for object references that must exist in the program. */
  private def validateObjectRef(
    program: ProgramSymbols,
    className: String,
    line: Int,
    col: Int
  )(mkError: String => String): Result[Unit] =
    if !program.existsClass(className) then
      err(TypeDiag(mkError(className), line, col))
    else ok(())

  private def checkField(program: ProgramSymbols, cls: ClassSymbol, f: VarSymbol): Result[Unit] =
    f.valueType match
      case ObjectRefType(cn) =>
        validateObjectRef(program, cn, f.getLine, f.getColumn)(
          Messages.unknownFieldType(cls.getName, f.getName, _)
        )
      case _ => ok(())

  private def checkParam(program: ProgramSymbols, cls: ClassSymbol, m: MethodSymbol, p: VarSymbol): Result[Unit] =
    p.valueType match
      case ObjectRefType(cn) =>
        validateObjectRef(program, cn, p.getLine, p.getColumn)(
          Messages.unknownParamType(cls.getName, m.getName, p.getName, _)
        )
      case _ => ok(())

  private def checkLocal(program: ProgramSymbols, cls: ClassSymbol, m: MethodSymbol, v: VarSymbol): Result[Unit] =
    v.valueType match
      case ObjectRefType(cn) =>
        validateObjectRef(program, cn, v.getLine, v.getColumn)(
          Messages.unknownLocalType(cls.getName, m.getName, v.getName, _)
        )
      case _ => ok(())

  private def checkMethodReturn(program: ProgramSymbols, cls: ClassSymbol, m: MethodSymbol): Result[Unit] =
    m.getReturnSig match
      case ReturnSig.Obj(cn) =>
        validateObjectRef(program, cn, m.getReturnTypeLine(), m.getReturnTypeColumn())(
          Messages.unknownReturnType(cls.getName, m.getName, _)
        )
      case _ => ok(())

  private def validateList[A](items: List[A])(check: A => Result[Unit]): Result[Unit] =
    Result.sequenceE(items)(check)

  private def validateMethod(program: ProgramSymbols, cls: ClassSymbol, m: MethodSymbol): Result[Unit] =
    for
      _ <- checkMethodReturn(program, cls, m)
      _ <- validateList(m.parameters.toList)(p => checkParam(program, cls, m, p))
      _ <- validateList(m.locals.toList)(v => checkLocal(program, cls, m, v))
    yield ()

  private def validateClass(cls: ClassSymbol, program: ProgramSymbols): Result[Unit] =
    for
      _ <- validateList(cls.allFields)(f => checkField(program, cls, f))
      _ <- validateList(cls.allMethods)(m => validateMethod(program, cls, m))
    yield ()
