package assignment3

import assignment3.ast.{Diag, ResolveDiag}
import assignment3.symbol.{MethodSymbol, ProgramSymbols}

object ValidationStage {
  import assignment3.ast.Result
  import assignment3.ast.Result.toResult

  def validateEntrypointD(symbols: ProgramSymbols, entryClass: String, entryMethod: String): Either[Diag, Unit] = {
    for
      ms <- symbols.getMethod(entryClass, entryMethod)
              .toResult(ResolveDiag(Messages.missingEntrypoint(entryClass, entryMethod), -1))
      _ <- validateSignature(ms, entryClass, entryMethod)
    yield ()
  }

  private def validateSignature(ms: MethodSymbol, entryClass: String, entryMethod: String): Either[Diag, Unit] =
    for
      _ <- Result.require(ms.expectedUserArgs() == 0,
             ResolveDiag(Messages.entrypointNoParams(entryClass, entryMethod), entrypointErrorLine(ms)))
      param <- ms.parameters.headOption
                 .toResult(ResolveDiag(Messages.entrypointMustBeInstance(entryClass, entryMethod), entrypointErrorLine(ms)))
      _ <- validateParamType(param, entryClass, entryMethod, ms)
    yield ()

  private def validateParamType(param: assignment3.symbol.VarSymbol, entryClass: String, entryMethod: String, ms: MethodSymbol): Either[Diag, Unit] =
    param.valueType match
      case ObjectRefType(cn) if cn == entryClass => Right(())
      case _ => Left(ResolveDiag(Messages.entrypointMustBeInstance(entryClass, entryMethod), entrypointErrorLine(ms)))

  private def entrypointErrorLine(ms: MethodSymbol): Int = {
    val ln = ms.getBodyStartLine()
    if (ln > 0) ln else -1
  }
}
