package assignment3

import assignment3.ast.{Diag, ResolveDiag}
import assignment3.symbol.{MethodSymbol, ProgramSymbols}

object ValidationStage {
  def validateEntrypointD(symbols: ProgramSymbols, entryClass: String, entryMethod: String): Either[Diag, Unit] = {
    symbols.getMethod(entryClass, entryMethod) match
      case None => Left(ResolveDiag(Messages.missingEntrypoint(entryClass, entryMethod), -1))
      case Some(ms) =>
        if (ms.expectedUserArgs() != 0) Left(ResolveDiag(Messages.entrypointNoParams(entryClass, entryMethod), entrypointErrorLine(ms)))
        else ms.parameters.headOption match {
          case Some(param) =>
            param.valueType match {
              case ObjectRefType(ot) if ot.getClassName == entryClass => Right(())
              case _ => Left(ResolveDiag(Messages.entrypointMustBeInstance(entryClass, entryMethod), entrypointErrorLine(ms)))
            }
          case None => Left(ResolveDiag(Messages.entrypointMustBeInstance(entryClass, entryMethod), entrypointErrorLine(ms)))
        }
  }

  private def entrypointErrorLine(ms: MethodSymbol): Int = {
    val ln = ms.getBodyStartLine()
    if (ln > 0) ln else -1
  }
}
