package assignment3

import assignment3.ast.{Diag, ResolveDiag}
import assignment3.symbol.{MethodSymbol, ProgramSymbols}

object ValidationStage {
  def validateEntrypointD(symbols: ProgramSymbols, entryClass: String, entryMethod: String): Either[Diag, Unit] = {
    symbols.getMethod(entryClass, entryMethod) match
      case None => Left(ResolveDiag(Messages.missingEntrypoint(entryClass, entryMethod), -1))
      case Some(ms) =>
        if (ms.expectedUserArgs() != 0) Left(ResolveDiag(Messages.entrypointNoParams(entryClass, entryMethod), entrypointErrorLine(ms)))
        else if (ms.parameters.isEmpty || !ms.parameters.head.isObject || ms.parameters.head.getClassTypeName != entryClass)
          Left(ResolveDiag(Messages.entrypointMustBeInstance(entryClass, entryMethod), entrypointErrorLine(ms)))
        else Right(())
  }

  private def entrypointErrorLine(ms: MethodSymbol): Int = {
    val ln = ms.getBodyStartLine()
    if (ln > 0) ln else -1
  }
}
