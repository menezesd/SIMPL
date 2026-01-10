package assignment3.symbol

import assignment3.{CompilerUtils, LiveOak3Compiler, Messages, Offsets, ParserBase, TokenizerView, ValueType}
import assignment3.ast.{Diag, SyntaxDiag, ResolveDiag, Result}
import edu.utexas.cs.sam.io.{SamTokenizer, Tokenizer}
import Tokenizer.TokenType

/** Immutable first-pass symbol table builder. */
object SymbolTableBuilder {
  /** Constants for symbol table construction. */
  private object SymbolConstants {
    val ThisParameterIndex = 0
    val ThisParameterName = "this"
  }

  def buildD(
    tokenizer: SamTokenizer,
    rules: CompilerUtils.LexicalRules = CompilerUtils.LexicalRules.Default
  )(using CompilerUtils.RecorderContext): Either[Diag, ProgramSymbols] = {
    val parser = new Parser(new TokenizerView(tokenizer, rules), rules)
    parser.buildProgram()
  }

  private class Parser(
    override protected val tv: TokenizerView,
    rules: CompilerUtils.LexicalRules
  )(using CompilerUtils.RecorderContext) extends ParserBase {

    def buildProgram(): Result[ProgramSymbols] =
      parseClasses(Vector.empty).flatMap { classes =>
        val program = ProgramSymbols(classes.map(c => c.name -> c).toMap)
        TypeValidator.validateTypes(program).map(_ => program)
      }

    private def isTypeToken(knownClasses: Vector[ClassSymbol], currentClassName: String): Boolean =
      tv.isTypeWord(
        ProgramSymbols(knownClasses.map(c => c.name -> c).toMap),
        currentClassName,
        CompilerUtils.TypeCheckConfig.AllowUnknown
      )

    private def parseClasses(acc: Vector[ClassSymbol]): Result[Vector[ClassSymbol]] =
      if (tv.peekKind() == TokenType.EOF) ok(acc)
      else parseClass(acc).flatMap(cls => parseClasses(acc :+ cls))

    private def parseClass(knownClasses: Vector[ClassSymbol]): Result[ClassSymbol] =
      for
        _ <- expectWordR("class")
        className <- getIdentifierR()
        _ <- if (knownClasses.exists(_.name == className))
          err[Unit](ResolveDiag(Messages.duplicateClass(className), tv.line, tv.col))
        else ok(())
        fields <- parseFields(className)
        _ <- if (!tv.consumeChar('{')) syntax(Messages.expectedClassHeaderOpenBrace(className)) else ok(())
        methods <- parseMethods(knownClasses, className, Vector.empty)
      yield ClassSymbol(className, fields._1, methods.map(m => m.name -> m).toMap, fields._2)

    private def parseFields(className: String): Result[(Vector[VarSymbol], Vector[String])] =
      if (!tv.consumeChar('(')) ok((Vector.empty, Vector.empty))
      else if (tv.consumeChar(')')) ok((Vector.empty, Vector.empty))
      else parseFieldGroups(Vector.empty, Vector.empty)

    private def parseFieldGroups(fields: Vector[VarSymbol], order: Vector[String]): Result[(Vector[VarSymbol], Vector[String])] =
      if (tv.peekKind() != TokenType.WORD) {
        if (tv.consumeChar(')')) ok((fields, order))
        else syntax(Messages.expectedCloseParenAfterFieldDecls)
      } else {
        for
          rawType <- getWordR()
          vt = CompilerUtils.parseTypeOrObjectName(rawType, tv.line)
          namesResult <- parseNameList(vt, isParam = false, startIndex = -1)
          (newFields, newOrder) = namesResult.foldLeft((fields, order)) { case ((fs, os), sym) =>
            (fs :+ sym, os :+ sym.name)
          }
          result <- if (tv.consumeChar(')')) ok((newFields, newOrder)) else parseFieldGroups(newFields, newOrder)
        yield result
      }

    private def parseNameList(vt: ValueType, isParam: Boolean, startIndex: Int): Result[Vector[VarSymbol]] = {
      def loop(acc: Vector[VarSymbol], idx: Int): Result[Vector[VarSymbol]] =
        for
          name <- getIdentifierR()
          atName = tv.at(name)
          sym = VarSymbol(atName.value, vt, isParam, idx, atName.line, atName.col)
          result <- {
            if (tv.consumeChar(',')) loop(acc :+ sym, idx + 1)
            else if (tv.consumeChar(';')) ok(acc :+ sym)
            else syntax(Messages.expectedFieldDeclSemicolon)
          }
        yield result
      loop(Vector.empty, startIndex)
    }

    private def parseMethods(knownClasses: Vector[ClassSymbol], className: String, acc: Vector[MethodSymbol]): Result[Vector[MethodSymbol]] =
      if (tv.consumeChar('}')) ok(acc)
      else parseMethod(knownClasses, className).flatMap(m => parseMethods(knownClasses, className, acc :+ m))

    private def parseMethod(knownClasses: Vector[ClassSymbol], className: String): Result[MethodSymbol] =
      for
        typeWord <- getWordR()
        rtLine = tv.line
        rtCol = tv.col
        returnSig = assignment3.ast.high.ReturnSigUtils.fromRawType(typeWord, tv.line)
        name <- getIdentifierR()
        methodRet = assignment3.ast.high.ReturnSigUtils.toValueTypeOpt(returnSig)
        _ <- expectCharR('(')
        _ <- if (className == LiveOak3Compiler.ENTRY_CLASS && name == LiveOak3Compiler.ENTRY_METHOD && tv.peekKind() == TokenType.WORD)
          syntax(Messages.mainNoParams) else ok(())
        params <- parseParams(knownClasses, className, Vector(VarSymbol(
          SymbolConstants.ThisParameterName,
          ValueType.ofObject(className),
          true,
          SymbolConstants.ThisParameterIndex,
          Offsets.SourceLocation.UnknownLine,
          Offsets.SourceLocation.UnknownColumn
        )))
        _ <- expectCharR(')')
        _ <- expectCharR('{')
        bodyStart = tv.line
        locals <- parseLocals(knownClasses, className, Vector.empty)
        _ <- skipBodyRemainder()
      yield MethodSymbol(name, params, locals, methodRet, bodyStart, rtLine, rtCol)

    private def parseParams(knownClasses: Vector[ClassSymbol], className: String, acc: Vector[VarSymbol]): Result[Vector[VarSymbol]] =
      if (!isTypeToken(knownClasses, className)) ok(acc)
      else
        for
          rawType <- getWordR()
          vt = CompilerUtils.parseTypeOrObjectName(rawType, tv.line)
          pName <- getIdentifierR()
          atName = tv.at(pName)
          sym = VarSymbol(atName.value, vt, true, acc.size, atName.line, atName.col)
          _ = tv.consumeChar(',')
          result <- parseParams(knownClasses, className, acc :+ sym)
        yield result

    private def parseLocals(knownClasses: Vector[ClassSymbol], className: String, acc: Vector[VarSymbol]): Result[Vector[VarSymbol]] =
      if (!isTypeToken(knownClasses, className)) ok(acc)
      else
        for
          rawType <- getWordR()
          vt = CompilerUtils.parseTypeOrObjectName(rawType, tv.line)
          newLocals <- parseLocalNames(vt, acc.size, Vector.empty)
          result <- parseLocals(knownClasses, className, acc ++ newLocals)
        yield result

    private def parseLocalNames(vt: ValueType, startIdx: Int, acc: Vector[VarSymbol]): Result[Vector[VarSymbol]] =
      for
        name <- getIdentifierR()
        atName = tv.at(name)
        sym = VarSymbol(atName.value, vt, false, startIdx + acc.size, atName.line, atName.col)
        result <- {
          if (tv.consumeChar(',')) parseLocalNames(vt, startIdx, acc :+ sym)
          else if (tv.consumeChar(';')) ok(acc :+ sym)
          else syntax(Messages.expectedVarDeclSeparator)
        }
      yield result

    private def skipBodyRemainder(): Result[Unit] =
      skipBalancedBraces(Messages.unbalancedBraces)
  }
}

/** Legacy class-based API for backwards compatibility. */
final class SymbolTableBuilder(
  rules: CompilerUtils.LexicalRules = CompilerUtils.LexicalRules.Default
)(using CompilerUtils.RecorderContext) {
  def buildD(tokenizer: SamTokenizer): Either[Diag, ProgramSymbols] =
    SymbolTableBuilder.buildD(tokenizer, rules)
}
