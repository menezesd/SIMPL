package assignment3.symbol

import assignment3._
import assignment3.{ParserBase, TokenizerView}
import assignment3.ast.{Diag, SyntaxDiag, TypeDiag, ResolveDiag, Result}
import edu.utexas.cs.sam.io.{SamTokenizer, Tokenizer}
import Tokenizer.TokenType

/** Immutable first-pass symbol table builder. */
object SymbolTableBuilder {
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

    private inline def typeE[A](msg: String, line: Int, col: Int): Result[A] =
      Left(TypeDiag(msg, line, col))

    def buildProgram(): Result[ProgramSymbols] =
      parseClasses(Vector.empty).flatMap { classes =>
        val program = ProgramSymbols(classes.map(c => c.name -> c).toMap)
        validateTypes(program).map(_ => program)
      }

    private def isTypeToken(knownClasses: Vector[ClassSymbol], currentClassName: String): Boolean =
      tv.isTypeWord(
        ProgramSymbols(knownClasses.map(c => c.name -> c).toMap),
        currentClassName,
        allowUnknown = true,
        excludeStmtStarters = true
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
        _ <- if (className == "Main" && name == "main" && tv.peekKind() == TokenType.WORD)
          syntax(Messages.mainNoParams) else ok(())
        params <- parseParams(knownClasses, className, Vector(VarSymbol("this", ValueType.ofObject(className), true, 0, -1, -1)))
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

    private def skipBodyRemainder(): Result[Unit] = {
      @scala.annotation.tailrec
      def loop(depth: Int): Result[Unit] =
        if (depth == 0) ok(())
        else if (tv.peekKind() == TokenType.EOF) Left(SyntaxDiag(Messages.unbalancedBraces, tv.line, tv.col))
        else if (tv.consumeChar('{')) loop(depth + 1)
        else if (tv.consumeChar('}')) loop(depth - 1)
        else { CompilerUtils.skipToken(tv.tz); loop(depth) }
      loop(1)
    }

    private def validateTypes(program: ProgramSymbols): Result[Unit] = {
      import assignment3.ast.high.ReturnSig

      def checkField(cls: ClassSymbol, f: VarSymbol): Result[Unit] =
        f.valueType match {
          case ObjectRefType(ot) if !program.existsClass(ot.getClassName) =>
            err(TypeDiag(Messages.unknownFieldType(cls.getName, f.getName, ot.getClassName), f.getLine, f.getColumn))
          case _ => ok(())
        }

      def checkParam(cls: ClassSymbol, m: MethodSymbol, p: VarSymbol): Result[Unit] =
        p.valueType match {
          case ObjectRefType(ot) if !program.existsClass(ot.getClassName) =>
            err(TypeDiag(Messages.unknownParamType(cls.getName, m.getName, p.getName, ot.getClassName), p.getLine, p.getColumn))
          case _ => ok(())
        }

      def checkLocal(cls: ClassSymbol, m: MethodSymbol, v: VarSymbol): Result[Unit] =
        v.valueType match {
          case ObjectRefType(ot) if !program.existsClass(ot.getClassName) =>
            err(TypeDiag(Messages.unknownLocalType(cls.getName, m.getName, v.getName, ot.getClassName), v.getLine, v.getColumn))
          case _ => ok(())
        }

      def checkMethodReturn(cls: ClassSymbol, m: MethodSymbol): Result[Unit] = m.getReturnSig match {
        case ReturnSig.Obj(cn) if !program.existsClass(cn) =>
          err(TypeDiag(Messages.unknownReturnType(cls.getName, m.getName, cn), m.getReturnTypeLine(), m.getReturnTypeColumn()))
        case _ => ok(())
      }

      def validateList[A](items: List[A])(check: A => Result[Unit]): Result[Unit] =
        Result.sequenceE(items)(check)

      def validateMethod(cls: ClassSymbol, m: MethodSymbol): Result[Unit] =
        for
          _ <- checkMethodReturn(cls, m)
          _ <- validateList(m.parameters.toList)(p => checkParam(cls, m, p))
          _ <- validateList(m.locals.toList)(v => checkLocal(cls, m, v))
        yield ()

      def validateClass(cls: ClassSymbol): Result[Unit] =
        for
          _ <- validateList(cls.allFields)(f => checkField(cls, f))
          _ <- validateList(cls.allMethods)(m => validateMethod(cls, m))
        yield ()

      validateList(program.allClasses)(validateClass)
    }
  }
}

/** Legacy class-based API for backwards compatibility. */
final class SymbolTableBuilder(
  rules: CompilerUtils.LexicalRules = CompilerUtils.LexicalRules.Default
)(using CompilerUtils.RecorderContext) {
  def buildD(tokenizer: SamTokenizer): Either[Diag, ProgramSymbols] =
    SymbolTableBuilder.buildD(tokenizer, rules)
}
