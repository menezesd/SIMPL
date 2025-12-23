package assignment3.symbol

import assignment3._
import assignment3.{ParserBase, TokenizerView}
import scala.compiletime.uninitialized
import assignment3.ast.{Diag, SyntaxDiag, TypeDiag, ResolveDiag, Result}
import edu.utexas.cs.sam.io.{SamTokenizer, Tokenizer}
import Tokenizer.TokenType
import scala.collection.mutable

/** Scala port of first-pass symbol table builder. */
final class SymbolTableBuilder(
  rules: CompilerUtils.LexicalRules = CompilerUtils.LexicalRules.Default
)(using CompilerUtils.RecorderContext) extends ParserBase {
  // Build into immutable structures directly
  private var classesAcc = Vector.empty[ClassSymbol]
  private var tvInst: TokenizerView = uninitialized
  override protected def tv: TokenizerView = tvInst

  // Diagnostic-first helpers
  private inline def typeE[A](msg: String, line: Int, col: Int): Result[A] = Left(TypeDiag(msg, line, col))

  private def expectCharR(ch: Char): Result[Unit] = tv.expectChar(ch)
  private def expectWordR(word: String): Result[Unit] = tv.expectWord(word)
  private def getIdentifierR(): Result[String] = tv.getIdentifier
  private def getWordR(): Result[String] = tv.getWord

  private def isTypeToken(currentClassName: String): Boolean =
    tv.isTypeWord(
      ProgramSymbols(classesAcc.map(c => c.name -> c).toMap),
      currentClassName,
      allowUnknown = true,
      excludeStmtStarters = true
    )

  private def parseNameListWithSemicolonR(addVar: (String, Int, Int) => Unit, errorMsg: String): Result[Unit] = {
    var res: Result[Boolean] = ok(true)
    def shouldContinue: Boolean = res match
      case Right(true) => true
      case _ => false
    while (shouldContinue) {
      res =
        for
          name <- getIdentifierR()
          atName = tv.at(name)
          _ = addVar(atName.value, atName.line, atName.col)
          next <-
            if (tv.consumeChar(',')) then ok(true)
            else if (tv.consumeChar(';')) then ok(false)
            else syntax(errorMsg)
        yield next
    }
    res.map(_ => ())
  }

  private def parseParamsR(className: String, addParam: (ValueType, String, Int, Int) => Unit): Result[Unit] = {
    var res: Result[Unit] = ok(())
    while (res.isRight && isTypeToken(className)) {
      res =
        for
          rawType <- getWordR()
          vt = CompilerUtils.parseTypeOrObjectName(rawType, tv.line)
          pName <- getIdentifierR()
          atName = tv.at(pName)
          _ = addParam(vt, atName.value, atName.line, atName.col)
          _ = if (tv.consumeChar(',')) () else ()
        yield ()
    }
    res
  }

  private def buildR(tokenizer: SamTokenizer): Result[ProgramSymbols] =
    tvInst = new TokenizerView(tokenizer, rules)
    def outer(): Result[Unit] =
      if (tv.peekKind() != TokenType.EOF) then
        parseClassR().flatMap(_ => outer())
      else ok(())
    for
      _ <- outer()
      program = ProgramSymbols(classesAcc.map(c => c.name -> c).toMap)
      _ <- validateTypesR(program)
    yield program

  /** Diagnostic-first variant: never throws; returns Either[Diag, ProgramSymbols]. */
  def buildD(tokenizer: SamTokenizer): Either[Diag, ProgramSymbols] = buildR(tokenizer)

  private def parseClassR(): Result[Unit] =
    for
      _ <- expectWordR("class")
      className <- getIdentifierR()
      // Reject duplicate class declarations
      _ <- {
        if (classesAcc.exists(_.name == className)) then
          err[Unit](ResolveDiag(Messages.duplicateClass(className), tv.line, tv.col))
        else ok(())
      }
      res <- {
        // accumulate fields
        var fieldSyms = Vector.empty[VarSymbol]
        var fieldOrder = Vector.empty[String]

        // Parse optional field list in parentheses
        def parseFields(): Result[Unit] =
          if (tv.consumeChar('(')) then
            if (tv.consumeChar(')')) ok(())
            else
              def fieldsLoop(): Result[Unit] =
                if (tv.peekKind() != TokenType.WORD) then
                  if (tv.consumeChar(')')) ok(()) else syntax(Messages.expectedCloseParenAfterFieldDecls)
                else
                  for
                    rawType <- getWordR()
                    vt = CompilerUtils.parseTypeOrObjectName(rawType, tv.line)
                    valueType = if (vt.isObject) ValueType.ofObject(vt.getObject.getClassName) else ValueType.ofPrimitive(vt.getPrimitive)
                    _ <- parseNameListWithSemicolonR(
                      (name, line, col) => {
                        fieldSyms :+= new VarSymbol(name, valueType, false, -1, line, col)
                        fieldOrder :+= name
                      },
                      Messages.expectedFieldDeclSemicolon
                    )
                    res <- if (tv.consumeChar(')')) ok(()) else fieldsLoop()
                  yield res
              fieldsLoop()
          else ok(())

        val headerRes =
          for
            _ <- parseFields()
            _ <- if (!tv.consumeChar('{')) then syntax(Messages.expectedClassHeaderOpenBrace(className)) else ok(())
          yield ()

        headerRes.flatMap { _ =>
          // Parse methods until closing '}'
          def methodsLoop(acc: Vector[MethodSymbol]): Result[Vector[MethodSymbol]] =
            if (tv.consumeChar('}')) then ok(acc)
            else parseMethodR(className).flatMap(m => methodsLoop(acc :+ m))

          methodsLoop(Vector.empty).map { ms =>
            val cls = ClassSymbol(className, fieldSyms, ms.map(m => m.name -> m).toMap, fieldOrder)
            classesAcc :+= cls
            ()
          }
        }
      }
    yield res

  private def parseMethodR(className: String): Result[MethodSymbol] = {
    // mutable accumulators for params/locals
    var params = Vector(new VarSymbol("this", ValueType.ofObject(className), true, 0, -1, -1))
    var locals = Vector.empty[VarSymbol]

    for
      typeWord <- getWordR()
      rtLine = tv.line; rtCol = tv.col
      returnSig = assignment3.ast.high.ReturnSigUtils.fromRawType(typeWord, tv.line)
      name <- getIdentifierR()
      methodRet = assignment3.ast.high.ReturnSigUtils.toValueTypeOpt(returnSig)
      _ <- expectCharR('(')
      _ <- if (className == "Main" && name == "main" && tv.peekKind() == TokenType.WORD)
        then syntax(Messages.mainNoParams) else ok(())
      _ <- parseParamsR(
        className,
        (vt, pName, line, col) =>
          if (vt.isObject) params :+= new VarSymbol(pName, ValueType.ofObject(vt.getObject.getClassName), true, params.size, line, col)
          else params :+= new VarSymbol(pName, ValueType.ofPrimitive(vt.getPrimitive), true, params.size, line, col)
      )
      _ <- expectCharR(')')
      _ <- expectCharR('{')
      bodyStart = tv.line
      _ <- {
        def localsOuter(): Result[Unit] =
          if (isTypeToken(className)) then
            for
              rawType <- getWordR()
              vt = CompilerUtils.parseTypeOrObjectName(rawType, tv.line)
              valueType = if (vt.isObject) ValueType.ofObject(vt.getObject.getClassName) else ValueType.ofPrimitive(vt.getPrimitive)
              _ <- parseNameListWithSemicolonR(
                (lName, line, col) =>
                  locals :+= new VarSymbol(lName, valueType, false, locals.size, line, col),
                Messages.expectedVarDeclSeparator
              )
              _ <- localsOuter()
            yield ()
          else ok(())
        localsOuter()
      }
      _ <- skipBodyRemainderR()
    yield MethodSymbol(name, params, locals, methodRet, bodyStart, rtLine, rtCol)
  }

  private def skipBodyRemainderR(): Result[Unit] = {
    val stack = new java.util.ArrayDeque[Char]()
    stack.push('{')
    while (!stack.isEmpty && tv.peekKind() != TokenType.EOF) {
      if (tv.consumeChar('{')) stack.push('{')
      else if (tv.consumeChar('}')) stack.pop()
      else CompilerUtils.skipToken(tv.tz)
    }
    if (!stack.isEmpty) Left(SyntaxDiag(Messages.unbalancedBraces, tv.line, tv.col)) else Right(())
  }

  private def validateTypesR(program: ProgramSymbols): Result[Unit] = {
    import assignment3.ast.high.ReturnSig

    // Helper functions to validate, short-circuiting on first error
    def checkField(cls: ClassSymbol, f: VarSymbol): Result[Unit] =
      if (f.isObject) then
        val cn = f.classTypeNameOpt.getOrElse(f.getClassTypeName)
        if (!program.existsClass(cn)) then err(TypeDiag(Messages.unknownFieldType(cls.getName, f.getName, cn), f.getLine, f.getColumn))
        else ok(())
      else ok(())

    def checkParam(cls: ClassSymbol, m: MethodSymbol, p: VarSymbol): Result[Unit] =
      if (p.isObject) then
        val cn = p.classTypeNameOpt.getOrElse(p.getClassTypeName)
        if (!program.existsClass(cn)) then err(TypeDiag(Messages.unknownParamType(cls.getName, m.getName, p.getName, cn), p.getLine, p.getColumn))
        else ok(())
      else ok(())

    def checkLocal(cls: ClassSymbol, m: MethodSymbol, v: VarSymbol): Result[Unit] =
      if (v.isObject) then
        val cn = v.classTypeNameOpt.getOrElse(v.getClassTypeName)
        if (!program.existsClass(cn)) then err(TypeDiag(Messages.unknownLocalType(cls.getName, m.getName, v.getName, cn), v.getLine, v.getColumn))
        else ok(())
      else ok(())

    def checkMethodReturn(cls: ClassSymbol, m: MethodSymbol): Result[Unit] = m.getReturnSig match
      case ReturnSig.Obj(cn) => if (!program.existsClass(cn)) then err(TypeDiag(Messages.unknownReturnType(cls.getName, m.getName, cn), m.getReturnTypeLine(), m.getReturnTypeColumn())) else ok(())
      case _ => ok(())

    def loopFields(it: Iterator[VarSymbol], cls: ClassSymbol): Result[Unit] =
      if (!it.hasNext) ok(()) else checkField(cls, it.next()).flatMap(_ => loopFields(it, cls))

    def loopParams(it: Iterator[VarSymbol], cls: ClassSymbol, m: MethodSymbol): Result[Unit] =
      if (!it.hasNext) ok(()) else checkParam(cls, m, it.next()).flatMap(_ => loopParams(it, cls, m))

    def loopLocals(it: Iterator[VarSymbol], cls: ClassSymbol, m: MethodSymbol): Result[Unit] =
      if (!it.hasNext) ok(()) else checkLocal(cls, m, it.next()).flatMap(_ => loopLocals(it, cls, m))

    def loopMethods(it: Iterator[MethodSymbol], cls: ClassSymbol): Result[Unit] =
      if (!it.hasNext) ok(())
      else
        val m = it.next()
        checkMethodReturn(cls, m)
          .flatMap(_ => loopParams(m.parameters.iterator, cls, m))
          .flatMap(_ => loopLocals(m.locals.iterator, cls, m))
          .flatMap(_ => loopMethods(it, cls))

    def loopClasses(it: Iterator[ClassSymbol]): Result[Unit] =
      if (!it.hasNext) ok(())
      else
        val cls = it.next()
        loopFields(cls.allFields.iterator, cls)
          .flatMap(_ => loopMethods(cls.allMethods.iterator, cls))
          .flatMap(_ => loopClasses(it))

    loopClasses(program.allClasses.iterator)
  }
}
