package assignment3

import assignment3.ast.high._
import assignment3.symbol._
import edu.utexas.cs.sam.io.SamTokenizer
import edu.utexas.cs.sam.io.Tokenizer.TokenType
import scala.collection.mutable.ListBuffer
import assignment3.ast.{Diag, SyntaxDiag, TypeDiag, ResolveDiag, Result}

/** Scala port of high-level structural parser (classes/methods) with diagnostic-first API. */
private final class ProgramParser private (tz: SamTokenizer, symbols: ProgramSymbols) {
  // Result helpers
  private inline def ok[A](a: A): Result[A] = Right(a)
  private inline def err[A](d: Diag): Result[A] = Left(d)
  private inline def syntax[A](msg: String): Result[A] = Left(SyntaxDiag(msg, tz.lineNo(), CompilerUtils.column(tz)))

  // Safe wrappers around throwing CompilerUtils for gradual migration
  private def expectCharR(ch: Char): Result[Unit] =
    try { CompilerUtils.expectChar(tz, ch, tz.lineNo()); ok(()) }
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def expectWordR(word: String): Result[Unit] =
    try { CompilerUtils.expectWord(tz, word, tz.lineNo()); ok(()) }
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def getIdentifierR(): Result[String] =
    try ok(CompilerUtils.getIdentifier(tz))
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def getWordR(): Result[String] =
    try ok(CompilerUtils.getWord(tz))
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def parseProgramR(): Result[ProgramNode] =
    val classes = ListBuffer.empty[ClassNode]
    def loop(): Result[Unit] =
      if (tz.peekAtKind() != TokenType.EOF) then
        parseClassR().flatMap { cls => classes += cls; loop() }
      else ok(())
    loop().map(_ => ProgramNode(classes.toList))

  // Public diagnostic-first parsing
  def parseProgramD(): Either[Diag, ProgramNode] = parseProgramR()

  private def parseClassR(): Result[ClassNode] =
    for
      _ <- expectWordR("class")
      className <- getIdentifierR()
      csOpt = symbols.getClass(className)
      _ <- csOpt match
        case Some(_) => ok(())
        case None    => syntax(s"Class symbol missing for '$className'")
      _ <- if CompilerUtils.check(tz, '(') then
        if !CompilerUtils.check(tz, ')') then parseFieldDeclsR(className) else ok(())
      else ok(())
      _ <- expectCharR('{')
      methods <- parseMethodsR(className)
    yield ClassNode(className, methods)

  private def parseFieldDeclsR(className: String): Result[Unit] =
    def fieldsLoop(): Result[Unit] =
      if CompilerUtils.isTypeWord(tz, symbols, className, false, true) then
        for
          rawType <- getWordR()
          _ = CompilerUtils.parseTypeOrObjectName(rawType, tz.lineNo())
          _ <- if ("void" == rawType) then syntax("'void' is not a valid field type") else ok(())
          _ <- getIdentifierR().map(_ => ())
          _ <-
            def idsLoop(): Result[Unit] =
              if CompilerUtils.check(tz, ',') then getIdentifierR().flatMap(_ => idsLoop()) else ok(())
            idsLoop()
          _ <- if CompilerUtils.check(tz, ';') then ok(()) else syntax("Expected ';' in field list")
          _ = if (CompilerUtils.check(tz, ')')) () else ()
          _ <- fieldsLoop()
        yield ()
      else ok(())
    fieldsLoop()

  private def parseMethodsR(className: String): Result[List[MethodNode]] =
    val methods = ListBuffer.empty[MethodNode]
    def loop(): Result[Unit] =
      if !CompilerUtils.check(tz, '}') then
        parseMethodR(className).flatMap(m => { methods += m; loop() })
      else ok(())
    loop().map(_ => methods.toList)

  private def parseMethodR(className: String): Result[MethodNode] =
    val returnSigR: Result[assignment3.ast.high.ReturnSig] =
      if (tz.test("void")) then expectWordR("void").map(_ => assignment3.ast.high.ReturnSig.Void)
      else if (tz.peekAtKind() == TokenType.WORD) then
        for
          rt <- getWordR()
          vt = CompilerUtils.parseTypeOrObjectName(rt, tz.lineNo())
        yield if (vt.isObject) assignment3.ast.high.ReturnSig.Obj(vt.getObject.getClassName)
              else if (vt.isPrimitive) assignment3.ast.high.ReturnSig.Prim(vt.getPrimitive)
              else assignment3.ast.high.ReturnSig.Void
      else syntax("Expected return type")

    for
      returnSig <- returnSigR
      methodName <- getIdentifierR()
      msOpt = symbols.getMethod(className, methodName)
      ms <- msOpt match
        case Some(m) => ok(m)
        case None    => syntax(s"Method symbol missing for '$className.$methodName'")
      _ <- expectCharR('(')
      params <- parseParamsR(ms, methodName)
      _ <- expectCharR(')')
      _ <- expectCharR('{')
      _ <- skipLocalDeclsR(className)
      body <- parseMethodBodyR(className, methodName, ms, returnSig)
    yield MethodNode(className, methodName, params, returnSig, body)

  private def parseParamsR(ms: MethodSymbol, methodName: String): Result[List[ParamNode]] =
    val expectedUserParams = assignment3.ast.MethodUtils.expectedUserArgs(ms)
    val params = ListBuffer.empty[ParamNode]
    var paramIndex = 0
    def loop(): Result[Unit] =
      if (tz.peekAtKind() == TokenType.WORD) then
        for
          pr <- getWordR()
          vt = CompilerUtils.parseTypeOrObjectName(pr, tz.lineNo())
          pTypeOpt = if (vt.isPrimitive) Some(vt.getPrimitive) else None
          pobjOpt = if (vt.isObject) Some(vt.getObject.getClassName) else None
          pName <- getIdentifierR()
          _ <-
            if (paramIndex >= expectedUserParams) then
              val atLeast = paramIndex + 1
              syntax(s"Too many parameters in '$methodName': expected $expectedUserParams, got at least $atLeast")
            else ok(())
          formal = assignment3.ast.MethodUtils.userParamAt(ms, paramIndex)
          typeOk = if (formal.isObject) pobjOpt.contains(formal.getClassTypeName) else pTypeOpt.contains(formal.getType)
          _ <- if (!typeOk || formal.getName != pName) then
            val position = paramIndex + 1
            val expectedType = if (formal.isObject) formal.getClassTypeName else String.valueOf(formal.getType)
            val actualType = pobjOpt.getOrElse(pTypeOpt.map(_.toString).getOrElse("void"))
            syntax(s"Parameter mismatch in '$methodName' at position $position: expected $expectedType ${formal.getName}, but found $actualType $pName")
          else ok(())
          _ = params += ParamNode(pName, pobjOpt, pTypeOpt)
          _ = paramIndex += 1
          _ = if (!CompilerUtils.check(tz, ',')) () else ()
          _ <- loop()
        yield ()
      else ok(())
    loop().map(_ => params.toList)

  private def parseMethodBodyR(className: String, methodName: String, ms: MethodSymbol, returnSig: assignment3.ast.high.ReturnSig): Result[assignment3.ast.Block] =
    val stmtParser = new assignment3.ast.AstParser(tz, new assignment3.ast.NewMethodContext(ms, symbols), symbols, false)
    val folder = assignment3.ast.IdiomaticConstFolder
    val stmts = ListBuffer.empty[assignment3.ast.Stmt]
    def loop(): Result[Unit] =
      if !CompilerUtils.check(tz, '}') then
        for
          parsed <- stmtParser.parseStmtD()
          folded = folder.foldStmt(parsed)
          _ <- assignment3.ast.IdiomaticSemantic.checkStmtE(folded, ms, tz.lineNo(), symbols)
          _ = stmts += folded
          _ <- loop()
        yield ()
      else ok(())
    for
      _ <- loop()
      missingReturn = (returnSig != assignment3.ast.high.ReturnSig.Void) && (stmts.isEmpty || !endsWithReturn(stmts.last))
      _ <- if (missingReturn) then syntax("Method missing final return") else ok(())
    yield assignment3.ast.Block(stmts.toList)

  private def endsWithReturn(s: assignment3.ast.Stmt): Boolean = s match {
    case _: assignment3.ast.Return => true
    case b: assignment3.ast.Block =>
      val inner = b.statements
      inner.nonEmpty && endsWithReturn(inner.last)
    case _ => false
  }

  private def skipLocalDeclsR(className: String): Result[Unit] = {
    def outer(): Result[Unit] =
      if (CompilerUtils.isTypeWord(tz, symbols, className, false, true)) then
        for
          rawType <- getWordR()
          _ = CompilerUtils.parseTypeOrObjectName(rawType, tz.lineNo())
          _ <- if ("void" == rawType) then syntax("'void' is not a valid local variable type") else ok(())
          _ <-
            def inner(): Result[Unit] =
              if (true) then
                for
                  _ <- getIdentifierR().map(_ => ())
                  res <-
                    if (CompilerUtils.check(tz, ',')) then inner()
                    else if (CompilerUtils.check(tz, ';')) then ok(())
                    else syntax("Expected ',' or ';' in variable declaration")
                yield res
              else ok(())
            inner()
          _ <- outer()
        yield ()
      else ok(())
    outer()
  }
}

private object ProgramParser {
  def parseD(fileName: String, symbols: ProgramSymbols): Either[Diag, ProgramNode] = {
    val tz = new SamTokenizer(fileName, SamTokenizer.TokenizerOptions.PROCESS_STRINGS)
    new ProgramParser(tz, symbols).parseProgramD()
  }
}
