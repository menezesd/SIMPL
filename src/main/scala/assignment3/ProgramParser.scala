package assignment3

import assignment3.ast.high._
import assignment3.symbol._
import edu.utexas.cs.sam.io.SamTokenizer
import edu.utexas.cs.sam.io.Tokenizer.TokenType
import scala.collection.mutable.ListBuffer
import assignment3.ast.{Diag, Result}
import assignment3.ParserSupport

/** Scala port of high-level structural parser (classes/methods) with diagnostic-first API. */
private final class ProgramParser private (
  tz: SamTokenizer,
  symbols: ProgramSymbols,
  rules: CompilerUtils.LexicalRules = CompilerUtils.LexicalRules.Default
)(using CompilerUtils.RecorderContext) extends ParserBase {
  override protected val tv: TokenizerView = new TokenizerView(tz, rules)

  private def parseProgramR(): Result[ProgramNode] =
    val classes = ListBuffer.empty[ClassNode]
    def loop(): Result[Unit] =
      if (tv.peekKind() != TokenType.EOF) then
        parseClassR().flatMap { cls => classes += cls; loop() }
      else ok(())
    loop().map(_ => ProgramNode(classes.toList))

  // Public diagnostic-first parsing
  def parseProgramD(): Either[Diag, ProgramNode] = parseProgramR()

  private def parseClassR(): Result[ClassNode] =
    for
      _ <- expectWordR("class")
      className <- getIdentifierR()
      _ <- ParserSupport.requireClass(symbols, className, tz).map(_ => ())
      _ <- if (tv.consumeChar('(')) then
        if (!tv.consumeChar(')')) then parseFieldDeclsR(className) else ok(())
      else ok(())
      _ <- expectCharR('{')
      methods <- parseMethodsR(className)
    yield ClassNode(className, methods)

  private def parseFieldDeclsR(className: String): Result[Unit] =
    parseVarDeclsR(
      className,
      Messages.invalidFieldType,
      Messages.expectedFieldListSemicolon,
      () => { tv.consumeChar(')'); ok(()) }
    )

  private def parseMethodsR(className: String): Result[List[MethodNode]] =
    collectUntil('}')(parseMethodR(className))

  private def parseMethodR(className: String): Result[MethodNode] =
    for
      returnSig <- parseReturnSigR()
      methodName <- getIdentifierR()
      ms <- ParserSupport.requireMethod(symbols, className, methodName, tz)
      _ <- expectCharR('(')
      params <- parseParamsR(ms, methodName)
      _ <- expectCharR(')')
      _ <- expectCharR('{')
      _ <- skipLocalDeclsR(className)
      body <- parseMethodBodyR(className, methodName, ms, returnSig)
    yield MethodNode(className, methodName, params, returnSig, body)

  private def parseParamsR(ms: MethodSymbol, methodName: String): Result[List[ParamNode]] =
    val expectedUserParams = ms.expectedUserArgs()
    val params = ListBuffer.empty[ParamNode]
    def loop(paramIndex: Int): Result[Unit] =
      if (tv.peekKind() == TokenType.WORD) then
        for
          pr <- getWordR()
          vt = CompilerUtils.parseTypeOrObjectName(pr, tv.line)
          (pTypeOpt, pobjOpt) = vt match {
            case PrimitiveType(t) => (Some(t), None)
            case ObjectRefType(ot) => (None, Some(ot.getClassName))
          }
          pName <- getIdentifierR()
          _ <-
            if (paramIndex >= expectedUserParams) then
              val atLeast = paramIndex + 1
              syntax(Messages.tooManyParameters(methodName, expectedUserParams, atLeast))
            else ok(())
          formal = ms.parameters(paramIndex + 1) // +1 skips implicit 'this'
          typeOk = formal.valueType match {
            case ObjectRefType(ot) => pobjOpt.contains(ot.getClassName)
            case PrimitiveType(t) => pTypeOpt.contains(t)
          }
          _ <- if (!typeOk || formal.getName != pName) then
            val position = paramIndex + 1
            val expectedType = formal.valueType match {
              case ObjectRefType(ot) => ot.getClassName
              case PrimitiveType(t) => t.toString
            }
            val actualType = pobjOpt.getOrElse(pTypeOpt.map(_.toString).getOrElse("void"))
            syntax(Messages.parameterMismatch(methodName, position, expectedType, formal.getName, actualType, pName))
          else ok(())
          _ = params += ParamNode(pName, pobjOpt, pTypeOpt)
          _ = tv.consumeChar(',')
          _ <- loop(paramIndex + 1)
        yield ()
      else ok(())
    loop(0).map(_ => params.toList)

  private def parseMethodBodyR(className: String, methodName: String, ms: MethodSymbol, returnSig: assignment3.ast.high.ReturnSig): Result[assignment3.ast.Block] =
    val stmtParser = new assignment3.ast.AstParser(tz, new assignment3.ast.NewMethodContext(ms, symbols), symbols, false, rules)
    val folder = assignment3.ast.IdiomaticConstFolder
    val stmts = ListBuffer.empty[assignment3.ast.Stmt]
    def loop(): Result[Unit] =
      if (!tv.consumeChar('}')) then
        for
          parsed <- stmtParser.parseStmtD()
          folded = folder.foldStmt(parsed)
          _ <- assignment3.ast.IdiomaticSemantic.checkStmtE(folded, ms, tv.line, symbols)
          _ = stmts += folded
          _ <- loop()
        yield ()
      else ok(())
    for
      _ <- loop()
      missingReturn = (returnSig != assignment3.ast.high.ReturnSig.Void) && (stmts.isEmpty || !endsWithReturn(stmts.last))
      _ <- if (missingReturn) then syntax(Messages.missingFinalReturn) else ok(())
    yield assignment3.ast.Block(stmts.toList)

  private def endsWithReturn(s: assignment3.ast.Stmt): Boolean = s match {
    case _: assignment3.ast.Return => true
    case b: assignment3.ast.Block =>
      val inner = b.statements
      inner.nonEmpty && endsWithReturn(inner.last)
    case _ => false
  }

  private def skipLocalDeclsR(className: String): Result[Unit] =
    parseVarDeclsR(
      className,
      Messages.invalidLocalVariableType,
      Messages.expectedVarDeclSeparator,
      () => ok(())
    )

  private def parseVarDeclsR(
    className: String,
    invalidTypeMsg: String,
    separatorErrorMsg: String,
    afterDecl: () => Result[Unit]
  ): Result[Unit] = {
    def idsLoop(): Result[Unit] =
      for
        _ <- getIdentifierR().map(_ => ())
        res <-
          if (tv.consumeChar(',')) then idsLoop()
          else if (tv.consumeChar(';')) then ok(())
          else syntax(separatorErrorMsg)
      yield res

    def loop(): Result[Unit] =
      if (tv.isTypeWord(symbols, className, false, true)) then
        for
          rawType <- getWordR()
          _ = CompilerUtils.parseTypeOrObjectName(rawType, tv.line)
          _ <- if ("void" == rawType) then syntax(invalidTypeMsg) else ok(())
          _ <- idsLoop()
          _ <- afterDecl()
          _ <- loop()
        yield ()
      else ok(())
    loop()
  }

  private def parseReturnSigR(): Result[assignment3.ast.high.ReturnSig] =
    if (tv.peekKind() == TokenType.WORD) then
      for rt <- getWordR()
      yield assignment3.ast.high.ReturnSigUtils.fromRawType(rt, tv.line)
    else syntax(Messages.expectedReturnType)

}

private object ProgramParser {
  def parseD(
    fileName: String,
    symbols: ProgramSymbols,
    rules: CompilerUtils.LexicalRules = CompilerUtils.LexicalRules.Default
  )(using CompilerUtils.RecorderContext): Either[Diag, ProgramNode] = {
    val tz = new SamTokenizer(fileName, SamTokenizer.TokenizerOptions.PROCESS_STRINGS)
    new ProgramParser(tz, symbols, rules).parseProgramD()
  }
}
