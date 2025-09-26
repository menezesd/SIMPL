package assignment3

import assignment3.ast.high._
import assignment3.symbol._
import edu.utexas.cs.sam.io.SamTokenizer
import edu.utexas.cs.sam.io.Tokenizer.TokenType
import scala.collection.mutable.ListBuffer
import assignment3.ast.{Diag, SyntaxDiag, TypeDiag, ResolveDiag}

/** Scala port of high-level structural parser (classes/methods). */
private final class ProgramParser private (tz: SamTokenizer, symbols: ProgramSymbols) {
  private def parseProgram(): ProgramNode = {
    val classes = ListBuffer.empty[ClassNode]
    while (tz.peekAtKind() != TokenType.EOF) {
      classes += parseClass()
    }
    ProgramNode(classes.toList)
  }

  // Diagnostic-first parsing
  def parseProgramD(): Either[Diag, ProgramNode] =
    try Right(parseProgram())
    catch
      case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))
      case te: TypeErrorException   => Left(TypeDiag(te.getMessage, te.line, te.column))
      case ce: CompilerException    => Left(ResolveDiag(ce.getMessage, ce.line, ce.column))
      case t: Throwable             => Left(ResolveDiag(Option(t.getMessage).getOrElse("Unknown error"), -1))

  private def parseClass(): ClassNode = {
    CompilerUtils.expectWord(tz, "class", tz.lineNo())
    val className = CompilerUtils.getIdentifier(tz)
    val csOpt = symbols.getClass(className)
  val cs = csOpt.getOrElse(throw new SyntaxErrorException(s"Class symbol missing for '$className'", tz.lineNo()))
    if (CompilerUtils.check(tz, '(')) {
      if (!CompilerUtils.check(tz, ')')) {
        while (CompilerUtils.isTypeWord(tz, symbols, className, false, true)) {
          val rawType = CompilerUtils.getWord(tz)
          val _vt = CompilerUtils.parseTypeOrObjectName(rawType, tz.lineNo())
          if ("void" == rawType) throw new SyntaxErrorException("'void' is not a valid field type", tz.lineNo())
          CompilerUtils.getIdentifier(tz)
          while (CompilerUtils.check(tz, ',')) { CompilerUtils.getIdentifier(tz) }
          if (!CompilerUtils.check(tz, ';')) throw new SyntaxErrorException("Expected ';' in field list", tz.lineNo())
          if (CompilerUtils.check(tz, ')')) { /* done */ }
        }
      }
    }
    CompilerUtils.expectChar(tz, '{', tz.lineNo())
    val methods = ListBuffer.empty[MethodNode]
    while (!CompilerUtils.check(tz, '}')) {
      methods += parseMethod(className)
    }
    ClassNode(className, methods.toList)
  }

  private def parseMethod(className: String): MethodNode = {
    val returnSig: assignment3.ast.high.ReturnSig =
      if (tz.test("void")) { CompilerUtils.expectWord(tz, "void", tz.lineNo()); assignment3.ast.high.ReturnSig.Void }
      else if (tz.peekAtKind() == TokenType.WORD) {
        val rt = CompilerUtils.getWord(tz)
        val vt = CompilerUtils.parseTypeOrObjectName(rt, tz.lineNo())
        if (vt.isObject) assignment3.ast.high.ReturnSig.Obj(vt.getObject.getClassName)
        else if (vt.isPrimitive) assignment3.ast.high.ReturnSig.Prim(vt.getPrimitive)
        else assignment3.ast.high.ReturnSig.Void
  } else throw new SyntaxErrorException("Expected return type", tz.lineNo())

    val methodName = CompilerUtils.getIdentifier(tz)
    val msOpt = symbols.getMethod(className, methodName)
  val ms = msOpt.getOrElse(throw new SyntaxErrorException(s"Method symbol missing for '$className.$methodName'", tz.lineNo()))

    CompilerUtils.expectChar(tz, '(', tz.lineNo())
    val expectedUserParams = assignment3.ast.MethodUtils.expectedUserArgs(ms)
    val params = ListBuffer.empty[ParamNode]
    var paramIndex = 0
    while (tz.peekAtKind() == TokenType.WORD) {
    val pr = CompilerUtils.getWord(tz)
    val vt = CompilerUtils.parseTypeOrObjectName(pr, tz.lineNo())
    val pTypeOpt = if (vt.isPrimitive) Some(vt.getPrimitive) else None
    val pobjOpt = if (vt.isObject) Some(vt.getObject.getClassName) else None
      val pName = CompilerUtils.getIdentifier(tz)
      if (paramIndex >= expectedUserParams) {
        val atLeast = paramIndex + 1
  throw new SyntaxErrorException(s"Too many parameters in '$methodName': expected $expectedUserParams, got at least $atLeast", tz.lineNo())
      }
      val formal = assignment3.ast.MethodUtils.userParamAt(ms, paramIndex)
      val typeOk = if (formal.isObject) pobjOpt.contains(formal.getClassTypeName) else pTypeOpt.contains(formal.getType)
      if (!typeOk || formal.getName != pName) {
        val position = paramIndex + 1
        val expectedType = if (formal.isObject) formal.getClassTypeName else String.valueOf(formal.getType)
        val actualType = pobjOpt.getOrElse(pTypeOpt.map(_.toString).getOrElse("void"))
        val msg = s"Parameter mismatch in '$methodName' at position $position: expected $expectedType ${formal.getName}, but found $actualType $pName"
  throw new SyntaxErrorException(msg, tz.lineNo())
      }
      params += ParamNode(pName, pobjOpt, pTypeOpt)
      paramIndex += 1
      if (!CompilerUtils.check(tz, ',')) { /* stop */ } else ()
    }
    if (paramIndex != expectedUserParams)
      throw new SyntaxErrorException(s"Parameter count mismatch in '$methodName': expected $expectedUserParams, got $paramIndex", tz.lineNo())
    CompilerUtils.expectChar(tz, ')', tz.lineNo())
    CompilerUtils.expectChar(tz, '{', tz.lineNo())
    skipLocalDecls(tz, className)
    val stmtParser = new assignment3.ast.AstParser(tz, new assignment3.ast.NewMethodContext(ms, symbols), symbols, false)
    val folder = assignment3.ast.IdiomaticConstFolder
    val stmts = ListBuffer.empty[assignment3.ast.Stmt]
    while (!CompilerUtils.check(tz, '}')) {
      val parsed: assignment3.ast.Stmt = stmtParser.parseStmt()
      val s: assignment3.ast.Stmt = folder.foldStmt(parsed)
      assignment3.ast.IdiomaticSemantic.checkStmtE(s, ms, tz.lineNo(), symbols) match {
        case Left(diag)  => throw new SyntaxErrorException(diag.message, diag.line, diag.column)
        case Right(()) => ()
      }
      stmts += s
    }
    val missingReturn = (returnSig != assignment3.ast.high.ReturnSig.Void) && (stmts.isEmpty || !endsWithReturn(stmts.last))
    if (missingReturn) throw new SyntaxErrorException("Method missing final return", tz.lineNo())
    val body = assignment3.ast.Block(stmts.toList)
    MethodNode(className, methodName, params.toList, returnSig, body)
  }

  private def endsWithReturn(s: assignment3.ast.Stmt): Boolean = s match {
    case _: assignment3.ast.Return => true
    case b: assignment3.ast.Block =>
      val inner = b.statements
      inner.nonEmpty && endsWithReturn(inner.last)
    case _ => false
  }

  private def skipLocalDecls(tz: SamTokenizer, className: String): Unit = {
    while (CompilerUtils.isTypeWord(tz, symbols, className, false, true)) {
      val rawType = CompilerUtils.getWord(tz)
      CompilerUtils.parseTypeOrObjectName(rawType, tz.lineNo())
      if ("void" == rawType) throw new SyntaxErrorException("'void' is not a valid local variable type", tz.lineNo())
      var more = true
      while (more) {
        CompilerUtils.getIdentifier(tz)
        if (CompilerUtils.check(tz, ',')) {
          // continue
        } else if (CompilerUtils.check(tz, ';')) {
          more = false
        } else {
          throw new SyntaxErrorException("Expected ',' or ';' in variable declaration", tz.lineNo())
        }
      }
    }
  }
}

private object ProgramParser {
  @throws[Exception]
  def parse(fileName: String, symbols: ProgramSymbols): ProgramNode = {
    val tz = new SamTokenizer(fileName, SamTokenizer.TokenizerOptions.PROCESS_STRINGS)
    new ProgramParser(tz, symbols).parseProgram()
  }
  def parseD(fileName: String, symbols: ProgramSymbols): Either[Diag, ProgramNode] = {
    val tz = new SamTokenizer(fileName, SamTokenizer.TokenizerOptions.PROCESS_STRINGS)
    new ProgramParser(tz, symbols).parseProgramD()
  }
}
