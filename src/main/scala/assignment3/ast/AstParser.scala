package assignment3.ast

import assignment3.{CompilerUtils, Messages, ParserBase, TokenizerView, Type, ValueType}
import assignment3.symbol.{MethodSymbol, VarSymbol}
import edu.utexas.cs.sam.io.SamTokenizer
import edu.utexas.cs.sam.io.Tokenizer.TokenType
import scala.collection.mutable.ListBuffer

/** Idiomatic parser producing sealed AST (Scala 3). */
final class AstParser(
  tokenizer: SamTokenizer,
  method: MethodContext,
  programSymbols: assignment3.symbol.ProgramSymbols,
  declarationsEnabled: Boolean,
  rules: CompilerUtils.LexicalRules = CompilerUtils.LexicalRules.Default
)(using CompilerUtils.RecorderContext) extends ParserBase:
  def this(tokenizer: SamTokenizer, method: MethodContext, programSymbols: assignment3.symbol.ProgramSymbols)(using CompilerUtils.RecorderContext) =
    this(tokenizer, method, programSymbols, true, CompilerUtils.LexicalRules.Default)

  // Public API (diagnostic-first)

  import assignment3.ast.{Result, SyntaxDiag, TypeDiag, ResolveDiag}
  override protected val tv: TokenizerView = new TokenizerView(tokenizer, rules)

  // Diagnostic-first, no-throw variants
  def parseExprD(): Result[Expr] = parseExprR()
  def parseStmtD(): Result[Stmt] = parseStmtR()
  def parseBlockD(): Result[Block] = parseBlockR()
  def parseVarDeclsD(): Result[List[VarDecl]] = parseVarDeclsR()

  private def tryImplicitThisField(fieldName: String): Option[FieldAccess] =
    for {
      nmc <- method match { case n: NewMethodContext => Some(n); case _ => None }
      ms = nmc.getSymbol
      thisSym <- ms.parameters.headOption if thisSym.getName == "this"
      className <- thisSym.classTypeNameOpt
      cs <- programSymbols.getClass(className)
      fSym <- cs.field(fieldName)
      off = cs.fieldOffset(fieldName)
      fi = assignment3.symbol.ClassSymbol.FieldInfo(off, fSym.valueType, fSym)
    } yield FieldAccess(This(), fieldName, Some(fi))

  private def resolveFieldInfo(target: Expr, fieldName: String): Option[assignment3.symbol.ClassSymbol.FieldInfo] =
    for
      cn <- IdiomaticTypeUtils.classNameOf(target, method, programSymbols)
      cs <- programSymbols.getClass(cn)
      fi <- cs.getFieldInfo(fieldName)
    yield fi

  private def parseVarDeclsR(): Result[List[VarDecl]] =
    if !declarationsEnabled then ok(Nil)
    else
      val decls = ListBuffer.empty[VarDecl]
      parseVarDeclGroupsR(decls).map(_ => decls.toList)

  /** Parse all variable declaration groups (e.g., "int x, y; String z;") */
  private def parseVarDeclGroupsR(decls: ListBuffer[VarDecl]): Result[Unit] =
    if tv.peekKind() == TokenType.WORD then
      parseOneVarGroupR(decls).flatMap(_ => parseVarDeclGroupsR(decls))
    else ok(())

  /** Parse a single declaration group: "Type name1, name2, ...;" */
  private def parseOneVarGroupR(decls: ListBuffer[VarDecl]): Result[Unit] =
    for
      rawType <- getWordR()
      (varTypeOpt, valueTypeOpt) = Type.parseE(rawType, tv.line) match
        case Right(vt) => (Some(vt), Some(assignment3.ValueType.ofPrimitive(vt)))
        case Left(_)   => (Some(Type.INT), Some(assignment3.ValueType.ofObject(rawType)))
      _ <- if tv.peekKind() != TokenType.WORD then syntax(Messages.expectedVarNameAfterType) else ok(())
      name <- getIdentifierR()
      _ = decls += VarDecl(name, varTypeOpt, valueTypeOpt, None)
      _ <- parseRemainingVarNamesR(decls, varTypeOpt, valueTypeOpt)
    yield ()

  /** Parse remaining variable names after the first: ", name2, name3" until ";" */
  private def parseRemainingVarNamesR(
    decls: ListBuffer[VarDecl],
    varTypeOpt: Option[Type],
    valueTypeOpt: Option[ValueType]
  ): Result[Unit] =
    if tv.consumeChar(',') then
      if tv.peekKind() != TokenType.WORD then syntax(Messages.expectedVarNameAfterType)
      else getIdentifierR().flatMap { nm =>
        decls += VarDecl(nm, varTypeOpt, valueTypeOpt, None)
        parseRemainingVarNamesR(decls, varTypeOpt, valueTypeOpt)
      }
    else if tv.consumeChar(';') then ok(())
    else syntax(Messages.expectedVarDeclSeparator)

  // --- Statement parsing helpers ---

  private def parseBreakR(): Result[Stmt] =
    for { _ <- expectWordR("break"); _ <- expectCharR(';') } yield Break()

  private def parseReturnR(): Result[Stmt] =
    for { _ <- expectWordR("return"); value <- parseExprR(); _ <- expectCharR(';') } yield Return(Some(value))

  private def parseIfR(): Result[Stmt] =
    for
      _ <- expectWordR("if")
      _ <- expectCharR('(')
      cond <- parseExprR()
      _ <- expectCharR(')')
      thenB <- parseBlockR()
      _ <- expectWordR("else")
      elseB <- parseBlockR()
    yield If(cond, thenB, elseB)

  private def parseWhileR(): Result[Stmt] =
    for
      _ <- expectWordR("while")
      _ <- expectCharR('(')
      cond <- parseExprR()
      _ <- expectCharR(')')
      body <- parseBlockR()
    yield While(cond, body)

  private def parseAssignmentR(): Result[Stmt] =
    for
      firstIdent <- getIdentifierR()
      lhsBase <-
        method.lookupVar(firstIdent) match
          case Some(_: VarSymbol) if method.isInstanceOf[NewMethodContext] => ok(Var(firstIdent))
          case _ if method.isInstanceOf[NewMethodContext] =>
            tryImplicitThisField(firstIdent).map(ok(_)).getOrElse(syntax(Messages.undeclaredVariable(firstIdent)))
          case _ => syntax(Messages.undeclaredVariable(firstIdent))
      lhsFinal <- parseLhsChainR(lhsBase)
      _ <- expectCharR('=')
      rhs <- parseExprR()
      _ <- expectCharR(';')
      stmtRes <- lhsFinal match
        case Var(name, _) => ok(Assign(name, rhs))
        case FieldAccess(t, f, fi, _) => ok(FieldAssign(t, f, fi.map(_.offset).getOrElse(-1), rhs))
        case _ => syntax(Messages.unsupportedLhs)
    yield stmtRes

  private def parseLhsChainR(base: Expr): Result[Expr] =
    def loop(current: Expr, dotCount: Int): Result[Expr] =
      if tv.consumeChar('.') then
        AstEither.checkSingleChainLevelD(dotCount, tv.line, tv.col) match
          case Left(diag) => Left(SyntaxDiag(diag.message, tv.line, tv.col))
          case Right(()) =>
            getIdentifierR().flatMap { fld =>
              val fiOpt = resolveFieldInfo(current, fld)
              loop(FieldAccess(current, fld, fiOpt), dotCount + 1)
            }
      else ok(current)
    loop(base, 0)

  private def parseStmtR(): Result[Stmt] =
    if tv.consumeChar('{') then
      collectUntil('}')(parseStmtR()).map(Block(_))
    else if tv.consumeChar(';') then ok(Block(Nil))
    else if tv.peekKind() != TokenType.WORD then syntax(Messages.expectedStatement)
    else if tv.test("break") then parseBreakR()
    else if tv.test("return") then parseReturnR()
    else if tv.test("if") then parseIfR()
    else if tv.test("while") then parseWhileR()
    else parseAssignmentR()

  private def parseBlockR(): Result[Block] =
    for
      _ <- expectCharR('{')
      stmts <- collectUntil('}')(parseStmtR())
    yield Block(stmts)

  // Helper: check if next token is a binary operator
  private def isBinaryOpAhead(): Boolean =
    tv.test('+') || tv.test('-') || tv.test('*') || tv.test('/') ||
    tv.test('%') || tv.test('&') || tv.test('|') || tv.test('>') ||
    tv.test('<') || tv.test('=')

  // Helper: parse unary expression inside parentheses
  private def parseUnaryParenR(): Result[Expr] =
    for
      op <- getOpR()
      inner <- parseExprR()
      _ <- expectCharR(')')
      u <- AstEither.buildUnaryD(op, inner, method, programSymbols, tv.line, tv.col)
    yield u

  // Helper: parse ternary expression after seeing '?'
  private def parseTernaryTailR(cond: Expr): Result[Expr] =
    for
      thenE <- parseExprR()
      _ <- expectCharR(':')
      elseE <- parseExprR()
      te <- AstEither.buildTernaryD(cond, thenE, elseE, method, programSymbols, tv.line, tv.col)
      _ <- expectCharR(')')
    yield te

  // Helper: parse binary expression tail
  private def parseBinaryTailR(left: Expr): Result[Expr] =
    for
      op <- getOpR()
      right <- parseExprR()
      be <- buildBinaryR(left, op, right)
      _ <- expectCharR(')')
    yield be

  // Helper: parse parenthesized expression (unary, binary, ternary, or grouped)
  private def parseParenExprR(): Result[Expr] =
    if tv.test('~') || tv.test('!') then parseUnaryParenR()
    else
      for
        first <- parseExprR()
        res <-
          if tv.consumeChar('?') then parseTernaryTailR(first)
          else if isBinaryOpAhead() then parseBinaryTailR(first)
          else expectCharR(')').map(_ => first)
      yield res

  // Helper: parse method call on target expression
  private def parseMethodCallR(base: Expr, methodName: String, dotCount: Int): Result[Expr] =
    for
      _ <- base match
        case This(_) => AstEither.methodCallOnThisForbiddenD(tv.line, tv.col)
        case _       => ok(())
      args <- parseCommaSeparatedList(')')(parseExprR())
      msEither = AstEither.resolveMethodOnExprD(base, methodName, method, programSymbols, tv.line, tv.col)
      classNameOpt = IdiomaticTypeUtils.classNameOf(base, method, programSymbols)
      labelName = classNameOpt.fold(methodName)(cn => s"${cn}_${methodName}")
      retVtOpt: Option[ValueType] = msEither.toOption.map(_.getReturnSig).flatMap {
        case assignment3.ast.high.ReturnSig.Void => None
        case assignment3.ast.high.ReturnSig.Obj(cn) => Some(assignment3.ValueType.ofObject(cn))
        case assignment3.ast.high.ReturnSig.Prim(t) => Some(assignment3.ValueType.ofPrimitive(t))
      }
      callable <- msEither match
        case Right(ms) =>
          classNameOpt match
            case Some(cn) => ok(ScalaCallableMethod.forInstance(cn, ms))
            case None     => err(TypeDiag(Messages.cannotDetermineTargetClass, tv.line, tv.col))
        case Left(_) => ok(ScalaCallableMethod.fallback(labelName, retVtOpt, args.size + 1))
      next = InstanceCall(base, callable, args)
      res <- parseChainedAccessR(next, dotCount + 1)
    yield res

  // Helper: parse chain of field accesses and method calls after base expression
  private def parseChainedAccessR(base: Expr, dotCount: Int): Result[Expr] =
    if tv.consumeChar('.') then
      AstEither.checkSingleChainLevelD(dotCount, tv.line, tv.col) match
        case Left(diag) => Left(SyntaxDiag(diag.message, tv.line, tv.col))
        case Right(()) =>
          getIdentifierR().flatMap { ident =>
            if tv.consumeChar('(') then parseMethodCallR(base, ident, dotCount)
            else
              val fiOpt = AstEither.resolveFieldInfoD(base, ident, method, programSymbols, tv.line, tv.col).toOption
              parseChainedAccessR(FieldAccess(base, ident, fiOpt), dotCount + 1)
          }
    else ok(base)

  private def parseExprR(): Result[Expr] =
    if tv.consumeChar('(') then parseParenExprR()
    else
      for
        base0 <- parseTerminalR()
        result <- parseChainedAccessR(base0, 0)
      yield result

  private def parseTerminalR(): Result[Expr] =
    tv.peekKind() match
      case TokenType.INTEGER => getIntR().map(IntLit(_))
      case TokenType.STRING  => getStringR().map(StrLit(_))
      case TokenType.WORD =>
        getWordR().flatMap { w =>
          w match
            case "true"  => ok(BoolLit(true))
            case "false" => ok(BoolLit(false))
            case "null"  => ok(NullLit())
            case "this"  => ok(This())
            case "new" =>
              for
                cls <- getIdentifierR()
                _ <- expectCharR('(')
                args <- parseCommaSeparatedList(')')(parseExprR())
              yield NewObject(cls, args)
            case _ =>
              method.lookupVar(w) match
                case Some(vs: assignment3.symbol.VarSymbol) if method.isInstanceOf[NewMethodContext] => ok(Var(vs.getName))
                case _ =>
                  method.lookupMethodGlobal(w) match
                    case Some(ms: assignment3.symbol.MethodSymbol) =>
                      for
                        _ <- expectCharR('(')
                        args <- parseCommaSeparatedList(')')(parseExprR())
                      yield Call(ScalaCallableMethod.forSymbol(ms), args)
                    case None =>
                      tryImplicitThisField(w) match
                        case Some(implicitF) => ok(implicitF)
                        case None => syntax(Messages.undeclaredSymbol(w))
        }
      case _ => syntax(Messages.unexpectedTokenInExpression)

  private def buildBinaryR(left: Expr, op: Char, right: Expr): Result[Expr] =
    AstEither.buildBinaryD(left, op, right, method, programSymbols, tv.line, tv.col)
