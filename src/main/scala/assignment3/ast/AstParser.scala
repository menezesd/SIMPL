package assignment3.ast

import assignment3._
import assignment3.TokenizerView
import assignment3.ParserBase
import assignment3.ast.{MethodContext, NewMethodContext}
import assignment3.ast.SymbolVarBinding
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
      if ms.numParameters() > 0 && ms.parameters.headOption.exists(_.getName == "this")
      thisSym = ms.parameters.head
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
      def parseOneGroup(): Result[Unit] =
        for
          rawType <- getWordR()
          tuple <-
            try
              val vt = Type.parse(rawType, tv.line)
              ok((Some(vt), Some(assignment3.ValueType.ofPrimitive(vt))))
            catch
              case _: TypeErrorException => ok((Some(Type.INT), Some(assignment3.ValueType.ofObject(rawType))))
          (varTypeOpt, valueTypeOpt) = tuple
          _ <-
            if tv.peekKind() != TokenType.WORD then syntax(Messages.expectedVarNameAfterType)
            else ok(())
          name <- getIdentifierR()
          _ = decls += VarDecl(name, varTypeOpt, valueTypeOpt, None)
          _ <-
            def loop(): Result[Unit] =
              if tv.consumeChar( ',') then
                if tv.peekKind() != TokenType.WORD then syntax(Messages.expectedVarNameAfterType)
                else
                  getIdentifierR().map { nm => decls += VarDecl(nm, varTypeOpt, valueTypeOpt, None) } match
                    case Left(d) => Left(d)
                    case Right(_) => loop()
              else if tv.consumeChar( ';') then ok(())
              else syntax(Messages.expectedVarDeclSeparator)
            loop()
        yield ()
      def outer(): Result[Unit] =
        if tv.peekKind() == TokenType.WORD then parseOneGroup().flatMap(_ => outer()) else ok(())
      outer().map(_ => decls.toList)

  private def parseStmtR(): Result[Stmt] =
    if tv.consumeChar( '{') then
      collectUntil('}')(parseStmtR()).map(Block(_))
    else if tv.consumeChar( ';') then ok(Block(Nil))
    else if tv.peekKind() != TokenType.WORD then syntax(Messages.expectedStatement)
    else if tv.test("break") then
      for { _ <- expectWordR("break"); _ <- expectCharR(';') } yield Break()
    else if tv.test("return") then
      for { _ <- expectWordR("return"); value <- parseExprR(); _ <- expectCharR(';') } yield Return(Some(value))
    else if tv.test("if") then
      for
        _ <- expectWordR("if")
        _ <- expectCharR('(')
        cond <- parseExprR()
        _ <- expectCharR(')')
        thenB <- parseBlockR()
        _ <- expectWordR("else")
        elseB <- parseBlockR()
      yield If(cond, thenB, elseB)
    else if tv.test("while") then
      for
        _ <- expectWordR("while")
        _ <- expectCharR('(')
        cond <- parseExprR()
        _ <- expectCharR(')')
        body <- parseBlockR()
      yield While(cond, body)
    else
      for
        firstIdent <- getIdentifierR()
        lhsBase <-
          method.lookupVar(firstIdent) match
            case Some(_: VarSymbol) if method.isInstanceOf[NewMethodContext] => ok(Var(firstIdent))
            case _ if method.isInstanceOf[NewMethodContext] =>
              tryImplicitThisField(firstIdent).map(ok(_)).getOrElse(syntax(Messages.undeclaredVariable(firstIdent)))
            case _ => syntax(Messages.undeclaredVariable(firstIdent))
        lhsFinal <-
          def loop(base: Expr, dotCount: Int): Result[Expr] =
            if tv.consumeChar( '.') then
              AstEither.checkSingleChainLevelD(dotCount, tv.line, tv.col) match
                case Left(diag) => Left(SyntaxDiag(diag.message, tv.line, tv.col))
                case Right(())  =>
                  getIdentifierR().flatMap { fld =>
                    val fiOpt = resolveFieldInfo(base, fld)
                    loop(FieldAccess(base, fld, fiOpt), dotCount + 1)
                  }
            else ok(base)
          loop(lhsBase, 0)
        _ <- expectCharR('=')
        rhs <- parseExprR()
        _ <- expectCharR(';')
        stmtRes <- lhsFinal match
          case Var(name, _) => ok(Assign(name, rhs))
          case FieldAccess(t, f, fi, _) => ok(FieldAssign(t, f, fi.map(_.offset).getOrElse(-1), rhs))
          case _ => syntax(Messages.unsupportedLhs)
      yield stmtRes

  private def parseBlockR(): Result[Block] =
    for
      _ <- expectCharR('{')
      stmts <- collectUntil('}')(parseStmtR())
    yield Block(stmts)

  private def parseExprR(): Result[Expr] =
    def isBinaryOpAhead(): Boolean =
      tv.test('+') || tv.test('-') || tv.test('*') || tv.test('/') ||
      tv.test('%') || tv.test('&') || tv.test('|') || tv.test('>') ||
      tv.test('<') || tv.test('=')
    if tv.consumeChar( '(') then
      // unary paren expr
      if tv.test('~') || tv.test('!') then
        for
          op <- getOpR()
          inner <- parseExprR()
          _ <- expectCharR(')')
          u <- AstEither.buildUnaryD(op, inner, method, programSymbols, tv.line, tv.col)
        yield u
      else
        for
          first <- parseExprR()
          res <-
            if tv.consumeChar( '?') then
              for
                thenE <- parseExprR()
                _ <- expectCharR(':')
                elseE <- parseExprR()
                te <- AstEither.buildTernaryD(first, thenE, elseE, method, programSymbols, tv.line, tv.col)
                _ <- expectCharR(')')
              yield te
            else if isBinaryOpAhead() then
              for
                op <- getOpR()
                second <- parseExprR()
                be <- buildBinaryR(first, op, second)
                _ <- expectCharR(')')
              yield be
            else
              for
                _ <- expectCharR(')')
              yield first
        yield res
    else
      for
        base0 <- parseTerminalR()
        result <-
          def loop(base: Expr, dotCount: Int): Result[Expr] =
            if tv.consumeChar( '.') then
              AstEither.checkSingleChainLevelD(dotCount, tv.line, tv.col) match
                case Left(diag) => Left(SyntaxDiag(diag.message, tv.line, tv.col))
                case Right(())  =>
                  getIdentifierR().flatMap { ident =>
                    if tv.consumeChar( '(') then
                      // method call
                      for
                        // forbid calling methods on 'this'
                        _ <- (base match
                          case This(_) => AstEither.methodCallOnThisForbiddenD(tv.line, tv.col)
                          case _       => ok(())
                        )
                        args <- parseCommaSeparatedList(')')(parseExprR())
                        msEither = AstEither.resolveMethodOnExprD(base, ident, method, programSymbols, tv.line, tv.col)
                        classNameOpt = IdiomaticTypeUtils.classNameOf(base, method, programSymbols)
                        labelName = classNameOpt.map(cn => s"${cn}_${ident}").getOrElse(ident)
                        retVtOpt: Option[ValueType] = msEither.toOption.map(_.getReturnSig).flatMap {
                          case assignment3.ast.high.ReturnSig.Void => None
                          case assignment3.ast.high.ReturnSig.Obj(cn) => Some(assignment3.ValueType.ofObject(cn))
                          case assignment3.ast.high.ReturnSig.Prim(t) => Some(assignment3.ValueType.ofPrimitive(t))
                        }
                        callable <- (msEither match
                          case Right(ms) =>
                            classNameOpt match
                              case Some(cn) => ok(new assignment3.ast.ScalaInstanceCallable(cn, ms))
                              case None     => err(TypeDiag(Messages.cannotDetermineTargetClass, tv.line, tv.col))
                          case Left(_) => ok(new assignment3.ast.ScalaInstanceCallableFallback(
                              labelName,
                              retVtOpt,
                              args.size + 1
                            ))
                        )
                        next = InstanceCall(base, callable, args)
                        res <- loop(next, dotCount + 1)
                      yield res
                    else
                      val fiOpt = AstEither.resolveFieldInfoD(base, ident, method, programSymbols, tv.line, tv.col).toOption
                      loop(FieldAccess(base, ident, fiOpt), dotCount + 1)
                  }
            else ok(base)
          loop(base0, 0)
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
                      yield Call(new assignment3.ast.SymbolCallableMethod(ms), args)
                    case None =>
                      tryImplicitThisField(w) match
                        case Some(implicitF) => ok(implicitF)
                        case None => syntax(Messages.undeclaredSymbol(w))
        }
      case _ => syntax(Messages.unexpectedTokenInExpression)

  private def buildBinaryR(left: Expr, op: Char, right: Expr): Result[Expr] =
    AstEither.buildBinaryD(left, op, right, method, programSymbols, tv.line, tv.col)
