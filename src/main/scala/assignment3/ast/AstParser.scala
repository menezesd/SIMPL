package assignment3.ast

import assignment3._
import assignment3.ast.{MethodContext, NewMethodContext}
import assignment3.ast.SymbolVarBinding
import assignment3.symbol.{MethodSymbol, VarSymbol}
import edu.utexas.cs.sam.io.SamTokenizer
import edu.utexas.cs.sam.io.Tokenizer.TokenType
import scala.collection.mutable.ListBuffer

/** Idiomatic parser producing sealed AST (Scala 3). */
final class AstParser(tokenizer: SamTokenizer, method: MethodContext, programSymbols: assignment3.symbol.ProgramSymbols, declarationsEnabled: Boolean):
  def this(tokenizer: SamTokenizer, method: MethodContext, programSymbols: assignment3.symbol.ProgramSymbols) =
    this(tokenizer, method, programSymbols, true)

  // Public API (diagnostic-first)

  // Result helpers
  import assignment3.ast.{Result, SyntaxDiag, TypeDiag, ResolveDiag}
  private inline def ok[A](a: A): Result[A] = Right(a)
  private inline def err[A](d: Diag): Result[A] = Left(d)
  private inline def syntax[A](msg: String): Result[A] = Left(SyntaxDiag(msg, tokenizer.lineNo(), CompilerUtils.column(tokenizer)))
  private inline def typeE[A](msg: String): Result[A] = Left(TypeDiag(msg, tokenizer.lineNo(), CompilerUtils.column(tokenizer)))

  // Safe wrappers around throwing CompilerUtils for gradual migration
  private def expectCharR(ch: Char): Result[Unit] =
    try { CompilerUtils.expectChar(tokenizer, ch, tokenizer.lineNo()); ok(()) }
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def expectWordR(word: String): Result[Unit] =
    try { CompilerUtils.expectWord(tokenizer, word, tokenizer.lineNo()); ok(()) }
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def getIdentifierR(): Result[String] =
    try ok(CompilerUtils.getIdentifier(tokenizer))
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def getWordR(): Result[String] =
    try ok(CompilerUtils.getWord(tokenizer))
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def getIntR(): Result[Int] =
    try ok(CompilerUtils.getInt(tokenizer))
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def getStringR(): Result[String] =
    try ok(CompilerUtils.getString(tokenizer))
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def getOpR(): Result[Char] =
    try ok(CompilerUtils.getOp(tokenizer))
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

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
      if thisSym.isObject
      cs <- programSymbols.getClass(thisSym.getClassTypeName)
      fSym <- cs.field(fieldName)
      off = cs.fieldOffset(fieldName)
      vt = fSym.getValueType
      fi = new assignment3.symbol.ClassSymbol.FieldInfo(off, vt, fSym)
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
              val vt = Type.parse(rawType, tokenizer.lineNo())
              ok((Some(vt), Some(assignment3.ValueType.ofPrimitive(vt))))
            catch
              case _: TypeErrorException => ok((Some(Type.INT), Some(assignment3.ValueType.ofObject(rawType))))
          (varTypeOpt, valueTypeOpt) = tuple
          _ <-
            if tokenizer.peekAtKind() != TokenType.WORD then syntax("Expected variable name after type")
            else ok(())
          name <- getIdentifierR()
          _ = decls += VarDecl(name, varTypeOpt, valueTypeOpt, None)
          _ <-
            def loop(): Result[Unit] =
              if CompilerUtils.check(tokenizer, ',') then
                if tokenizer.peekAtKind() != TokenType.WORD then syntax("Expected variable name after type")
                else
                  getIdentifierR().map { nm => decls += VarDecl(nm, varTypeOpt, valueTypeOpt, None) } match
                    case Left(d) => Left(d)
                    case Right(_) => loop()
              else if CompilerUtils.check(tokenizer, ';') then ok(())
              else syntax("Expected ',' or ';' in variable declaration")
            loop()
        yield ()
      def outer(): Result[Unit] =
        if tokenizer.peekAtKind() == TokenType.WORD then parseOneGroup().flatMap(_ => outer()) else ok(())
      outer().map(_ => decls.toList)

  private def parseStmtR(): Result[Stmt] =
    if CompilerUtils.check(tokenizer, '{') then
      val stmts = ListBuffer.empty[Stmt]
      def loop(): Result[Unit] =
        if CompilerUtils.check(tokenizer, '}') then ok(()) else parseStmtR().flatMap(s => { stmts += s; loop() })
      loop().map(_ => Block(stmts.toList))
    else if CompilerUtils.check(tokenizer, ';') then ok(Block(Nil))
    else if tokenizer.peekAtKind() != TokenType.WORD then syntax("Expected statement")
    else if tokenizer.test("break") then
      for { _ <- expectWordR("break"); _ <- expectCharR(';') } yield Break()
    else if tokenizer.test("return") then
      for { _ <- expectWordR("return"); value <- parseExprR(); _ <- expectCharR(';') } yield Return(Some(value))
    else if tokenizer.test("if") then
      for
        _ <- expectWordR("if")
        _ <- expectCharR('(')
        cond <- parseExprR()
        _ <- expectCharR(')')
        thenB <- parseBlockR()
        _ <- expectWordR("else")
        elseB <- parseBlockR()
      yield If(cond, thenB, elseB)
    else if tokenizer.test("while") then
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
              tryImplicitThisField(firstIdent).map(ok(_)).getOrElse(syntax(s"Undeclared variable: $firstIdent"))
            case _ => syntax(s"Undeclared variable: $firstIdent")
        lhsFinal <-
          def loop(base: Expr, dotCount: Int): Result[Expr] =
            if CompilerUtils.check(tokenizer, '.') then
              AstEither.checkSingleChainLevelD(dotCount, tokenizer.lineNo(), CompilerUtils.column(tokenizer)) match
                case Left(diag) => Left(SyntaxDiag(diag.message, tokenizer.lineNo(), CompilerUtils.column(tokenizer)))
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
          case _ => syntax("Unsupported LHS in assignment")
      yield stmtRes

  private def parseBlockR(): Result[Block] =
    for
      _ <- expectCharR('{')
      stmts <-
        val buf = ListBuffer.empty[Stmt]
        def loop(): Result[List[Stmt]] =
          if CompilerUtils.check(tokenizer, '}') then ok(buf.toList)
          else parseStmtR().flatMap(s => { buf += s; loop() })
        loop()
    yield Block(stmts)

  private def parseExprR(): Result[Expr] =
    def isBinaryOpAhead(): Boolean =
      tokenizer.test('+') || tokenizer.test('-') || tokenizer.test('*') || tokenizer.test('/') ||
      tokenizer.test('%') || tokenizer.test('&') || tokenizer.test('|') || tokenizer.test('>') ||
      tokenizer.test('<') || tokenizer.test('=')
    if CompilerUtils.check(tokenizer, '(') then
      // unary paren expr
      if tokenizer.test('~') || tokenizer.test('!') then
        for
          op <- getOpR()
          inner <- parseExprR()
          _ <- expectCharR(')')
          u <- AstEither.buildUnaryD(op, inner, method, programSymbols, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
        yield u
      else
        for
          first <- parseExprR()
          res <-
            if CompilerUtils.check(tokenizer, '?') then
              for
                thenE <- parseExprR()
                _ <- expectCharR(':')
                elseE <- parseExprR()
                te <- AstEither.buildTernaryD(first, thenE, elseE, method, programSymbols, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
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
          def parseArgsR(): Result[List[Expr]] =
            if CompilerUtils.check(tokenizer, ')') then ok(Nil)
            else
              for
                first <- parseExprR()
                args <-
                  def loop(acc: List[Expr]): Result[List[Expr]] =
                    if CompilerUtils.check(tokenizer, ',') then
                      parseExprR().flatMap(e => loop(acc :+ e))
                    else expectCharR(')').map(_ => acc)
                  loop(List(first))
              yield args
          def loop(base: Expr, dotCount: Int): Result[Expr] =
            if CompilerUtils.check(tokenizer, '.') then
              AstEither.checkSingleChainLevelD(dotCount, tokenizer.lineNo(), CompilerUtils.column(tokenizer)) match
                case Left(diag) => Left(SyntaxDiag(diag.message, tokenizer.lineNo(), CompilerUtils.column(tokenizer)))
                case Right(())  =>
                  getIdentifierR().flatMap { ident =>
                    if CompilerUtils.check(tokenizer, '(') then
                      // method call
                      for
                        // forbid calling methods on 'this'
                        _ <- (base match
                          case This(_) => AstEither.methodCallOnThisForbiddenD(tokenizer.lineNo(), CompilerUtils.column(tokenizer))
                          case _       => ok(())
                        )
                        args <- parseArgsR()
                        msEither = AstEither.resolveMethodOnExprD(base, ident, method, programSymbols, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
                        classNameOpt = IdiomaticTypeUtils.classNameOf(base, method, programSymbols)
                        labelName = classNameOpt.map(cn => s"${cn}_${ident}").getOrElse(ident)
                        retVtOpt = msEither.toOption.map(_.getReturnSig).map {
                          case assignment3.ast.high.ReturnSig.Void => null
                          case assignment3.ast.high.ReturnSig.Obj(cn) => assignment3.ValueType.ofObject(cn)
                          case assignment3.ast.high.ReturnSig.Prim(t) => assignment3.ValueType.ofPrimitive(t)
                        }
                        callable <- (msEither match
                          case Right(ms) =>
                            classNameOpt match
                              case Some(cn) => ok(new assignment3.ast.ScalaInstanceCallable(cn, ms))
                              case None     => err(TypeDiag("Cannot determine target class for instance call", tokenizer.lineNo(), CompilerUtils.column(tokenizer)))
                          case Left(_) => ok(new assignment3.ast.ScalaInstanceCallableFallback(
                              labelName,
                              retVtOpt.getOrElse(null),
                              args.size + 1
                            ))
                        )
                        next = InstanceCall(base, callable, args)
                        res <- loop(next, dotCount + 1)
                      yield res
                    else
                      val fiOpt = AstEither.resolveFieldInfoD(base, ident, method, programSymbols, tokenizer.lineNo(), CompilerUtils.column(tokenizer)).toOption
                      loop(FieldAccess(base, ident, fiOpt), dotCount + 1)
                  }
            else ok(base)
          loop(base0, 0)
      yield result

  private def parseTerminalR(): Result[Expr] =
    tokenizer.peekAtKind() match
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
              def parseArgsAfterOpenParenR(): Result[List[Expr]] =
                if CompilerUtils.check(tokenizer, ')') then ok(Nil)
                else
                  for
                    first <- parseExprR()
                    args <-
                      def loop(acc: List[Expr]): Result[List[Expr]] =
                        if CompilerUtils.check(tokenizer, ',') then
                          parseExprR().flatMap(e => loop(acc :+ e))
                        else expectCharR(')').map(_ => acc)
                      loop(List(first))
                  yield args
              for
                cls <- getIdentifierR()
                _ <- expectCharR('(')
                args <- parseArgsAfterOpenParenR()
              yield NewObject(cls, args)
            case _ =>
              method.lookupVar(w) match
                case Some(vs: assignment3.symbol.VarSymbol) if method.isInstanceOf[NewMethodContext] => ok(Var(vs.getName))
                case _ =>
                  method.lookupMethodGlobal(w) match
                    case Some(ms: assignment3.symbol.MethodSymbol) =>
                      def parseArgsAfterOpenParenR(): Result[List[Expr]] =
                        if CompilerUtils.check(tokenizer, ')') then ok(Nil)
                        else
                          for
                            first <- parseExprR()
                            args <-
                              def loop(acc: List[Expr]): Result[List[Expr]] =
                                if CompilerUtils.check(tokenizer, ',') then
                                  parseExprR().flatMap(e => loop(acc :+ e))
                                else expectCharR(')').map(_ => acc)
                              loop(List(first))
                          yield args
                      for
                        _ <- expectCharR('(')
                        args <- parseArgsAfterOpenParenR()
                      yield Call(new assignment3.ast.SymbolCallableMethod(ms), args)
                    case None =>
                      tryImplicitThisField(w) match
                        case Some(implicitF) => ok(implicitF)
                        case None => syntax(s"Undeclared symbol: $w")
        }
      case _ => syntax("Unexpected token in expression")

  private def buildBinaryR(left: Expr, op: Char, right: Expr): Result[Expr] =
    AstEither.buildBinaryD(left, op, right, method, programSymbols, tokenizer.lineNo(), CompilerUtils.column(tokenizer))

  // Removed legacy throwing parseTerminal; parseTerminalR is the canonical one
  
