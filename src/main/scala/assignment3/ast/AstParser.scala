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

  // Public API
  def parseExpr(): Expr = parseExprInternal()
  def parseStmt(): Stmt = parseStmtInternal()
  def parseBlock(): Block = parseBlockInternal()
  def parseVarDecls(): List[VarDecl] = parseVarDeclsInternal()

  private def tryImplicitThisField(fieldName: String): Option[FieldAccess] =
    for {
      nmc <- method match { case n: NewMethodContext => Some(n); case _ => None }
      ms = nmc.getSymbol
      if ms.numParameters() > 0 && ms.getParameters.get(0).getName == "this"
      thisSym = ms.getParameters.get(0)
      if thisSym.isObject
      cs <- programSymbols.getClass(thisSym.getClassTypeName)
      fSym <- cs.getField(fieldName)
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

  private def parseVarDeclsInternal(): List[VarDecl] =
    if !declarationsEnabled then return Nil
    val decls = ListBuffer.empty[VarDecl]
    while tokenizer.peekAtKind() == TokenType.WORD do
      val rawType = CompilerUtils.getWord(tokenizer)
      var varTypeOpt: Option[Type] = None
      var valueTypeOpt: Option[assignment3.ValueType] = None
      try
        val vt = Type.parse(rawType, tokenizer.lineNo()); varTypeOpt = Some(vt); valueTypeOpt = Some(assignment3.ValueType.ofPrimitive(vt))
      catch
        case _: TypeErrorException => varTypeOpt = Some(Type.INT); valueTypeOpt = Some(assignment3.ValueType.ofObject(rawType))
      var more = true
      while more do
        if tokenizer.peekAtKind() != TokenType.WORD then throw new SyntaxErrorException("Expected variable name after type", tokenizer.lineNo(), CompilerUtils.column(tokenizer))
        val name = CompilerUtils.getIdentifier(tokenizer)
        decls += VarDecl(name, varTypeOpt, valueTypeOpt, None)
        if CompilerUtils.check(tokenizer, ',') then ()
        else if CompilerUtils.check(tokenizer, ';') then more = false
        else throw new SyntaxErrorException("Expected ',' or ';' in variable declaration", tokenizer.lineNo(), CompilerUtils.column(tokenizer))
    decls.toList

  private def parseStmtInternal(): Stmt =
    if CompilerUtils.check(tokenizer, '{') then
      val stmts = ListBuffer.empty[Stmt]
      while !CompilerUtils.check(tokenizer, '}') do stmts += parseStmtInternal()
      return Block(stmts.toList)
    if CompilerUtils.check(tokenizer, ';') then return Block(Nil)
    if tokenizer.peekAtKind() != TokenType.WORD then throw new SyntaxErrorException("Expected statement", tokenizer.lineNo(), CompilerUtils.column(tokenizer))
    if tokenizer.test("break") then { CompilerUtils.expectWord(tokenizer, "break", tokenizer.lineNo()); CompilerUtils.expectChar(tokenizer, ';', tokenizer.lineNo()); return Break() }
    if tokenizer.test("return") then { CompilerUtils.expectWord(tokenizer, "return", tokenizer.lineNo()); val value = parseExprInternal(); CompilerUtils.expectChar(tokenizer, ';', tokenizer.lineNo()); return Return(Option(value)) }
    if tokenizer.test("if") then
      CompilerUtils.expectWord(tokenizer, "if", tokenizer.lineNo()); CompilerUtils.expectChar(tokenizer, '(', tokenizer.lineNo())
      val cond = parseExprInternal(); CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo())
      val thenB = parseBlockInternal(); CompilerUtils.expectWord(tokenizer, "else", tokenizer.lineNo()); val elseB = parseBlockInternal()
      return If(cond, thenB, elseB)
    if tokenizer.test("while") then
      CompilerUtils.expectWord(tokenizer, "while", tokenizer.lineNo()); CompilerUtils.expectChar(tokenizer, '(', tokenizer.lineNo())
      val cond = parseExprInternal(); CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo()); val body = parseBlockInternal()
      return While(cond, body)
    val firstIdent = CompilerUtils.getIdentifier(tokenizer)
    var lhsBase: Expr =
      method.lookupVar(firstIdent) match
        case Some(_: VarSymbol) if method.isInstanceOf[NewMethodContext] => Var(firstIdent)
        case _ if method.isInstanceOf[NewMethodContext] =>
          tryImplicitThisField(firstIdent).getOrElse(throw new SyntaxErrorException("Undeclared variable: " + firstIdent, tokenizer.lineNo(), CompilerUtils.column(tokenizer)))
        case _ => throw new SyntaxErrorException("Undeclared variable: " + firstIdent, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
    var dotCount = 0
    while CompilerUtils.check(tokenizer, '.') do
      AstEither.checkSingleChainLevelD(dotCount, tokenizer.lineNo(), CompilerUtils.column(tokenizer)) match
        case Left(diag) => throw new SyntaxErrorException(diag.message, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
        case Right(())  => ()
      dotCount += 1
      val fld = CompilerUtils.getIdentifier(tokenizer)
      val fiOpt = resolveFieldInfo(lhsBase, fld)
      lhsBase = FieldAccess(lhsBase, fld, fiOpt)
    CompilerUtils.expectChar(tokenizer, '=', tokenizer.lineNo())
    val rhs = parseExprInternal()
    CompilerUtils.expectChar(tokenizer, ';', tokenizer.lineNo())
    lhsBase match
      case Var(name, _) => Assign(name, rhs)
      case FieldAccess(t, f, fi, _) => FieldAssign(t, f, fi.map(_.offset).getOrElse(-1), rhs)
      case _ => throw new SyntaxErrorException("Unsupported LHS in assignment", tokenizer.lineNo(), CompilerUtils.column(tokenizer))

  private def parseBlockInternal(): Block =
    CompilerUtils.expectChar(tokenizer, '{', tokenizer.lineNo())
    val stmts = ListBuffer.empty[Stmt]
    while !CompilerUtils.check(tokenizer, '}') do stmts += parseStmtInternal()
    Block(stmts.toList)

  private def parseExprInternal(): Expr =
    if CompilerUtils.check(tokenizer, '(') then
      if tokenizer.test('~') || tokenizer.test('!') then
        val op = CompilerUtils.getOp(tokenizer)
        val inner = parseExprInternal()
        CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo())
        AstEither.buildUnaryD(op, inner, method, programSymbols, tokenizer.lineNo(), CompilerUtils.column(tokenizer)) match
          case Right(u) => return u
          case Left(diag) => throw new TypeErrorException(diag.message, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
      val first = parseExprInternal()
      if tokenizer.peekAtKind() != edu.utexas.cs.sam.io.Tokenizer.TokenType.OPERATOR then
        throw new SyntaxErrorException("Expected operator/?/) after expression", tokenizer.lineNo(), CompilerUtils.column(tokenizer))
      if CompilerUtils.check(tokenizer, ')') then return first
      if CompilerUtils.check(tokenizer, '?') then
        val thenE = parseExprInternal()
        CompilerUtils.expectChar(tokenizer, ':', tokenizer.lineNo())
        val elseE = parseExprInternal()
        AstEither.buildTernaryD(first, thenE, elseE, method, programSymbols, tokenizer.lineNo(), CompilerUtils.column(tokenizer)) match
          case Right(te) =>
            CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo())
            return te
          case Left(diag) =>
            throw new TypeErrorException(diag.message, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
      val op = CompilerUtils.getOp(tokenizer)
      val second = parseExprInternal()
      CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo())
      return buildBinary(first, op, second)
    var base = parseTerminal()
    var dotCount = 0
    while CompilerUtils.check(tokenizer, '.') do
      AstEither.checkSingleChainLevelD(dotCount, tokenizer.lineNo(), CompilerUtils.column(tokenizer)) match
        case Left(diag) => throw new SyntaxErrorException(diag.message, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
        case Right(())  => ()
      dotCount += 1
      val ident = CompilerUtils.getIdentifier(tokenizer)
      if CompilerUtils.check(tokenizer, '(') then
        // Explicit method calls on 'this' are not allowed in LO-3 tests
        base match
          case This(_) =>
            AstEither.methodCallOnThisForbiddenD(tokenizer.lineNo(), CompilerUtils.column(tokenizer)) match
              case Left(diag) => throw new SyntaxErrorException(diag.message, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
              case Right(_) => ()
          case _ => ()
        val args = ListBuffer.empty[Expr]
        if !CompilerUtils.check(tokenizer, ')') then
          args += parseExprInternal()
          while CompilerUtils.check(tokenizer, ',') do args += parseExprInternal()
          CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo())
        val msEither = AstEither.resolveMethodOnExprD(base, ident, method, programSymbols, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
        val classNameOpt = IdiomaticTypeUtils.classNameOf(base, method, programSymbols)
        val labelName = classNameOpt.map(cn => s"${cn}_${ident}").getOrElse(ident)
        val retVtOpt: Option[assignment3.ValueType] = msEither.toOption.map(_.getReturnSig).map {
          case assignment3.ast.high.ReturnSig.Void => null
          case assignment3.ast.high.ReturnSig.Obj(cn) => assignment3.ValueType.ofObject(cn)
          case assignment3.ast.high.ReturnSig.Prim(t) => assignment3.ValueType.ofPrimitive(t)
        }
        val callable: assignment3.ast.CallableMethod = msEither match
          case Right(ms) =>
            val cn = classNameOpt.getOrElse {
              throw new TypeErrorException("Cannot determine target class for instance call", tokenizer.lineNo(), CompilerUtils.column(tokenizer))
            }
            new assignment3.ast.ScalaInstanceCallable(cn, ms)
          case Left(_) => new assignment3.ast.ScalaInstanceCallableFallback(
            labelName,
            retVtOpt.orNull,
            args.size + 1
          )
        base = InstanceCall(base, callable, args.toList)
      else
        val fiOpt = AstEither.resolveFieldInfoD(base, ident, method, programSymbols, tokenizer.lineNo(), CompilerUtils.column(tokenizer)).toOption
        base = FieldAccess(base, ident, fiOpt)
    base

  private def parseTerminal(): Expr =
    tokenizer.peekAtKind() match
      case TokenType.INTEGER => IntLit(CompilerUtils.getInt(tokenizer))
      case TokenType.STRING  => StrLit(CompilerUtils.getString(tokenizer))
      case TokenType.WORD =>
        val w = CompilerUtils.getWord(tokenizer)
        w match
          case "true"  => return BoolLit(true)
          case "false" => return BoolLit(false)
          case "null"  => return NullLit()
          case "this"  => return This()
          case "new" =>
            val cls = CompilerUtils.getIdentifier(tokenizer)
            CompilerUtils.expectChar(tokenizer, '(', tokenizer.lineNo())
            val args = ListBuffer.empty[Expr]
            if !CompilerUtils.check(tokenizer, ')') then
              args += parseExprInternal()
              while CompilerUtils.check(tokenizer, ',') do args += parseExprInternal()
              CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo())
            return NewObject(cls, args.toList)
          case _ =>
            method.lookupVar(w) match
              case Some(vs: VarSymbol) if method.isInstanceOf[NewMethodContext] =>
                Var(vs.getName)
              case _ =>
                method.lookupMethodGlobal(w) match
                  case Some(ms: MethodSymbol) =>
                    CompilerUtils.expectChar(tokenizer, '(', tokenizer.lineNo())
                    val args = ListBuffer.empty[Expr]
                    if !CompilerUtils.check(tokenizer, ')') then
                      args += parseExprInternal()
                      while CompilerUtils.check(tokenizer, ',') do args += parseExprInternal()
                      CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo())
                    Call(new assignment3.ast.SymbolCallableMethod(ms), args.toList)
                  case None =>
                    tryImplicitThisField(w) match
                      case Some(implicitF) => return implicitF
                      case None => throw new SyntaxErrorException("Undeclared symbol: " + w, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
      case _ => throw new SyntaxErrorException("Unexpected token in expression", tokenizer.lineNo(), CompilerUtils.column(tokenizer))

  private def buildBinary(left: Expr, op: Char, right: Expr): Expr =
    AstEither.buildBinaryD(left, op, right, method, programSymbols, tokenizer.lineNo(), CompilerUtils.column(tokenizer)) match
      case Right(expr) => expr
      case Left(diag)  => throw new TypeErrorException(diag.message, tokenizer.lineNo(), CompilerUtils.column(tokenizer))
