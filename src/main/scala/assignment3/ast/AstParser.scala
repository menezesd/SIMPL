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

  private def tryImplicitThisField(fieldName: String): FieldAccess =
    method match
      case nmc: NewMethodContext =>
        val ms = nmc.getSymbol
        if (ms.numParameters() == 0 || ms.getParameters.get(0).getName != "this") return null
        val thisSym = ms.getParameters.get(0)
        val thisType = thisSym.getObjectType
        if (thisType == null) return null
        val ps = programSymbols; if (ps == null) return null
        val cs = ps.getClass(thisType.getClassName); if (cs == null) return null
        val off = cs.fieldOffset(fieldName)
        val fSym = cs.getField(fieldName); if (fSym == null) return null
        val vt = fSym.getValueType
        val fi = new assignment3.symbol.ClassSymbol.FieldInfo(off, vt, fSym)
        FieldAccess(This(), fieldName, fi)
      case _ => null

  private def resolveFieldInfo(target: Expr, fieldName: String): assignment3.symbol.ClassSymbol.FieldInfo =
    val ps = programSymbols; if ps == null then return null
    val className = IdiomaticTypeUtils.classNameOf(target, method, programSymbols); if className == null then return null
    val cs = ps.getClass(className); if cs == null then return null
    cs.getFieldInfo(fieldName)

  private def parseVarDeclsInternal(): List[VarDecl] =
    if !declarationsEnabled then return Nil
    val decls = ListBuffer.empty[VarDecl]
    while tokenizer.peekAtKind() == TokenType.WORD do
      val rawType = CompilerUtils.getWord(tokenizer)
      var varType: Type = null
      var valueType: assignment3.ValueType = null
      try
        varType = Type.parse(rawType, tokenizer.lineNo()); valueType = assignment3.ValueType.ofPrimitive(varType)
      catch
        case _: TypeErrorException => varType = Type.INT; valueType = assignment3.ValueType.ofObject(rawType)
      var more = true
      while more do
        if tokenizer.peekAtKind() != TokenType.WORD then throw new SyntaxErrorException("Expected variable name after type", tokenizer.lineNo())
        val name = CompilerUtils.getIdentifier(tokenizer)
        decls += VarDecl(name, Option(varType), Option(valueType), None)
        if CompilerUtils.check(tokenizer, ',') then ()
        else if CompilerUtils.check(tokenizer, ';') then more = false
        else throw new SyntaxErrorException("Expected ',' or ';' in variable declaration", tokenizer.lineNo())
    decls.toList

  private def parseStmtInternal(): Stmt =
    if CompilerUtils.check(tokenizer, '{') then
      val stmts = ListBuffer.empty[Stmt]
      while !CompilerUtils.check(tokenizer, '}') do stmts += parseStmtInternal()
      return Block(stmts.toList)
    if CompilerUtils.check(tokenizer, ';') then return Block(Nil)
    if tokenizer.peekAtKind() != TokenType.WORD then throw new SyntaxErrorException("Expected statement", tokenizer.lineNo())
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
    var lhsBase: Expr = null
    method.lookup(firstIdent) match
      case vs: VarSymbol if method.isInstanceOf[NewMethodContext] =>
        lhsBase = Var(firstIdent)
      case _ if method.isInstanceOf[NewMethodContext] =>
        lhsBase = tryImplicitThisField(firstIdent)
        if lhsBase == null then throw new SyntaxErrorException("Undeclared variable: " + firstIdent, tokenizer.lineNo())
      case _ => throw new SyntaxErrorException("Undeclared variable: " + firstIdent, tokenizer.lineNo())
    var dotCount = 0
    while CompilerUtils.check(tokenizer, '.') do
      if dotCount > 0 then throw new SyntaxErrorException("Inappropriate method/field chaining", tokenizer.lineNo())
      dotCount += 1
      val fld = CompilerUtils.getIdentifier(tokenizer)
      val fi = resolveFieldInfo(lhsBase, fld)
      lhsBase = FieldAccess(lhsBase, fld, fi)
    CompilerUtils.expectChar(tokenizer, '=', tokenizer.lineNo())
    val rhs = parseExprInternal()
    CompilerUtils.expectChar(tokenizer, ';', tokenizer.lineNo())
    lhsBase match
      case Var(name, _) => Assign(name, rhs)
      case FieldAccess(t, f, fi, _) => FieldAssign(t, f, if fi != null then fi.offset else -1, rhs)
      case _ => throw new SyntaxErrorException("Unsupported LHS in assignment", tokenizer.lineNo())

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
        val t = IdiomaticTypeUtils.typeOf(inner, method, programSymbols)
        if op == '~' then
          if t != Type.INT && t != Type.STRING then throw new TypeErrorException("'~' requires INT or STRING", tokenizer.lineNo())
          return Unary(if t == Type.STRING then UnaryOp.Neg else UnaryOp.Neg, inner, Some(if t == Type.STRING then Type.STRING else Type.INT))
        else
          if t != Type.BOOL then throw new TypeErrorException("'!' requires BOOL", tokenizer.lineNo())
          return Unary(UnaryOp.Not, inner, Some(Type.BOOL))
      val first = parseExprInternal()
      if tokenizer.peekAtKind() != edu.utexas.cs.sam.io.Tokenizer.TokenType.OPERATOR then throw new SyntaxErrorException("Expected operator/?/) after expression", tokenizer.lineNo())
      if CompilerUtils.check(tokenizer, ')') then return first
      if CompilerUtils.check(tokenizer, '?') then
        val thenE = parseExprInternal(); CompilerUtils.expectChar(tokenizer, ':', tokenizer.lineNo()); val elseE = parseExprInternal()
        val tt = IdiomaticTypeUtils.typeOf(thenE, method, programSymbols); val et = IdiomaticTypeUtils.typeOf(elseE, method, programSymbols)
        if !tt.isCompatibleWith(et) then throw new TypeErrorException("Ternary branch type mismatch", tokenizer.lineNo())
        CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo())
        return Ternary(first, thenE, elseE, Some(tt))
      val op = CompilerUtils.getOp(tokenizer); val second = parseExprInternal(); CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo())
      return buildBinary(first, op, second)
    var base = parseTerminal()
    var dotCount = 0
    while CompilerUtils.check(tokenizer, '.') do
      if dotCount > 0 then throw new SyntaxErrorException("Inappropriate method/field chaining", tokenizer.lineNo())
      dotCount += 1
      val ident = CompilerUtils.getIdentifier(tokenizer)
      if CompilerUtils.check(tokenizer, '(') then
        // Explicit method calls on 'this' are not allowed in LO-3 tests
        base match
          case This(_) => throw new SyntaxErrorException("Method call on 'this' is not allowed", tokenizer.lineNo())
          case _ => ()
        val args = ListBuffer.empty[Expr]
        if !CompilerUtils.check(tokenizer, ')') then
          args += parseExprInternal()
          while CompilerUtils.check(tokenizer, ',') do args += parseExprInternal()
          CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo())
        var ms: MethodSymbol = null
        val className = IdiomaticTypeUtils.classNameOf(base, method, programSymbols)
        if className != null && programSymbols != null then
          val cs = programSymbols.getClass(className)
          if cs != null then ms = cs.getMethod(ident)
        val labelName = if className != null then s"${className}_${ident}" else ident
        val retType = if ms != null then ms.getReturnType else Type.INT
        val callable: assignment3.ast.CallableMethod =
          if ms != null then new assignment3.ast.ScalaInstanceCallable(className, ms)
          else new assignment3.ast.ScalaInstanceCallableFallback(
            labelName,
            if retType == null then null else assignment3.ValueType.ofPrimitive(retType),
            args.size + 1
          )
        base = InstanceCall(base, callable, args.toList)
      else
        val fi = if programSymbols != null then resolveFieldInfo(base, ident) else null
        base = FieldAccess(base, ident, fi)
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
        var sym = method.lookup(w)
        if sym == null then sym = method.lookupMethodGlobal(w)
        if sym == null then
          val implicitF = tryImplicitThisField(w)
          if implicitF != null then return implicitF
          throw new SyntaxErrorException("Undeclared symbol: " + w, tokenizer.lineNo())
        sym match
          case ms: MethodSymbol =>
            CompilerUtils.expectChar(tokenizer, '(', tokenizer.lineNo())
            val args = ListBuffer.empty[Expr]
            if !CompilerUtils.check(tokenizer, ')') then
              args += parseExprInternal()
              while CompilerUtils.check(tokenizer, ',') do args += parseExprInternal()
              CompilerUtils.expectChar(tokenizer, ')', tokenizer.lineNo())
            Call(new assignment3.ast.SymbolCallableMethod(ms), args.toList)
          case vs: VarSymbol if method.isInstanceOf[NewMethodContext] =>
            Var(vs.getName)
          case other => throw new SyntaxErrorException("Unsupported symbol type for variable expression: " + other.getClass.getSimpleName, tokenizer.lineNo())
      case _ => throw new SyntaxErrorException("Unexpected token in expression", tokenizer.lineNo())

  private def buildBinary(left: Expr, op: Char, right: Expr): Expr =
    val lt = IdiomaticTypeUtils.typeOf(left, method, programSymbols); val rt = IdiomaticTypeUtils.typeOf(right, method, programSymbols)
    if op == '*' && ((lt == Type.STRING && rt == Type.INT) || (lt == Type.INT && rt == Type.STRING)) then return Binary(BinaryOp.Mul, left, right, Some(Type.STRING))
    if op == '+' && lt == Type.STRING && rt == Type.STRING then return Binary(BinaryOp.Add, left, right, Some(Type.STRING))
    if (op == '<' || op == '>' || op == '=') && lt == Type.STRING && rt == Type.STRING then return Binary(op match
      case '<' => BinaryOp.Lt
      case '>' => BinaryOp.Gt
      case '=' => BinaryOp.Eq
    , left, right, Some(Type.BOOL))
    if op == '=' && lt.isCompatibleWith(rt) then return Binary(BinaryOp.Eq, left, right, Some(Type.BOOL))
    if op == '&' || op == '|' then
      if lt != Type.BOOL || rt != Type.BOOL then throw new TypeErrorException("Logical op requires BOOL operands", tokenizer.lineNo())
      return Binary(if op == '&' then BinaryOp.And else BinaryOp.Or, left, right, Some(Type.BOOL))
    if op == '+' || op == '-' || op == '*' || op == '/' || op == '%' then
      if lt != Type.INT || rt != Type.INT then throw new TypeErrorException("Arithmetic op requires INT operands", tokenizer.lineNo())
      val bop = op match
        case '+' => BinaryOp.Add
        case '-' => BinaryOp.Sub
        case '*' => BinaryOp.Mul
        case '/' => BinaryOp.Div
        case '%' => BinaryOp.Mod
      return Binary(bop, left, right, Some(Type.INT))
    if op == '<' || op == '>' || op == '=' then
      if !lt.isCompatibleWith(rt) then throw new TypeErrorException("Comparison requires matching types", tokenizer.lineNo())
      val bop = if op == '<' then BinaryOp.Lt else if op == '>' then BinaryOp.Gt else BinaryOp.Eq
      return Binary(bop, left, right, Some(Type.BOOL))
    throw new TypeErrorException("Unsupported operator: " + op, tokenizer.lineNo())
