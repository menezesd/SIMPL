package assignment3.ast

import assignment3.symbol.{MethodSymbol, ProgramSymbols}

object AstEither:
  // Adapters for existing call sites expecting CompilerException
  def toCE[L <: Diag, R](e: Either[L, R]): Either[assignment3.CompilerException, R] = e.left.map(Diag.toCompilerException)

  /** Resolve the class name of an expression as Either[Diag, String]. */
  def resolveClassNameD(e: Expr, method: MethodContext, ps: ProgramSymbols, line: Int): Either[Diag, String] =
    IdiomaticTypeUtils.classNameOf(e, method, ps)
      .toRight(TypeDiag("Unable to resolve class for expression", line))

  /** Resolve a method symbol on the class of an expression, or produce a Diag error. */
  def resolveMethodOnExprD(e: Expr, methodName: String, method: MethodContext, ps: ProgramSymbols, line: Int): Either[Diag, MethodSymbol] =
    for
      cn <- resolveClassNameD(e, method, ps, line)
      cs <- ps.getClass(cn).toRight(ResolveDiag(s"Unknown class '$cn'", line))
      ms <- cs.getMethod(methodName).toRight(ResolveDiag(s"Unknown method '$methodName' on class '$cn'", line))
    yield ms

  /** Resolve field info on a target expression. */
  def resolveFieldInfoD(target: Expr, fieldName: String, method: MethodContext, ps: ProgramSymbols, line: Int): Either[Diag, assignment3.symbol.ClassSymbol.FieldInfo] =
    for
      cn <- resolveClassNameD(target, method, ps, line)
      cs <- ps.getClass(cn).toRight(ResolveDiag(s"Unknown class '$cn'", line))
      fi <- cs.getFieldInfo(fieldName).toRight(ResolveDiag(s"Unknown field '$fieldName' on class '$cn'", line))
    yield fi

  // Backwards-compat: CE-typed variants delegating to Diag and adapting the Left
  def resolveClassName(e: Expr, method: MethodContext, ps: ProgramSymbols, line: Int) = toCE(resolveClassNameD(e, method, ps, line))
  def resolveMethodOnExpr(e: Expr, methodName: String, method: MethodContext, ps: ProgramSymbols, line: Int) = toCE(resolveMethodOnExprD(e, methodName, method, ps, line))
  def resolveFieldInfo(target: Expr, fieldName: String, method: MethodContext, ps: ProgramSymbols, line: Int) = toCE(resolveFieldInfoD(target, fieldName, method, ps, line))

  /** Type-check and build a binary expression with diagnostic result. Mirrors AstParser.buildBinary logic. */
  def buildBinaryD(left: Expr, op: Char, right: Expr, method: MethodContext, ps: ProgramSymbols, line: Int): Either[Diag, Expr] =
    val lt = IdiomaticTypeUtils.typeOf(left, method, ps); val rt = IdiomaticTypeUtils.typeOf(right, method, ps)
    // String-repeat and concat special forms
    if op == '*' && ((lt == assignment3.Type.STRING && rt == assignment3.Type.INT) || (lt == assignment3.Type.INT && rt == assignment3.Type.STRING)) then
      Right(Binary(BinaryOp.Mul, left, right, Some(assignment3.Type.STRING)))
    else if op == '+' && lt == assignment3.Type.STRING && rt == assignment3.Type.STRING then
      Right(Binary(BinaryOp.Add, left, right, Some(assignment3.Type.STRING)))
    else if (op == '<' || op == '>' || op == '=') && lt == assignment3.Type.STRING && rt == assignment3.Type.STRING then
      val bop = op match
        case '<' => BinaryOp.Lt
        case '>' => BinaryOp.Gt
        case '=' => BinaryOp.Eq
      Right(Binary(bop, left, right, Some(assignment3.Type.BOOL)))
    else if op == '=' && lt.isCompatibleWith(rt) then
      Right(Binary(BinaryOp.Eq, left, right, Some(assignment3.Type.BOOL)))
    else if op == '&' || op == '|' then
      if lt != assignment3.Type.BOOL || rt != assignment3.Type.BOOL then Left(TypeDiag("Logical op requires BOOL operands", line))
      else Right(Binary(if op == '&' then BinaryOp.And else BinaryOp.Or, left, right, Some(assignment3.Type.BOOL)))
    else if op == '+' || op == '-' || op == '*' || op == '/' || op == '%' then
      if lt != assignment3.Type.INT || rt != assignment3.Type.INT then Left(TypeDiag("Arithmetic op requires INT operands", line))
      else
        val bop = op match
          case '+' => BinaryOp.Add
          case '-' => BinaryOp.Sub
          case '*' => BinaryOp.Mul
          case '/' => BinaryOp.Div
          case '%' => BinaryOp.Mod
        Right(Binary(bop, left, right, Some(assignment3.Type.INT)))
    else if op == '<' || op == '>' || op == '=' then
      if !lt.isCompatibleWith(rt) then Left(TypeDiag("Comparison requires matching types", line))
      else
        val bop = if op == '<' then BinaryOp.Lt else if op == '>' then BinaryOp.Gt else BinaryOp.Eq
        Right(Binary(bop, left, right, Some(assignment3.Type.BOOL)))
    else Left(TypeDiag("Unsupported operator: " + op, line))

  /** Type-check and build a unary expression for '~' or '!' operators with diagnostic result. */
  def buildUnaryD(op: Char, inner: Expr, method: MethodContext, ps: ProgramSymbols, line: Int): Either[Diag, Expr] =
    val t = IdiomaticTypeUtils.typeOf(inner, method, ps)
    if op == '~' then
      if t != assignment3.Type.INT && t != assignment3.Type.STRING then Left(TypeDiag("'~' requires INT or STRING", line))
      else Right(Unary(UnaryOp.Neg, inner, Some(if t == assignment3.Type.STRING then assignment3.Type.STRING else assignment3.Type.INT)))
    else if op == '!' then
      if t != assignment3.Type.BOOL then Left(TypeDiag("'!' requires BOOL", line))
      else Right(Unary(UnaryOp.Not, inner, Some(assignment3.Type.BOOL)))
    else Left(SyntaxDiag("Unsupported unary operator: " + op, line))

  /** Type-check and build a ternary expression with diagnostic result. */
  def buildTernaryD(cond: Expr, thenE: Expr, elseE: Expr, method: MethodContext, ps: ProgramSymbols, line: Int): Either[Diag, Expr] =
    val tt = IdiomaticTypeUtils.typeOf(thenE, method, ps)
    val et = IdiomaticTypeUtils.typeOf(elseE, method, ps)
    if !tt.isCompatibleWith(et) then Left(TypeDiag("Ternary branch type mismatch", line))
    else Right(Ternary(cond, thenE, elseE, Some(tt)))
