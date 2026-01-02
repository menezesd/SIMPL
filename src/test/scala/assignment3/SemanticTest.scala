package assignment3

import assignment3.ast._
import assignment3.ast.IdiomaticSemantic
import assignment3.symbol.{ClassSymbol, MethodSymbol, ProgramSymbols, VarSymbol}
import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class SemanticTest {
  private val emptySymbols = ProgramSymbols.empty()

  private def method(vars: Vector[VarSymbol] = Vector.empty, returnType: Option[ValueType] = None): MethodSymbol =
    MethodSymbol("test", vars, Vector.empty, returnType)

  @Test
  def testCheckIntLit(): Unit = {
    val result = IdiomaticSemantic.checkExprE(IntLit(42), method(), 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckBoolLit(): Unit = {
    val result = IdiomaticSemantic.checkExprE(BoolLit(true), method(), 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckStrLit(): Unit = {
    val result = IdiomaticSemantic.checkExprE(StrLit("hello"), method(), 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckNullLit(): Unit = {
    val result = IdiomaticSemantic.checkExprE(NullLit(), method(), 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckVar(): Unit = {
    val result = IdiomaticSemantic.checkExprE(Var("x"), method(), 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckUnary(): Unit = {
    val expr = Unary(UnaryOp.Not, BoolLit(true))
    val result = IdiomaticSemantic.checkExprE(expr, method(), 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckBinary(): Unit = {
    val expr = Binary(BinaryOp.Add, IntLit(1), IntLit(2))
    val result = IdiomaticSemantic.checkExprE(expr, method(), 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckTernary(): Unit = {
    val expr = Ternary(BoolLit(true), IntLit(1), IntLit(2))
    val result = IdiomaticSemantic.checkExprE(expr, method(), 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckInstanceCallOnNull(): Unit = {
    val callable = ScalaCallableMethod.fallback("foo", None, 0)
    val expr = InstanceCall(NullLit(), callable, Nil)
    val result = IdiomaticSemantic.checkExprE(expr, method(), 1, emptySymbols)
    assertTrue(result.isLeft, "Instance call on null should fail")
  }

  @Test
  def testCheckInstanceCallOnThis(): Unit = {
    val callable = ScalaCallableMethod.fallback("foo", None, 0)
    val expr = InstanceCall(This(), callable, Nil)
    val result = IdiomaticSemantic.checkExprE(expr, method(), 1, emptySymbols)
    assertTrue(result.isLeft, "Instance call on 'this' should fail")
  }

  @Test
  def testCheckFieldAccessOnNull(): Unit = {
    val expr = FieldAccess(NullLit(), "x")
    val result = IdiomaticSemantic.checkExprE(expr, method(), 1, emptySymbols)
    assertTrue(result.isLeft, "Field access on null should fail")
  }

  @Test
  def testCheckBlockStmt(): Unit = {
    val block = Block(List(Return(Some(IntLit(0)))))
    val m = method(returnType = Some(ValueType.ofPrimitive(Type.INT)))
    val result = IdiomaticSemantic.checkStmtE(block, m, 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckIfWithBoolCondition(): Unit = {
    val stmt = If(BoolLit(true), Block(Nil), Block(Nil))
    val result = IdiomaticSemantic.checkStmtE(stmt, method(), 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckIfWithNonBoolCondition(): Unit = {
    val stmt = If(IntLit(1), Block(Nil), Block(Nil))
    val result = IdiomaticSemantic.checkStmtE(stmt, method(), 1, emptySymbols)
    assertTrue(result.isLeft, "If with non-bool condition should fail")
  }

  @Test
  def testCheckWhileWithBoolCondition(): Unit = {
    val stmt = While(BoolLit(true), Block(Nil))
    val result = IdiomaticSemantic.checkStmtE(stmt, method(), 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckWhileWithNonBoolCondition(): Unit = {
    val stmt = While(IntLit(1), Block(Nil))
    val result = IdiomaticSemantic.checkStmtE(stmt, method(), 1, emptySymbols)
    assertTrue(result.isLeft, "While with non-bool condition should fail")
  }

  @Test
  def testCheckBreak(): Unit = {
    val result = IdiomaticSemantic.checkStmtE(Break(), method(), 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckReturnWithValueForNonVoid(): Unit = {
    val m = method(returnType = Some(ValueType.ofPrimitive(Type.INT)))
    val stmt = Return(Some(IntLit(42)))
    val result = IdiomaticSemantic.checkStmtE(stmt, m, 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckReturnWithoutValueForVoid(): Unit = {
    val m = method(returnType = None)  // None means void
    val stmt = Return(None)
    val result = IdiomaticSemantic.checkStmtE(stmt, m, 1, emptySymbols)
    assertTrue(result.isRight)
  }

  @Test
  def testCheckReturnWithValueForVoid(): Unit = {
    val m = method(returnType = None)  // None means void
    val stmt = Return(Some(IntLit(42)))
    val result = IdiomaticSemantic.checkStmtE(stmt, m, 1, emptySymbols)
    assertTrue(result.isLeft, "Return with value for void method should fail")
  }

  @Test
  def testCheckReturnTypeMismatch(): Unit = {
    val m = method(returnType = Some(ValueType.ofPrimitive(Type.BOOL)))
    val stmt = Return(Some(IntLit(42)))  // returning int, expected bool
    val result = IdiomaticSemantic.checkStmtE(stmt, m, 1, emptySymbols)
    assertTrue(result.isLeft, "Return type mismatch should fail")
  }

  @Test
  def testCheckFieldAssignOnNull(): Unit = {
    val stmt = FieldAssign(NullLit(), "x", 0, IntLit(1))
    val result = IdiomaticSemantic.checkStmtE(stmt, method(), 1, emptySymbols)
    assertTrue(result.isLeft, "Field assign on null should fail")
  }
}
