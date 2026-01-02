package assignment3

import assignment3.ast._
import assignment3.ast.IdiomaticTypeUtils
import assignment3.symbol.{ClassSymbol, MethodSymbol, ProgramSymbols, VarSymbol}
import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class TypeUtilsTest {
  private val emptySymbols = ProgramSymbols.empty()

  private def methodCtx(vars: VarSymbol*): NewMethodContext = {
    val method = MethodSymbol("test", vars.toVector, Vector.empty, None)
    new NewMethodContext(method, emptySymbols)
  }

  @Test
  def testTypeOfIntLit(): Unit = {
    val ctx = methodCtx()
    assertEquals(Type.INT, IdiomaticTypeUtils.typeOf(IntLit(42), ctx, emptySymbols))
  }

  @Test
  def testTypeOfBoolLit(): Unit = {
    val ctx = methodCtx()
    assertEquals(Type.BOOL, IdiomaticTypeUtils.typeOf(BoolLit(true), ctx, emptySymbols))
  }

  @Test
  def testTypeOfStrLit(): Unit = {
    val ctx = methodCtx()
    assertEquals(Type.STRING, IdiomaticTypeUtils.typeOf(StrLit("hello"), ctx, emptySymbols))
  }

  @Test
  def testTypeOfNullLit(): Unit = {
    val ctx = methodCtx()
    // Null is lowered to INT (pointer representation)
    assertEquals(Type.INT, IdiomaticTypeUtils.typeOf(NullLit(), ctx, emptySymbols))
  }

  @Test
  def testTypeOfVar(): Unit = {
    val intVar = VarSymbol("x", ValueType.ofPrimitive(Type.INT), parameter = false, index = 0)
    val ctx = methodCtx(intVar)
    assertEquals(Type.INT, IdiomaticTypeUtils.typeOf(Var("x"), ctx, emptySymbols))
  }

  @Test
  def testTypeOfVarBool(): Unit = {
    val boolVar = VarSymbol("flag", ValueType.ofPrimitive(Type.BOOL), parameter = false, index = 0)
    val ctx = methodCtx(boolVar)
    assertEquals(Type.BOOL, IdiomaticTypeUtils.typeOf(Var("flag"), ctx, emptySymbols))
  }

  @Test
  def testTypeOfUnaryNot(): Unit = {
    val ctx = methodCtx()
    val expr = Unary(UnaryOp.Not, BoolLit(true))
    assertEquals(Type.BOOL, IdiomaticTypeUtils.typeOf(expr, ctx, emptySymbols))
  }

  @Test
  def testTypeOfUnaryNegInt(): Unit = {
    val ctx = methodCtx()
    val expr = Unary(UnaryOp.Neg, IntLit(5))
    assertEquals(Type.INT, IdiomaticTypeUtils.typeOf(expr, ctx, emptySymbols))
  }

  @Test
  def testTypeOfUnaryNegString(): Unit = {
    val ctx = methodCtx()
    val expr = Unary(UnaryOp.Neg, StrLit("abc"))
    assertEquals(Type.STRING, IdiomaticTypeUtils.typeOf(expr, ctx, emptySymbols))
  }

  @Test
  def testTypeOfBinaryWithResultType(): Unit = {
    val ctx = methodCtx()
    val expr = Binary(BinaryOp.Add, IntLit(1), IntLit(2), Some(Type.INT))
    assertEquals(Type.INT, IdiomaticTypeUtils.typeOf(expr, ctx, emptySymbols))
  }

  @Test
  def testTypeOfTernaryWithResultType(): Unit = {
    val ctx = methodCtx()
    val expr = Ternary(BoolLit(true), IntLit(1), IntLit(2), Some(Type.INT))
    assertEquals(Type.INT, IdiomaticTypeUtils.typeOf(expr, ctx, emptySymbols))
  }

  @Test
  def testTypeOfThis(): Unit = {
    val ctx = methodCtx()
    // 'this' is lowered to INT (object pointer)
    assertEquals(Type.INT, IdiomaticTypeUtils.typeOf(This(), ctx, emptySymbols))
  }

  @Test
  def testClassNameOfNewObject(): Unit = {
    val ctx = methodCtx()
    val expr = NewObject("Widget", Nil)
    assertEquals(Some("Widget"), IdiomaticTypeUtils.classNameOf(expr, ctx, emptySymbols))
  }

  @Test
  def testClassNameOfVarWithObjectType(): Unit = {
    val objVar = VarSymbol("w", ValueType.ofObject("Widget"), parameter = false, index = 0)
    val ctx = methodCtx(objVar)
    assertEquals(Some("Widget"), IdiomaticTypeUtils.classNameOf(Var("w"), ctx, emptySymbols))
  }

  @Test
  def testClassNameOfIntLitIsNone(): Unit = {
    val ctx = methodCtx()
    assertEquals(None, IdiomaticTypeUtils.classNameOf(IntLit(42), ctx, emptySymbols))
  }
}
