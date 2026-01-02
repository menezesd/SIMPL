package assignment3

import assignment3.ast._
import assignment3.ast.IdiomaticCodegen
import assignment3.symbol.{MethodSymbol, ProgramSymbols, VarSymbol}
import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class CodegenTest {
  private val emptyCtx = IdiomaticCodegen.Ctx(None, None)

  @Test
  def testEmitIntLit(): Unit = {
    val result = IdiomaticCodegen.emitExprD(IntLit(42), emptyCtx)
    assertTrue(result.isRight, s"Expected Right but got $result")
    val code = result.getOrElse(sys.error("unexpected"))
    assertTrue(code.toString.contains("PUSHIMM 42"), s"Expected PUSHIMM 42, got: ${code.toString}")
  }

  @Test
  def testEmitBoolLitTrue(): Unit = {
    val result = IdiomaticCodegen.emitExprD(BoolLit(true), emptyCtx)
    assertTrue(result.isRight)
    val code = result.getOrElse(sys.error("unexpected"))
    assertTrue(code.toString.contains("PUSHIMM 1"), s"Expected PUSHIMM 1, got: ${code.toString}")
  }

  @Test
  def testEmitBoolLitFalse(): Unit = {
    val result = IdiomaticCodegen.emitExprD(BoolLit(false), emptyCtx)
    assertTrue(result.isRight)
    val code = result.getOrElse(sys.error("unexpected"))
    assertTrue(code.toString.contains("PUSHIMM 0"), s"Expected PUSHIMM 0, got: ${code.toString}")
  }

  @Test
  def testEmitNullLit(): Unit = {
    val result = IdiomaticCodegen.emitExprD(NullLit(), emptyCtx)
    assertTrue(result.isRight)
    val code = result.getOrElse(sys.error("unexpected"))
    assertTrue(code.toString.contains("PUSHIMM 0"), s"Expected PUSHIMM 0 for null, got: ${code.toString}")
  }

  @Test
  def testEmitStrLit(): Unit = {
    val result = IdiomaticCodegen.emitExprD(StrLit("hello"), emptyCtx)
    assertTrue(result.isRight)
    val code = result.getOrElse(sys.error("unexpected"))
    assertTrue(code.toString.contains("hello"), s"Expected string content, got: ${code.toString}")
  }

  @Test
  def testEmitVarWithoutFrame(): Unit = {
    // Should fail without a frame context
    val result = IdiomaticCodegen.emitExprD(Var("x"), emptyCtx)
    assertTrue(result.isLeft, "Expected error when emitting Var without frame")
  }

  @Test
  def testEmitVarWithFrame(): Unit = {
    val varSym = VarSymbol("x", ValueType.ofPrimitive(Type.INT), parameter = true, index = 0)
    val method = MethodSymbol("test", Vector(varSym), Vector.empty, None)
    val frame = new SymbolMethodFrame(method)
    val ctx = IdiomaticCodegen.Ctx(Some(frame), None)

    val result = IdiomaticCodegen.emitExprD(Var("x"), ctx)
    assertTrue(result.isRight, s"Expected Right but got $result")
  }

  @Test
  def testEmitThisWithoutFrame(): Unit = {
    val result = IdiomaticCodegen.emitExprD(This(), emptyCtx)
    assertTrue(result.isLeft, "Expected error when emitting This without frame")
  }

  @Test
  def testEmitBlockEmpty(): Unit = {
    val result = IdiomaticCodegen.emitStmtD(Block(Nil), emptyCtx)
    assertTrue(result.isRight)
    val code = result.getOrElse(sys.error("unexpected"))
    assertEquals("", code.toString.trim)
  }

  @Test
  def testEmitVarDeclWithInit(): Unit = {
    val decl = VarDecl("x", Some(Type.INT), Some(ValueType.ofPrimitive(Type.INT)), Some(IntLit(42)))
    val result = IdiomaticCodegen.emitStmtD(decl, emptyCtx)
    assertTrue(result.isRight)
    val code = result.getOrElse(sys.error("unexpected"))
    assertTrue(code.toString.contains("PUSHIMM 42"))
  }

  @Test
  def testEmitVarDeclWithoutInit(): Unit = {
    val decl = VarDecl("x", Some(Type.INT), Some(ValueType.ofPrimitive(Type.INT)), None)
    val result = IdiomaticCodegen.emitStmtD(decl, emptyCtx)
    assertTrue(result.isRight)
    val code = result.getOrElse(sys.error("unexpected"))
    // Should push null/0 for uninitialized
    assertTrue(code.toString.contains("PUSHIMM 0"))
  }

  @Test
  def testEmitBreakOutsideLoop(): Unit = {
    val result = IdiomaticCodegen.emitStmtD(Break(), emptyCtx)
    assertTrue(result.isLeft, "Break outside loop should fail")
  }

  @Test
  def testEmitBreakInsideLoop(): Unit = {
    val endLabel = new Label()
    val ctx = emptyCtx.copy(loopEndLabels = List(endLabel))
    val result = IdiomaticCodegen.emitStmtD(Break(), ctx)
    assertTrue(result.isRight, s"Break inside loop should succeed, got: $result")
  }

  @Test
  def testEmitReturnWithoutLabel(): Unit = {
    val result = IdiomaticCodegen.emitStmtD(Return(Some(IntLit(0))), emptyCtx)
    assertTrue(result.isLeft, "Return without return label should fail")
  }

  @Test
  def testEmitReturnWithLabel(): Unit = {
    val retLabel = new Label()
    val ctx = emptyCtx.copy(returnLabelOpt = Some(retLabel))
    val result = IdiomaticCodegen.emitStmtD(Return(Some(IntLit(42))), ctx)
    assertTrue(result.isRight, s"Return with label should succeed, got: $result")
  }
}
