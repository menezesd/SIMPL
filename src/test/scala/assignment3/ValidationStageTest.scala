package assignment3

import assignment3.symbol.{ClassSymbol, MethodSymbol, ProgramSymbols, VarSymbol}
import org.junit.jupiter.api.Assertions.{assertEquals, assertTrue}
import org.junit.jupiter.api.Test

class ValidationStageTest {
  private def programWithMain(method: MethodSymbol): ProgramSymbols = {
    val cls = ClassSymbol("Main", Vector.empty, Map("main" -> method), Vector.empty)
    ProgramSymbols(Map("Main" -> cls))
  }

  private def thisParam(className: String): VarSymbol =
    VarSymbol("this", ValueType.ofObject(className), parameter = true, index = 0)

  @Test
  def testMissingEntrypoint(): Unit = {
    val symbols = ProgramSymbols.empty()
    val res = ValidationStage.validateEntrypointD(symbols, "Main", "main")
    assertTrue(res.isLeft)
    val diag = res.swap.getOrElse(sys.error("expected Left"))
    assertEquals(Messages.missingEntrypoint("Main", "main"), diag.message)
  }

  @Test
  def testRejectsUserParams(): Unit = {
    val params = Vector(thisParam("Main"), VarSymbol("x", ValueType.ofPrimitive(Type.INT), parameter = true, index = 1))
    val method = MethodSymbol("main", params, Vector.empty, None)
    val res = ValidationStage.validateEntrypointD(programWithMain(method), "Main", "main")
    assertTrue(res.isLeft)
    val diag = res.swap.getOrElse(sys.error("expected Left"))
    assertEquals(Messages.entrypointNoParams("Main", "main"), diag.message)
  }

  @Test
  def testRejectsNonInstance(): Unit = {
    val params = Vector(thisParam("Other"))
    val method = MethodSymbol("main", params, Vector.empty, None)
    val res = ValidationStage.validateEntrypointD(programWithMain(method), "Main", "main")
    assertTrue(res.isLeft)
    val diag = res.swap.getOrElse(sys.error("expected Left"))
    assertEquals(Messages.entrypointMustBeInstance("Main", "main"), diag.message)
  }

  @Test
  def testAcceptsValidEntrypoint(): Unit = {
    val params = Vector(thisParam("Main"))
    val method = MethodSymbol("main", params, Vector.empty, None)
    val res = ValidationStage.validateEntrypointD(programWithMain(method), "Main", "main")
    assertTrue(res.isRight)
  }
}
