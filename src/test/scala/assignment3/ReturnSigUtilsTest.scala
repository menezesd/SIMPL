package assignment3

import assignment3.ast.high.{ReturnSig, ReturnSigUtils}
import org.junit.jupiter.api.Assertions.{assertEquals, assertTrue}
import org.junit.jupiter.api.Test

class ReturnSigUtilsTest {
  @Test
  def testFromRawType(): Unit = {
    assertEquals(ReturnSig.Void, ReturnSigUtils.fromRawType("void", 1))
    assertEquals(ReturnSig.Prim(Type.INT), ReturnSigUtils.fromRawType("int", 1))
    assertEquals(ReturnSig.Obj("Widget"), ReturnSigUtils.fromRawType("Widget", 1))
  }

  @Test
  def testToValueTypeOpt(): Unit = {
    assertTrue(ReturnSigUtils.toValueTypeOpt(ReturnSig.Void).isEmpty)
    assertEquals(Some(ValueType.ofPrimitive(Type.INT)), ReturnSigUtils.toValueTypeOpt(ReturnSig.Prim(Type.INT)))
    assertEquals(Some(ValueType.ofObject("Widget")), ReturnSigUtils.toValueTypeOpt(ReturnSig.Obj("Widget")))
  }
}
