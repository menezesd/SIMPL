package assignment3

import assignment3.ast.{Diag, Result, SyntaxDiag}
import assignment3.ast.Result._  // Import extension methods
import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class ResultTest {
  private def diag(msg: String): Diag = SyntaxDiag(msg, 1)

  @Test
  def testOk(): Unit = {
    val r = Result.ok(42)
    assertTrue(r.isRight)
    assertEquals(42, r.getOrElse(0))
  }

  @Test
  def testErr(): Unit = {
    val r: Result[Int] = Result.err(diag("error"))
    assertTrue(r.isLeft)
  }

  @Test
  def testUnit(): Unit = {
    val r = Result.unit
    assertTrue(r.isRight)
    assertEquals((), r.getOrElse(sys.error("unexpected")))
  }

  @Test
  def testSequenceEAllSuccess(): Unit = {
    var count = 0
    val items = List(1, 2, 3)
    val result = Result.sequenceE(items) { i =>
      count += 1
      Result.ok(())
    }
    assertTrue(result.isRight)
    assertEquals(3, count)
  }

  @Test
  def testSequenceEShortCircuits(): Unit = {
    var count = 0
    val items = List(1, 2, 3)
    val result = Result.sequenceE(items) { i =>
      count += 1
      if i == 2 then Result.err(diag("fail at 2"))
      else Result.ok(())
    }
    assertTrue(result.isLeft)
    assertEquals(2, count)  // Should stop at 2
  }

  @Test
  def testTraverseEAllSuccess(): Unit = {
    val items = List(1, 2, 3)
    val result = Result.traverseE(items)(i => Result.ok(i * 2))
    assertTrue(result.isRight)
    assertEquals(List(2, 4, 6), result.getOrElse(Nil))
  }

  @Test
  def testTraverseEShortCircuits(): Unit = {
    val items = List(1, 2, 3)
    val result = Result.traverseE(items) { i =>
      if i == 2 then Result.err(diag("fail"))
      else Result.ok(i * 2)
    }
    assertTrue(result.isLeft)
  }

  @Test
  def testFoldE(): Unit = {
    val items = List(1, 2, 3)
    val result = Result.foldE(items, 0)((acc, i) => Result.ok(acc + i))
    assertTrue(result.isRight)
    assertEquals(6, result.getOrElse(0))
  }

  @Test
  def testFoldEShortCircuits(): Unit = {
    val items = List(1, 2, 3)
    val result = Result.foldE(items, 0) { (acc, i) =>
      if i == 2 then Result.err(diag("fail"))
      else Result.ok(acc + i)
    }
    assertTrue(result.isLeft)
  }

  @Test
  def testFromOptionSome(): Unit = {
    val result = Result.fromOption(Some(42), diag("none"))
    assertTrue(result.isRight)
    assertEquals(42, result.getOrElse(0))
  }

  @Test
  def testFromOptionNone(): Unit = {
    val result = Result.fromOption(None, diag("none"))
    assertTrue(result.isLeft)
  }

  @Test
  def testRequireTrue(): Unit = {
    val result = Result.require(true, diag("fail"))
    assertTrue(result.isRight)
  }

  @Test
  def testRequireFalse(): Unit = {
    val result = Result.require(false, diag("fail"))
    assertTrue(result.isLeft)
  }

  @Test
  def testRecover(): Unit = {
    val okResult: Result[Int] = Result.ok(42)
    val errResult: Result[Int] = Result.err(diag("fail"))
    assertEquals(42, okResult.recover(0))
    assertEquals(0, errResult.recover(0))
  }

  @Test
  def testToOpt(): Unit = {
    val okResult: Result[Int] = Result.ok(42)
    val errResult: Result[Int] = Result.err(diag("fail"))
    assertEquals(Some(42), okResult.toOpt)
    assertEquals(None, errResult.toOpt)
  }

  @Test
  def testZip(): Unit = {
    val r1 = Result.ok(1)
    val r2 = Result.ok("a")
    val result = Result.zip(r1, r2)
    assertTrue(result.isRight)
    assertEquals((1, "a"), result.getOrElse(sys.error("unexpected")))
  }

  @Test
  def testZipWithError(): Unit = {
    val r1: Result[Int] = Result.ok(1)
    val r2: Result[String] = Result.err(diag("fail"))
    val result = Result.zip(r1, r2)
    assertTrue(result.isLeft)
  }

  @Test
  def testZip3(): Unit = {
    val r1 = Result.ok(1)
    val r2 = Result.ok("a")
    val r3 = Result.ok(true)
    val result = Result.zip3(r1, r2, r3)
    assertTrue(result.isRight)
    assertEquals((1, "a", true), result.getOrElse(sys.error("unexpected")))
  }

  @Test
  def testValidateAllSuccess(): Unit = {
    val items = List(2, 4, 6)
    val result = Result.validateAll(items)(_ % 2 == 0, i => diag(s"$i is odd"))
    assertTrue(result.isRight)
  }

  @Test
  def testValidateAllFailure(): Unit = {
    val items = List(2, 3, 6)
    val result = Result.validateAll(items)(_ % 2 == 0, i => diag(s"$i is odd"))
    assertTrue(result.isLeft)
  }

  @Test
  def testEnsureSuccess(): Unit = {
    val result = Result.ensure(Result.ok(42))(_ > 0, diag("must be positive"))
    assertTrue(result.isRight)
    assertEquals(42, result.getOrElse(0))
  }

  @Test
  def testEnsureFailure(): Unit = {
    val result = Result.ensure(Result.ok(-1))(_ > 0, diag("must be positive"))
    assertTrue(result.isLeft)
  }

  @Test
  def testMapErr(): Unit = {
    val result: Result[Int] = Result.err(diag("original"))
    val mapped = result.mapErr(d => SyntaxDiag("mapped: " + d.message, d.line))
    assertTrue(mapped.isLeft)
    mapped.left.foreach(d => assertTrue(d.message.startsWith("mapped:")))
  }

  @Test
  def testOrElseResult(): Unit = {
    val err: Result[Int] = Result.err(diag("first"))
    val fallback = Result.ok(42)
    val result = err.orElseResult(fallback)
    assertTrue(result.isRight)
    assertEquals(42, result.getOrElse(0))
  }
}
