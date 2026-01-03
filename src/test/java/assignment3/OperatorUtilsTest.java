package assignment3;

import org.junit.jupiter.api.Test;
import scala.util.Either;

import static org.junit.jupiter.api.Assertions.*;

public class OperatorUtilsTest {

    @Test
    void unaryNegEmitsTimesWithMinusOne() {
        Either<String, String> result = OperatorUtils.getUnopE('~');
        assertTrue(result.isRight());
        assertEquals("PUSHIMM -1\nTIMES\n", result.getOrElse(() -> ""));
    }

    @Test
    void unaryNotEmitsAddOneModTwo() {
        Either<String, String> result = OperatorUtils.getUnopE('!');
        assertTrue(result.isRight());
        assertEquals("PUSHIMM 1\nADD\nPUSHIMM 2\nMOD\n", result.getOrElse(() -> ""));
    }

    @Test
    void binopAdd() {
        Either<String, String> result = OperatorUtils.getBinopE('+');
        assertTrue(result.isRight());
        assertEquals("ADD\n", result.getOrElse(() -> ""));
    }

    @Test
    void binopMul() {
        Either<String, String> result = OperatorUtils.getBinopE('*');
        assertTrue(result.isRight());
        assertEquals("TIMES\n", result.getOrElse(() -> ""));
    }

    @Test
    void binopCmp() {
        Either<String, String> resultGt = OperatorUtils.getBinopE('>');
        assertTrue(resultGt.isRight());
        assertEquals("GREATER\n", resultGt.getOrElse(() -> ""));

        Either<String, String> resultLt = OperatorUtils.getBinopE('<');
        assertTrue(resultLt.isRight());
        assertEquals("LESS\n", resultLt.getOrElse(() -> ""));

        Either<String, String> resultEq = OperatorUtils.getBinopE('=');
        assertTrue(resultEq.isRight());
        assertEquals("EQUAL\n", resultEq.getOrElse(() -> ""));
    }
}
