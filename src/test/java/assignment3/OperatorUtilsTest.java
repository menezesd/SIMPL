package assignment3;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class OperatorUtilsTest {

    @Test
    void unaryNegEmitsTimesWithMinusOne() {
        String code = OperatorUtils.getUnop('~');
        assertEquals("PUSHIMM -1\nTIMES\n", code);
    }

    @Test
    void unaryNotEmitsAddOneModTwo() {
        String code = OperatorUtils.getUnop('!');
        assertEquals("PUSHIMM 1\nADD\nPUSHIMM 2\nMOD\n", code);
    }

    @Test
    void binopAdd() {
        assertEquals("ADD\n", OperatorUtils.getBinop('+'));
    }

    @Test
    void binopMul() {
        assertEquals("TIMES\n", OperatorUtils.getBinop('*'));
    }

    @Test
    void binopCmp() {
        assertEquals("GREATER\n", OperatorUtils.getBinop('>'));
        assertEquals("LESS\n", OperatorUtils.getBinop('<'));
        assertEquals("EQUAL\n", OperatorUtils.getBinop('='));
    }
}
