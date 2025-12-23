package assignment3;

import assignment3.symbol.ProgramSymbols;
import edu.utexas.cs.sam.io.SamTokenizer;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@SuppressWarnings("unchecked")
class CompilerUtilsTypeWordTest {
    @Test
    @DisplayName("isTypeWord: excludes statement starters when requested")
    void testExcludesStatementStarters() throws Exception {
        SamTokenizer tz = new SamTokenizer(new java.io.StringReader("return"), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
        assertFalse(CompilerUtils.isTypeWord(
                tz,
                ProgramSymbols.empty(),
                "Main",
                false,
                true
        ));
    }

    @Test
    @DisplayName("isTypeWord: recognizes primitive types")
    void testRecognizesPrimitiveTypes() throws Exception {
        SamTokenizer tz1 = new SamTokenizer(new java.io.StringReader("int"), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
    assertTrue(CompilerUtils.isTypeWord(
        tz1,
        ProgramSymbols.empty(),
        null,
        false,
        true
    ));

        SamTokenizer tz2 = new SamTokenizer(new java.io.StringReader("bool"), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
    assertTrue(CompilerUtils.isTypeWord(
        tz2,
        ProgramSymbols.empty(),
        null,
        false,
        true
    ));

        SamTokenizer tz3 = new SamTokenizer(new java.io.StringReader("String"), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
    assertTrue(CompilerUtils.isTypeWord(
        tz3,
        ProgramSymbols.empty(),
        null,
        false,
        true
    ));
    }

    @Test
    @DisplayName("isTypeWord: recognizes class names including current class")
    void testRecognizesClassNames() throws Exception {
        ProgramSymbols symbols = ProgramSymbols.withClassNames("C", "Main");

        SamTokenizer tzC = new SamTokenizer(new java.io.StringReader("C"), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
        assertTrue(CompilerUtils.isTypeWord(tzC, symbols, null, false, true));

        SamTokenizer tzMain = new SamTokenizer(new java.io.StringReader("Main"), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
        assertTrue(CompilerUtils.isTypeWord(tzMain, symbols, "Main", false, true));
    }
}
