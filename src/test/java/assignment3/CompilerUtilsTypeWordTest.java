package assignment3;

import assignment3.symbol.ProgramSymbols;
import edu.utexas.cs.sam.io.SamTokenizer;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class CompilerUtilsTypeWordTest {
    @Test
    @DisplayName("isTypeWord: excludes statement starters when requested")
    void testExcludesStatementStarters() throws Exception {
        SamTokenizer tz = new SamTokenizer(new java.io.StringReader("return"), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
        assertFalse(CompilerUtils.isTypeWord(
                tz,
                new ProgramSymbols(scala.collection.immutable.Map$.MODULE$.<String, assignment3.symbol.ClassSymbol>empty()),
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
        new ProgramSymbols(scala.collection.immutable.Map$.MODULE$.<String, assignment3.symbol.ClassSymbol>empty()),
        null,
        false,
        true
    ));

        SamTokenizer tz2 = new SamTokenizer(new java.io.StringReader("bool"), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
    assertTrue(CompilerUtils.isTypeWord(
        tz2,
        new ProgramSymbols(scala.collection.immutable.Map$.MODULE$.<String, assignment3.symbol.ClassSymbol>empty()),
        null,
        false,
        true
    ));

        SamTokenizer tz3 = new SamTokenizer(new java.io.StringReader("String"), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
    assertTrue(CompilerUtils.isTypeWord(
        tz3,
        new ProgramSymbols(scala.collection.immutable.Map$.MODULE$.<String, assignment3.symbol.ClassSymbol>empty()),
        null,
        false,
        true
    ));
    }

    @Test
    @DisplayName("isTypeWord: recognizes class names including current class")
    void testRecognizesClassNames() throws Exception {
    // Build immutable ProgramSymbols with classes C and Main
    scala.collection.immutable.Map<String, assignment3.symbol.ClassSymbol> m =
        scala.collection.immutable.Map$.MODULE$.empty();
    assignment3.symbol.ClassSymbol clsC = new assignment3.symbol.ClassSymbol(
        "C",
        scala.collection.immutable.Vector$.MODULE$.<assignment3.symbol.VarSymbol>empty(),
        scala.collection.immutable.Map$.MODULE$.<String, assignment3.symbol.MethodSymbol>empty(),
        scala.collection.immutable.Vector$.MODULE$.<String>empty()
    );
    assignment3.symbol.ClassSymbol clsMain = new assignment3.symbol.ClassSymbol(
        "Main",
        scala.collection.immutable.Vector$.MODULE$.<assignment3.symbol.VarSymbol>empty(),
        scala.collection.immutable.Map$.MODULE$.<String, assignment3.symbol.MethodSymbol>empty(),
        scala.collection.immutable.Vector$.MODULE$.<String>empty()
    );
    m = m.updated("C", clsC);
    m = m.updated("Main", clsMain);
    ProgramSymbols symbols = new ProgramSymbols(m);

        SamTokenizer tzC = new SamTokenizer(new java.io.StringReader("C"), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
        assertTrue(CompilerUtils.isTypeWord(tzC, symbols, null, false, true));

        SamTokenizer tzMain = new SamTokenizer(new java.io.StringReader("Main"), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
        assertTrue(CompilerUtils.isTypeWord(tzMain, symbols, "Main", false, true));
    }
}
