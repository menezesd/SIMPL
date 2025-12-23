package assignment3;

import edu.utexas.cs.sam.io.SamTokenizer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

class CompilerUtilsIdentifierTest {

    private SamTokenizer tokenizerFor(Path dir, String content) throws IOException {
        Path f = dir.resolve("id_test.lo");
        Files.writeString(f, content);
        return new SamTokenizer(f.toString(), SamTokenizer.TokenizerOptions.PROCESS_STRINGS);
    }

    @Test
    void accepts_valid_identifiers(@TempDir Path dir) throws Exception {
        SamTokenizer tz1 = tokenizerFor(dir, "foo");
        assertEquals("foo", CompilerUtils.getIdentifier(tz1, CompilerUtils.noRecorder()));

        SamTokenizer tz2 = tokenizerFor(dir, "a1_b2");
        assertEquals("a1_b2", CompilerUtils.getIdentifier(tz2, CompilerUtils.noRecorder()));
    }

    @Test
    void rejects_reserved_words(@TempDir Path dir) throws Exception {
        String[] reserved = {
                "class","if","else","while","return","break",
                "true","false","null","new","this","int","bool","String","void"
        };
        for (String w : reserved) {
            SamTokenizer tz = tokenizerFor(dir, w);
            SyntaxErrorException ex = assertThrows(SyntaxErrorException.class, () -> CompilerUtils.getIdentifier(tz, CompilerUtils.noRecorder()), w + " should be reserved");
            assertTrue(ex.getMessage().contains("Reserved word"));
        }
    }

    @Test
    void rejects_when_not_a_word(@TempDir Path dir) throws Exception {
        SamTokenizer tz = tokenizerFor(dir, "1abc");
        SyntaxErrorException ex = assertThrows(SyntaxErrorException.class, () -> CompilerUtils.getIdentifier(tz, CompilerUtils.noRecorder()));
        assertTrue(ex.getMessage().contains("Expected identifier"));
    }

    @Test
    void rejects_invalid_pattern_starting_underscore(@TempDir Path dir) throws Exception {
        SamTokenizer tz = tokenizerFor(dir, "_bad");
        SyntaxErrorException ex = assertThrows(SyntaxErrorException.class, () -> CompilerUtils.getIdentifier(tz, CompilerUtils.noRecorder()));
        // Depending on tokenizer, '_' may not be a WORD token. Accept either message.
        String msg = ex.getMessage();
        assertTrue(msg.contains("Invalid identifier") || msg.contains("Expected identifier"), msg);
    }
}
