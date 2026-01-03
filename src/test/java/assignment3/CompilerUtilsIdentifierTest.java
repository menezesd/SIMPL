package assignment3;

import assignment3.ast.Diag;
import edu.utexas.cs.sam.io.SamTokenizer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import scala.util.Either;

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
        Either<Diag, String> result1 = CompilerUtils.getIdentifierE(tz1, CompilerUtils.noRecorder());
        assertTrue(result1.isRight());
        assertEquals("foo", result1.getOrElse(() -> ""));

        SamTokenizer tz2 = tokenizerFor(dir, "a1_b2");
        Either<Diag, String> result2 = CompilerUtils.getIdentifierE(tz2, CompilerUtils.noRecorder());
        assertTrue(result2.isRight());
        assertEquals("a1_b2", result2.getOrElse(() -> ""));
    }

    @Test
    void rejects_reserved_words(@TempDir Path dir) throws Exception {
        String[] reserved = {
                "class","if","else","while","return","break",
                "true","false","null","new","this","int","bool","String","void"
        };
        for (String w : reserved) {
            SamTokenizer tz = tokenizerFor(dir, w);
            Either<Diag, String> result = CompilerUtils.getIdentifierE(tz, CompilerUtils.noRecorder());
            assertTrue(result.isLeft(), w + " should be reserved");
            Diag diag = result.swap().getOrElse(() -> null);
            assertNotNull(diag);
            assertTrue(diag.message().contains("Reserved word"), diag.message());
        }
    }

    @Test
    void rejects_when_not_a_word(@TempDir Path dir) throws Exception {
        SamTokenizer tz = tokenizerFor(dir, "1abc");
        Either<Diag, String> result = CompilerUtils.getIdentifierE(tz, CompilerUtils.noRecorder());
        assertTrue(result.isLeft());
        Diag diag = result.swap().getOrElse(() -> null);
        assertNotNull(diag);
        assertTrue(diag.message().contains("Expected identifier"));
    }

    @Test
    void rejects_invalid_pattern_starting_underscore(@TempDir Path dir) throws Exception {
        SamTokenizer tz = tokenizerFor(dir, "_bad");
        Either<Diag, String> result = CompilerUtils.getIdentifierE(tz, CompilerUtils.noRecorder());
        assertTrue(result.isLeft());
        Diag diag = result.swap().getOrElse(() -> null);
        assertNotNull(diag);
        // Depending on tokenizer, '_' may not be a WORD token. Accept either message.
        String msg = diag.message();
        assertTrue(msg.contains("Invalid identifier") || msg.contains("Expected identifier"), msg);
    }
}
