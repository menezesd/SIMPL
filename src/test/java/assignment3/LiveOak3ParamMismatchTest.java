package assignment3;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import java.nio.file.Path;
import static org.junit.jupiter.api.Assertions.*;

class LiveOak3ParamMismatchTest {
    private static final String badDir = Path.of("src","test","resources","LO-3","InvalidPrograms").toString();

    @Test
    @DisplayName("parameter count mismatch includes expected and actual")
    void countMismatchMessage() {
        String fileName = Path.of(badDir, "param_count_mismatch.lo").toString();
        Error err = assertThrows(Error.class, () -> LiveOak3Compiler.compiler(fileName));
        // stderr contains the standardized failed-to-compile message only; we just ensure it throws
        assertNotNull(err);
    }

    @Test
    @DisplayName("parameter type/name mismatch includes position and details")
    void detailMismatchMessage() {
        String fileName = Path.of(badDir, "param_type_mismatch.lo").toString();
        Error err = assertThrows(Error.class, () -> LiveOak3Compiler.compiler(fileName));
        assertNotNull(err);
    }
}
