package assignment2;

import assignment2.BinopType;
import assignment2.CompilerException;
import assignment2.OperatorUtils;
import assignment2.SyntaxErrorException;

public final class StringRuntime {
    private StringRuntime() {}

    // Shared label for the string-length runtime routine (emitted once)
    private static final String LENGTH_LABEL = "STR_LENGTH";
    // Shared label for reverse-string routine
    private static final String REVERSE_LABEL = "STR_REVERSE";
    // Shared label for concat-string routine
    private static final String CONCAT_LABEL = "STR_CONCAT";
    // Shared label for append-string-heap routine
    private static final String APPEND_LABEL = "STR_APPEND";
    // Shared label for compare-string routine
    private static final String COMPARE_LABEL = "STR_COMPARE";
    // Shared label for repeat-string routine
    private static final String REPEAT_LABEL = "STR_REPEAT";

    public static String repeatString(Type firstInputType, Type secondInputType) {
        SamBuilder sb = new SamBuilder();
        if (firstInputType == Type.STRING) {
            sb.append("SWAP\n");
        }
        sb.append("LINK\n");
        sb.append("JSR " + REPEAT_LABEL + "\n");
        sb.append("UNLINK\n");
        sb.append("ADDSP -1\n");
        return sb.toString();
    }

    public static String getStringLength() {
        SamBuilder sb = new SamBuilder();
        sb.append("JSR " + LENGTH_LABEL + "\n");
        return sb.toString();
    }

    /**
     * Emit all shared string helper functions that should only appear once in the
     * generated program. Currently this emits the STR_LENGTH subroutine.
     */
    public static String emitAllStringFunctions() {
        SamBuilder sb = new SamBuilder();

        // STR_LENGTH
        Label startCountLabel = new Label();
        Label stopCountLabel = new Label();
        sb.label(LENGTH_LABEL);
        sb.append("SWAP\nDUP\n");
        sb.label(startCountLabel.getName());
        sb.append("DUP\nPUSHIND\n");
        sb.append("ISNIL\n");
        sb.append("JUMPC " + stopCountLabel.getName() + "\n");
        sb.append("PUSHIMM 1\nADD\n");
        sb.append("JUMP " + startCountLabel.getName() + "\n");
        sb.label(stopCountLabel.getName());
        sb.append("SWAP\nSUB\nSWAP\nRST\n");

        // STR_REVERSE
        Label reverseStartLoopLabel = new Label();
        Label reverseStopLoopLabel = new Label();
        sb.label(REVERSE_LABEL);
        sb.append("PUSHIMM 0\nPUSHIMM 0\nPUSHIMM 0\n");
        sb.append("PUSHOFF -1\n");
        sb.append(getStringLength());
        sb.append("STOREOFF 2\n");
        sb.append("PUSHOFF 2\nPUSHIMM 1\nADD\nMALLOC\nSTOREOFF 3\n");
        sb.append("PUSHOFF 3\nSTOREOFF 4\n");
        sb.append("PUSHOFF 3\nPUSHOFF 2\nADD\nPUSHIMMCH '\0'\nSTOREIND\n");
        sb.label(reverseStartLoopLabel.getName());
        sb.append("PUSHOFF 2\nISNIL\nJUMPC " + reverseStopLoopLabel.getName() + "\n");
        sb.append("PUSHOFF 3\nPUSHOFF -1\nPUSHOFF 2\nADD\nPUSHIMM 1\nSUB\nPUSHIND\nSTOREIND\n");
        sb.append("PUSHOFF 3\nPUSHIMM 1\nADD\nSTOREOFF 3\n");
        sb.append("PUSHOFF 2\nPUSHIMM 1\nSUB\nSTOREOFF 2\n");
        sb.append("JUMP " + reverseStartLoopLabel.getName() + "\n");
        sb.label(reverseStopLoopLabel.getName());
        sb.append("PUSHOFF 4\nSTOREOFF -1\nADDSP -3\nRST\n");

        // STR_CONCAT
        sb.label(CONCAT_LABEL);
        sb.append("PUSHIMM 0\nPUSHIMM 0\n");
        sb.append("PUSHOFF -1\n");
        sb.append(getStringLength());
        sb.append("PUSHOFF -2\n");
        sb.append(getStringLength());
        sb.append("ADD\nPUSHIMM 1\nADD\nMALLOC\nSTOREOFF 2\n");
        sb.append("PUSHOFF 2\nSTOREOFF 3\n");
        sb.append("PUSHIMM 0\nPUSHOFF 2\nPUSHOFF -2\n");
        sb.append(appendStringHeap());
        sb.append("STOREOFF 2\n");
        sb.append("PUSHIMM 0\nPUSHOFF 2\nPUSHOFF -1\n");
        sb.append(appendStringHeap());
        sb.append("STOREOFF 2\n");
        sb.append("PUSHOFF 3\nSTOREOFF -2\nADDSP -2\nRST\n");

        // STR_APPEND
        Label appendStartLoopLabel = new Label();
        Label appendStopLoopLabel = new Label();
        sb.label(APPEND_LABEL);
        sb.append("PUSHOFF -2\nPUSHOFF -1\n");
        sb.label(appendStartLoopLabel.getName());
        sb.append("PUSHOFF 3\nPUSHIND\nISNIL\nJUMPC " + appendStopLoopLabel.getName() + "\n");
        sb.append("PUSHOFF 2\nPUSHOFF 3\nPUSHIND\nSTOREIND\n");
        sb.append("PUSHOFF 2\nPUSHIMM 1\nADD\nSTOREOFF 2\n");
        sb.append("PUSHOFF 3\nPUSHIMM 1\nADD\nSTOREOFF 3\n");
        sb.append("JUMP " + appendStartLoopLabel.getName() + "\n");
        sb.label(appendStopLoopLabel.getName());
        sb.append("PUSHOFF 2\nPUSHIMMCH '\0'\nSTOREIND\nPUSHOFF 2\nSTOREOFF -3\nADDSP -2\nRST\n");

        // STR_REPEAT
        Label repeatStartLoopLabel = new Label();
        Label repeatStopLoopLabel = new Label();
        Label repeatInvalidParamLabel = new Label();
        Label repeatReturnLabel = new Label();
        sb.label(REPEAT_LABEL);
        sb.append("PUSHIMM 0\nPUSHIMM 0\nPUSHIMM 0\n");
        sb.append("PUSHOFF -2\nISNEG\nJUMPC " + repeatInvalidParamLabel.getName() + "\n");
        sb.append("PUSHOFF -1\n");
        sb.append(getStringLength());
        sb.append("PUSHOFF -2\nTIMES\nPUSHIMM 1\nADD\nMALLOC\nSTOREOFF 3\n");
        sb.append("PUSHOFF 3\nSTOREOFF 4\n");
        sb.label(repeatStartLoopLabel.getName());
        sb.append("PUSHOFF 2\nPUSHOFF -2\nEQUAL\nJUMPC " + repeatStopLoopLabel.getName() + "\n");
        sb.append("PUSHIMM 0\nPUSHOFF 3\nPUSHOFF -1\n");
        sb.append(appendStringHeap());
        sb.append("STOREOFF 3\n");
        sb.append("PUSHOFF 2\nPUSHIMM 1\nADD\nSTOREOFF 2\n");
        sb.append("JUMP " + repeatStartLoopLabel.getName() + "\n");
        sb.label(repeatStopLoopLabel.getName());
        sb.append("PUSHOFF 4\nSTOREOFF -2\nJUMP " + repeatReturnLabel.getName() + "\n");
        sb.label(repeatInvalidParamLabel.getName());
        sb.append("PUSHIMMSTR \"\"\nSTOREOFF -2\n");
        sb.label(repeatReturnLabel.getName());
        sb.append("ADDSP -3\nRST\n");

        // STR_COMPARE
        Label cmpStartLoopLabel = new Label();
        Label cmpStopLoopLabel = new Label();
        sb.label(COMPARE_LABEL);
        sb.append("PUSHIMM 0\nPUSHIMM 0\n");
        sb.label(cmpStartLoopLabel.getName());
        sb.append("PUSHOFF -2\nPUSHOFF 2\nADD\nPUSHIND\nISNIL\n");
        sb.append("PUSHOFF -1\nPUSHOFF 2\nADD\nPUSHIND\nISNIL\nAND\n");
        sb.append("JUMPC " + cmpStopLoopLabel.getName() + "\n");
        sb.append("PUSHOFF -2\nPUSHOFF 2\nADD\nPUSHIND\n");
        sb.append("PUSHOFF -1\nPUSHOFF 2\nADD\nPUSHIND\nCMP\nSTOREOFF 3\n");
        sb.append("PUSHOFF 3\nJUMPC " + cmpStopLoopLabel.getName() + "\n");
        sb.append("PUSHOFF 2\nPUSHIMM 1\nADD\nSTOREOFF 2\n");
        sb.append("JUMP " + cmpStartLoopLabel.getName() + "\n");
        sb.label(cmpStopLoopLabel.getName());
        sb.append("PUSHOFF 3\nSTOREOFF -2\nADDSP -2\nRST\n");

        return sb.toString();
    }

    /**
     * reverseString helper originally lived in LiveOak0Compiler.
     * It expects one string on the stack and returns a new reversed string.
     */
    public static String reverseString() {
        SamBuilder sb = new SamBuilder();
        sb.append("LINK\nJSR " + REVERSE_LABEL + "\nUNLINK\n");
        return sb.toString();
    }

    public static String appendStringHeap() {
        SamBuilder sb = new SamBuilder();
        sb.append("LINK\nJSR " + APPEND_LABEL + "\nUNLINK\nADDSP -2\n");
        return sb.toString();
    }

    public static String concatString() {
        SamBuilder sb = new SamBuilder();
        sb.append("LINK\nJSR " + CONCAT_LABEL + "\nUNLINK\nADDSP -1\n");
        return sb.toString();
    }

    public static String compareString(char op) throws CompilerException {
        if (OperatorUtils.getBinopType(op) != BinopType.COMPARISON) {
            throw new SyntaxErrorException(
                "compareString receive invalid operation: " + op,
                -1
            );
        }
        SamBuilder sb = new SamBuilder();
        sb.append("LINK\nJSR " + COMPARE_LABEL + "\nUNLINK\nADDSP -1\n");
        if (op == '<') {
            sb.append("PUSHIMM 1\n");
        } else if (op == '>') {
            sb.append("PUSHIMM -1\n");
        } else {
            sb.append("PUSHIMM 0\n");
        }
        sb.append("EQUAL\n");
        return sb.toString();
    }
}
