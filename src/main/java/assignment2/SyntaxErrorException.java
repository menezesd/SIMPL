package assignment2;

/**
 * Exception thrown to indicate a syntax error at a specific line in the source code.
 */
public class SyntaxErrorException extends CompilerException {

    /**
     * Constructs a new SyntaxErrorException with the specified detail message and line number.
     *
     * @param message the detail message
     * @param line    the line number where the syntax error occurred
     */
    public SyntaxErrorException(String message, int line) {
        super(message, line);
    }

    /**
     * Returns a string representation of the syntax error exception.
     *
     * @return a string describing the syntax error and its line number
     */
    @Override
    public String toString() {
        return "Syntax error at line " + getLine() + ": " + getMessage();
    }
}
