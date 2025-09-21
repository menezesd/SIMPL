package assignment2;

/**
 * Exception thrown by the compiler to indicate an error at a specific line.
 */
public class CompilerException extends Exception {
    /** The line number where the error occurred. */
    private final int line;

    /**
     * Constructs a new CompilerException with the specified detail message and line number.
     *
     * @param message the detail message
     * @param line    the line number where the error occurred
     */
    public CompilerException(String message, int line) {
        super(message);
        this.line = line;
    }

    /**
     * Returns the line number where the error occurred.
     *
     * @return the line number
     */
    public int getLine() {
        return line;
    }

    /**
     * Returns a string representation of the exception.
     *
     * @return a string describing the error and its line number
     */
    @Override
    public String toString() {
        return "Error at line " + line + ": " + getMessage();
    }
}
