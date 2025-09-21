package assignment2;

/**
 * Exception thrown to indicate a type error at a specific line in the source code.
 */
public class TypeErrorException extends CompilerException {

    /**
     * Constructs a new TypeErrorException with the specified detail message and line number.
     *
     * @param message the detail message
     * @param line    the line number where the type error occurred
     */
    public TypeErrorException(String message, int line) {
        super(message, line);
    }

    /**
     * Returns a string representation of the type error exception.
     *
     * @return a string describing the type error and its line number
     */
    @Override
    public String toString() {
        return "Type error at line " + getLine() + ": " + getMessage();
    }
}
